library(tidyverse)
library(csodata)
library(sf)
library(lubridate)
library(zoo)
library(ISOweek)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
options(warn=1)

##########################
# paste names
########################

paste_names <- function(vector){vector %>% unique %>% sort %>% paste(collapse = '", \n"') %>% cat}

############################################
# county names
###########################################

county_list <- list(Galway=c("Galway","Galway City","Galway County"),
                  Limerick=c("Limerick","Limerick City","Limerick County"),
                  Cork=c("Cork","Cork City","Cork County"), 
                  Waterford=c("Waterford","Waterford City","Waterford County"), 
                  Tipperary=c("Tipperary","North Tipperary","South Tipperary"), 
                  Dublin=c("Dublin","Dublin City","Dun Laoghaire-Rathdown","Fingal","South Dublin"), 
                  Other=c("Carlow","Cavan","Clare","Donegal","Kerry","Kildare","Kilkenny","Laois", 
                          "Leitrim","Longford","Louth","Mayo","Meath","Monaghan","Offaly","Roscommon", 
                          "Sligo","Westmeath","Wexford","Wicklow"))


###########################
# Load CSO Table as Tibble
##########################

get_data <- function(id_string){
              data <- cso_get_data(id_string,
                                   wide_format = "tall",
                                   use_factors = FALSE) %>% tibble
              return(data)
              }

#########################################################################
# Identify date, id, value, and other variables in CSO County Data Table
#########################################################################

get_cols <- function(dataset){
  
  res <- list(date=NA,id=NA, value = NA, other_vars=NA)                  
  res[[1]] <- names(dataset)[grepl("date|year(?!\\.of\\.|s\\.and\\.Over|\\.of\\.taking|\\.ago|\\.previous)|quarter|month|week", names(dataset), ignore.case = TRUE,perl = TRUE)] 
  res[[2]] <- names(dataset)[grepl("county", names(dataset), ignore.case = TRUE,perl = TRUE)] 
  res[[3]] <- names(dataset)[grepl("value", names(dataset), ignore.case = TRUE)] 
  
  lapply(head(res,3),function(x) if(any(is.na(x))) warning("No column found for some criteria. Assign manually."))
  lapply(res[c(1,3)],function(x) if(length(x)>1) warning("More than one column found for date or value criteria. Check."))
  if(length(res[[2]])>1) print ("More than one county column identified. Check and consider network")
  
  res[[1]] <- res[[1]] %>% head(1)  
  res[[2]] <- res[[2]] %>% head(1)  
  res[[3]] <- res[[3]] %>% head(1)  
  
  stat_set <- dataset %>% select (-c(!!res[[1]],!!res[[2]],!!res[[3]]))
  res[[4]] <- sapply (stat_set, function(x) unique(x) %>% length) %>% sort %>% names
  
  return(res)
}

##############################
# clean data and pivot wider
#############################

reshape_data <- function(dataset, vars_list){
  data <- dataset %>% rename(date=vars_list[[1]], id=vars_list[[2]], value=vars_list[[3]])
  
  na_string <- "^\\.$|^-$|#VALUE!"
  data <- data %>% mutate(value = gsub(na_string,NA, value)%>% as.numeric)
  
  data <- data %>% mutate(date=if (grepl("year",cols[[1]], ignore.case = TRUE)){
    date %>%  strptime("%Y")%>% floor_date("year")} else
      if (grepl("quarter",cols[[1]], ignore.case = TRUE)){
        date %>% as.yearqtr %>%  as.Date %>% strptime("%Y-%m-%d")} else
          if (grepl("month",cols[[1]], ignore.case = TRUE)){
            if (any(grepl("\\d[Mm]\\d",date))){
              date %>% gsub("[Mm]","",.) %>% paste("1", sep="") %>% as.Date("%Y%m%d")%>% strptime("%Y-%m-%d")}} else
                if (grepl("week",cols[[1]], ignore.case = TRUE)){
                  if (any(grepl("\\d[Ww]\\d",date))){
                    date %>% gsub("W","-W",.)%>% paste("-1", sep="") %>% ISOweek2date %>% strptime("%Y-%m-%d")} else
                  if (any(grepl("\\s\\w{3,}\\s",date))){date%>% ymd() %>% strptime("%Y-%m-%d")}}else
                      date%>% ymd() %>% strptime("%Y-%m-%d"))

  data <- data %>% mutate_at(vars(cols[[4]]), ~ str_replace_all(., " |\\(|\\)", "_"))
  
  data <- data %>% pivot_wider(names_from = cols[[4]], values_from = value, names_sep="_")
  
  return(data)
}

########################################
# standardise counties column
#######################################

clean_counties <- function(data, drop_state=TRUE){
                    data$id[grep("Laoghire|Rathdown", data$id, ignore.case = TRUE)] <- "Dun Laoghaire-Rathdown" # Fix Dun Laoghaire
                    data$id[grep("Laoghais|Laoighis", data$id, ignore.case = TRUE)] <- "Laois"                  # Fix Laoise
                    data$id[grep("Tipperary NR", data$id, ignore.case = TRUE)] <- "North Tipperary"             # Fix North Tipp
                    data$id[grep("Tipperary SR", data$id, ignore.case = TRUE)] <- "South Tipperary"             # Fix South Tipp
                    data$id[grep("all counties", data$id, ignore.case = TRUE)] <- "State"                       # Fix South Tipp
                    data$id[grep("fingal", data$id, ignore.case = TRUE)] <- "Fingal"                            # Fix Fingal
                    
                    data$id <- gsub(" City and County|Co. |,", "",data$id, ignore.case =TRUE)                   # Fix City and ","
                    
                    drop_strings <- paste("connacht|munster|leinster|ulster",
                                          "Border|Mid-East|Mid-West|Midland|Non Attributable",
                                          "Place of work|Outside the State|Mid East|Elsewhere",
                                          "Total|unknown|Ireland|Northern and Western|Southern",
                                          "^Down$|South-East|^Derry$|^Antrim$|^Armagh$|Non-residents",
                                          "Dublin\\s\\d|Borough|South-West|^West$|Tyrone|Fermanagh",
                                          sep="|")                                                              
                    dropped_data <- data %>% filter(grepl(drop_strings,id, ignore.case = TRUE)) %>% pull(id) %>% unique %>% sort
                    cat("Dropped:\n",dropped_data,"\n")
                    data <- data %>% filter(!grepl(drop_strings,id, ignore.case = TRUE))                            # Drop others 
                    
                    if (drop_state) data <- data %>% filter(!id=="State")                                       # Drop State
                    
                    lapply(1:(length(county_list)-1), function (x) if(all(match(county_list[[x]], data$id) %>% is.na)){warning(paste("No data for ", names(county_list)[x]))})
                    
                    if (length(setdiff(data$id,unname(unlist(county_list))))>0){cat("Unexpected names:\n", setdiff(data$id,unname(unlist(county_list))),"\n")}
                    return(data)
                    }


###############################################################
# Add population to formatted dataset
#####################################################
add_pop <<- function(dataset, demographic="total_pop"){
                    load("C:\\Users\\glynn\\OneDrive\\Data Analytics\\00 Projects\\Population\\MASTER_POPS_CNA23.rda")
                    if(any(!dataset$id %in% unlist(county_list))) warning("Some locations in data not matched")
                    
                    dict <- c(total_pop="Population_Both_sexes_All_marital_status",
                              total_pop_male="Population_Male_All_marital_status", 
                              total_pop_female="Population_Female_All_marital_status")
                    demo <- dict[match(demographic,names(dict))]
                    
                    pops <- MASTER_POPS_CNA23 %>% mutate(joindate =  format(as.Date(date), "%Y")) %>% 
                      select(joindate, id,!!demo)
                    dataset <- dataset %>% mutate(joindate =  format(as.Date(date), "%Y"))
                    
                    dataset <- left_join(dataset, pops, by=c("joindate", "id"), suffix=c("","")) %>%
                      select(-joindate) %>% 
                      relocate(all_of(c("date", "id",demographic)))
                    
                    if (any(dataset[,demographic] %>% is.na))warning("Population not available for some regions")
                    return(dataset)
                  }


##################
# Print variables
#################

print_vars <- function(dataset){
  vars <- dataset %>% select(-date, -id) %>% names 
  vars <- data.frame(vars = vars, index=1:length(vars)) %>% tibble
  return(vars)
  }

##################
# select variables
###################

select_vars <- function(dataset, var_index){
                dataset %>% select(date, id, var_index+2) 
                }

##########
# check variables
#############

check_vars <- function (dataset){
                  for (c in 1: length(county_list)){
                    dataset %>% filter(id %in% (county_list[[c]])) %>% View(paste(names(county_list)[c]))
                  }}


###########################################################
# Check whether counties need to be merged or dropped
#############################################################

#dataset <- data

check_merge <- function(dataset, mode="auto"){
                  n_col <- ncol(dataset)
                  n_stats <- n_col-2
                  city_list <- head(county_list,-1)
                  counties <- city_list %>% names
                  n_counties <- city_list %>% length
                  periods <- dataset$date %>% unique %>% sort
                  n_periods <- periods %>% length
                  n_row <- n_periods*n_counties
                  merge_mat <- data.frame(matrix(NA,n_row,n_col)) %>% tibble
                  names(merge_mat) <- names(dataset)
                  merge_mat$date <- rep(periods, times=n_counties)
                  merge_mat$id <- rep(counties, each=n_periods)
                  
                  for (s in 1:n_stats){
                    #s <-1
                    #print(paste("s",s))
                    stat_data <- dataset %>% select(c(1,2,2+s))
                    
                    for (c in 1:n_counties){
                      #c<-1
                      #print(paste("s",s,"c",c))
                      county_comps <- city_list[[c]]
                      county_stat_data <- stat_data %>% filter(id %in% county_comps)
                      county_stat_data <- county_stat_data[!county_stat_data[,3] %>% is.na,] 
                      n_divs <- length(county_comps[-1])
                      
                      for (p in 1:n_periods){
                        #p<-1
                        #print(paste("s",s,"c",c,"p", p))
                        period_index <- p+(n_periods*(c-1))
                        stat_index <- 2+s
                        period <-periods[p] 
                        period_county_stat_data <- county_stat_data %>% filter(date==period)
                        period_county_stat_data_total <- period_county_stat_data %>% filter(id==county_comps[1])
                        period_county_stat_data_divided <- period_county_stat_data %>% filter(id %in% county_comps[-1])
                        
                        if (nrow(period_county_stat_data)==0){
                        if (mode=="county"){merge_mat[period_index,stat_index] <- 10} else      # 10 keep all, rename and sum, merge map 
                        merge_mat[period_index,stat_index] <- 0} else                           # 0  keep divs, drop county,leave map 
                            if(nrow(period_county_stat_data_total)==1 & nrow(period_county_stat_data_divided)<n_divs){
                            if (mode=="county"){merge_mat[period_index,stat_index] <- 11} else  # 11 drop divs, keep county, merge map
                            merge_mat[period_index,stat_index] <- 1} else                       # 1 drop divs, keep county, merge map
                                if (nrow(period_county_stat_data_total)==1 & nrow(period_county_stat_data_divided)==n_divs){
                                if (mode=="county"){merge_mat[period_index,stat_index] <- 12} else  # 12 drop divs, keep county, merge map
                                merge_mat[period_index,stat_index] <- 2} else                       # 2 keep divs, drop county, leave map
                                  if (nrow(period_county_stat_data_total)==0 & nrow(period_county_stat_data_divided)==n_divs){
                                  if (mode=="county"){merge_mat[period_index,stat_index] <- 13} else # 13 keep divs, drop county, rename and sum, merge map
                                  merge_mat[period_index,stat_index] <- 2}  else          # 2 keep divs, drop county, leave map unchanged
                                    if (nrow(period_county_stat_data_total)==0 & nrow(period_county_stat_data_divided)<n_divs){
                                    if (mode=="county"){merge_mat[period_index,stat_index] <- 14} else  # 14 keep divs, drop county, rename and sum, merge map
                                    merge_mat[period_index,stat_index] <- 3}            # 3 keep divs, drop county,leave map 
                      } # end periods
                    } # end counties
                  } # end stats
                  
                  merge_mat <- merge_mat %>%  mutate(merge_vec =  do.call(pmax,.[,names(.) %>% tail(-2)]))
                  return (merge_mat)
                } # end function 

##################################
# Merge or drop counties from dataset
#######################################

merge_data <- function(dataset, merge_matrix){
                    fix_ids <- function(vector, old_name, new_name){
                      if (length(new_name)==1){
                        dict <- rep(new_name, length(old_name))
                      } else {
                        dict <- new_name
                      }
                      names(dict) <- old_name    
                      recode(vector, !!!dict)
                    }
                    periods <- dataset$date %>% unique
                    merged_data <- data.frame(matrix(NA, ncol = ncol(dataset), nrow = 0))
                    names(merged_data) <- names(dataset)
                    
                    for (p in 1:length(periods)){
                      #p<-3
                      #print(paste("p",p))
                      period_data <- dataset %>% filter(date==periods[p])
                      period_merge_mat <- merge_matrix %>% filter(date==periods[p])
                      period_merge_vec <- period_merge_mat$merge_vec 
                      names(period_merge_vec) <- period_merge_mat$id
                      
                      for (c in 1:length(period_merge_vec)){
                      #c <- 5
                      #print(paste("p",p,"c",c))  
                      county <- county_list[[c]]    
                      if (period_merge_vec[c] %in% c(0,2,3)){
                          period_data <- period_data %>% filter(!id == county_list[[c]][1])} else  
                      if (period_merge_vec[c] %in% c(1,11,12)){
                          period_data <- period_data %>% filter(!id %in% (county_list[[c]] %>% tail(-1)))} else
                      if (period_merge_vec[c]==10){
                          old_names <- county_list[[c]] %>% tail(-1)
                          new_names <- county_list[[c]][1]
                          period_data$id <- fix_ids(period_data$id,old_names,new_names)} else
                      if (period_merge_vec[c] %in% c(13,14)){
                          period_data <- period_data %>% filter(!id == county_list[[c]][1])
                          old_names <- county_list[[c]] %>% tail(-1)
                          new_names <- county_list[[c]][1]
                          period_data$id <- fix_ids(period_data$id,old_names,new_names)}
                        }
                      merged_data <- rbind(merged_data, period_data)
                    }
                    merged_data <- merged_data %>% group_by(date, id) %>% summarise_all (sum, na.rm=TRUE) %>% data.frame %>% tibble
                    return(merged_data)
                  }

###############################################
# Add map to data
###############################################


add_map <- function(dataset, merge_mat, quality="medium"){
                  
                  qual <- c(high=1, medium=500, low=1000)
  
                  load("C:\\Users\\glynn\\OneDrive\\Data Analytics\\00 Projects\\[00] Functions and Data\\maps\\ireland_map_comps.rda")
                  
                  periods <- dataset$date %>% unique
                  res <- vector("list", length=length(periods))
                  for (p in 1:length(periods)){
                    
                    period_merge_vec <- merge_mat %>% filter(date==periods[p]) %>% pull(merge_vec)
                    
                    period_res <- vector("list",length=8)
                    
                    for (c in 1:6){
                         period_res[[c]]<-if(period_merge_vec[c]%in% c(1,10,11,12,13,14)){map_comps[[c]][[1]]}else{map_comps[[c]][[2]]} 
                        }
                    period_res[[7]]<-map_comps[[7]][[1]]
                    period_res[[8]]<-map_comps[[8]][[1]]
                    
                    period_map <- do.call("rbind",period_res)
                    
                    res[[p]] <- period_map %>% mutate(date=rep(periods[p], nrow(period_map)))
                    
                    } # end periods
                  map <- do.call("rbind", res)
                  map <- st_simplify(map, preserveTopology = FALSE, dTolerance = qual[match(quality,names(qual))])
                  
                  map <- left_join(map, dataset, by=c("date","id"))
                  return(map)
                }
          
##############################################
# load data
##############################################

# load("C:\\Users\\glynn\\OneDrive\\Data Analytics\\00 Projects\\CSO Locations\\COUNTY_CATS.rda")
# 
# load("C:\\Users\\glynn\\OneDrive\\Data Analytics\\00 Projects\\TOC\\CSO_FILES_MASTER.rda")
# 
# test_cases <- MASTER_FILES[match(lapply(county_cats[[2]], head,1), names(MASTER_FILES))]
# 
# all_cases <- MASTER_FILES[match(unlist(county_cats[[2]]), names(MASTER_FILES))]


# ls() %>% paste(collapse = '","') %>% cat

# rm(list=ls()[! ls() %in% c("all_cases","check_merge","clean_counties","county_cats",
#                            "county_list","get_data","MASTER_FILES","test_cases")])


# "CNA13" is census data

# (data <- raw_data <- get_data("VSB08"))
# (cols <- get_cols(data))
# (data <- reshape_data(data,cols))
# (data <- clean_counties(data))
# (merge_matrix <- check_merge(data))








