
library(tidyverse)
library(gganimate)
library(viridis)
library(magick)
library(sf)
library(elementalist)
library(png)

?days

# rm(list=ls()[! ls() %in% c("map_data")])

animate_ireland <- function(map_data, plot_title, legend_title,
                            top_left = "",top_right = "",
                            bot_right = "", bot_left="",
                            scale,
                            viridis_option, 
                            viridis_direction,
                            duration,
                            frames_per_period=1,
                            save_date=TRUE,
                            test=FALSE,
                            file_desc=""){
  
    names(map_data)[3] <- "value"
    
    if (test){gif_background <- "black"; plot_background <- "grey"}else{
    plot_background <- panel_background <- gif_background <- background <- '#e4eef7'}
    
    # page setup
    page_width <- 1000
    page_height <- 1000
    header_prop <- .1
    footer_prop <- .1
    
    # text sizes                  
    title_size <- 1.4
    annotation_size <- 5 
    legend_bar_length <- 185
    
    # Adjust map aspect ratio to fit page layout
    header <- header_prop * page_height
    footer <- footer_prop * page_height
    
    map_coords <- st_bbox(map_data)               
    x_lims <- map_coords[c(1,3)] 
    y_lims <- map_coords[c(2,4)]
    x_length <- dist(x_lims)
    y_length <- dist(y_lims)
    desired_ar <- page_width / (page_height - header - footer)
    new_x_length <- y_length * desired_ar
    x_adjustment <- (new_x_length - x_length) /2
    new_x_lims <- c(x_lims[1]-x_adjustment,
                    x_lims[2]+x_adjustment) 
    
    # base map
    plot_map <- ggplot(map_data) +
                      geom_sf(aes(fill = value))+
                      coord_sf(xlim=new_x_lims,
                               ylim=y_lims)+
                      theme_void()+
                      theme(plot.background=element_rect(fill=plot_background, 
                                                         colour=plot_background),
                            panel.background=element_rect(fill=panel_background, 
                                                          colour=panel_background))
    # adjust legend and title
    plot_map <- plot_map+ 
                    labs(title = plot_title) +
                    theme(legend.title=element_text(size=title_size*18),
                          legend.text=element_text(size=title_size*15),
                          legend.position = "top",
                          legend.justification = c(0,0),
                          legend.direction = 'horizontal',
                          legend.key.width = unit(legend_bar_length, "pt"),
                          plot.title = element_text(size = title_size*26,
                                                     vjust = 1))+
                    guides(fill = guide_colourbar(title=legend_title,
                                                  title.position = "top",
                                                  label.position = "bottom",
                                                  title.hjust = 0,
                                                  title.vjust =0))
    # add annotation
    annotations <- data.frame(xpos = c(-Inf, Inf,Inf,-Inf),
                              ypos =  c(Inf, Inf,-Inf,-Inf),
                              annotateText = c(top_left,top_right,
                                               bot_right,bot_left),
                              hjustvar = c(0,1,1,0) ,
                              vjustvar = c(1,1,0,-0.15)) 
    plot_map <- plot_map +
                  geom_text(data=annotations,
                            aes(x=xpos,
                                y=ypos,
                                hjust=hjustvar,
                                vjust=vjustvar,
                                label=annotateText),
                            size=annotation_size)
    # change colour scale
    if (scale=="viridis"){plot_map <- plot_map + 
                                        scale_fill_viridis(option=viridis_option, 
                                                           direction=viridis_direction,
                                                           na.value=plot_background)}
    # add animation
    plot_map <- plot_map + 
                    transition_time(date)
    
    
    # configure animation
    n_periods <- map_data$date %>% unique %>% length
    
    n_frames <- ifelse ((var(map_data$id %>% table)==0),frames_per_period*n_periods,n_periods)
    fps <- n_frames/duration
    
    # create gif
    gif <- animate(plot = plot_map,
                    nframes = n_frames,
                    width = page_width,
                    height= page_height-footer,
                    fps=fps,                                           
                    end_pause = 0,
                    bg=gif_background)
    

    gif1 <- image_read(gif)
    
    multip <- round(5/fps)
    
    gif1 <- gif1[rep(1:(length(gif1)), each=multip)]
    
  
  #############################################################################
  # create timeline
  #############################################################################
   
    # set proportions  
    prop_tl <- .3
    ar_tl <- (prop_tl * footer)/page_width
    ar_tl_panel <- footer/page_width
    bot_mar_tl <- footer*(1-prop_tl)
    side_mar_tl <- .125 * page_width
    
    # get date range
    dates <- map_data$date %>% unique
    range <- difftime(max(dates),min(dates),"days") %>% as.numeric
    breaks <- c(0,10,20,30,40,50,60,70,140,210,366,731,3660,9132,13000,20000,Inf)
    labels <- c("1 day","2 day","3 day","4 day","5 day","6 day",
                "1 week","2 week","3 week","1 month","3 month",
                "1 year","2 year","3 year","5 year","10 year")
    
    time_line_break <-cut(range,breaks, labels) %>% as.character
    time_line_labels <-case_when(grepl("year", time_line_break)~"%Y",
                                 grepl("month", time_line_break)~"%m%y",
                                 grepl("week|day", time_line_break)~"%d%m")
      
    interval <- difftime(dates[length(dates)],dates[length(dates)-1])
    dates <- c(dates,(dates %>% tail(1))+days(interval-1))
    dates <- data.frame(date=dates)
    dates <- rbind(dates %>% head(1), dates %>% tail(1))
    
    timeline_lab_size <- 15
    
    plot_timeline <- ggplot(dates, aes(x = date, y = 1)) + 
                        geom_point(size = 7, colour="#1e05ff") + 
                        scale_x_datetime(date_labels = time_line_labels, date_breaks = time_line_break) +
                        theme(aspect.ratio = ar_tl,
                              axis.text.y = element_blank(),
                              axis.text.x = element_text(size = timeline_lab_size), 
                              axis.title = element_blank(),
                              axis.line.y = element_blank(),
                              axis.ticks = element_blank(),
                              axis.line.x = element_blank(),
                              panel.grid = element_blank(),
                              panel.grid.major.x = element_line (size = .5, 
                                                                 linetype = "solid",
                                                                 colour="#c0bed1"),
                              plot.background = element_rect(fill = background, 
                                                             colour = background),
                              plot.margin = margin(.16*bot_mar_tl, 
                                                   .1*side_mar_tl, 
                                                   .5*bot_mar_tl, 
                                                   .1*side_mar_tl, "pt"),
                              panel.background = element_rect_round(fill = "#d5e7f5", 
                                                                    colour = NA,
                                                                    radius = unit(18, "pt")))+
                        transition_time(date)
                      
    
    gif2 <- animate(plot_timeline,
                    nframes = n_frames*multip, 
                    width = page_width,
                    height= footer,
                    fps=fps*multip,                                           
                    end_pause = 0,
                    bg = gif_background)
    
    gif2 <- image_read(gif2)
    
    
  #################################################################################
  # insert logo
  ###################################################################################
  
  twitter_handle <- "@irish_data_viz"
  twitter <- image_read("C:\\Users\\glynn\\OneDrive\\Data Analytics\\00 Projects\\[00] Functions and Data\\twitter.png") %>% 
    image_scale("55")
  text <- image_blank(4*image_info(twitter)[1,2], image_info(twitter)[1,3]) %>% 
    image_annotate(twitter_handle, 
                   size = image_info(twitter)[1,3]/2, 
                   color = "#1DA1F2",
                   location = "+0+10")
  logo <- image_append(c(twitter, text), stack = FALSE) %>% 
    image_trim 
  # calculate offset
  logo_width <- image_info(logo)[2]
  logo_height <- image_info(logo)[3]
  panel_width <- image_info(gif1)[1,2]
  panel_height <- image_info(gif1)[1,3]
  x_pos <- panel_width-(1.2*logo_width)
  y_pos <- panel_height - (1.2*logo_height)
  offset <- paste0("+",x_pos,"+",y_pos)
  
  ################################################################################
  # combine plots
  ###############################################################################
  
  new_gif <- image_append(c(image_composite(gif1[1], logo, offset=offset),
                            gif2[1]), 
                          stack = T)
  for(i in c(2:(length(gif1)),rep(length(gif1),ceiling(length(gif1)*.1)))){
    combined <- image_append(c(image_composite(gif1[i], logo, offset=offset),
                               gif2[i]), 
                             stack = T)
    new_gif <- c(new_gif, combined)}
  
  ###############################################################################
  # save gif
  ###############################################################################

  fps_new <- fps*multip
  
  factors_100 <- c(0.125, 0.25, 0.5, 0.625, 1, 
                   1.25, 2, 2.5, 3.125, 4, 5, 
                   6.25, 10, 12.5, 20, 25, 50, 100)
  
  fps_new <- factors_100[which.min(abs(fps_new-factors_100))]
  
  gif_name <- paste(plot_title,file_desc,if(save_date){format(Sys.time(), " %d-%b-%y %H.%M")}, ".gif", sep=" ")
  
  new_gif%>% image_animate(fps=fps_new) %>%image_write(gif_name)
  
  }



