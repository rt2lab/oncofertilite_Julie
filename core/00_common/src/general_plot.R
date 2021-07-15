
get_plot_final <- function(d_dictionnary,plot_list){

#####################################################################################
################################ First column #######################################
#####################################################################################

plot_text1 <- ggplot() + 
  annotate(geom="text",x=-1,
           y=0,vjust=-0.5,hjust=0,
           label = paste(plot_list$text1$title,": "),
           color= colors[2],
           family = "Palatino",
           size = font_size_title) + 
  annotate(geom="text",x=-1,
           y=0, vjust = 1,hjust=0,
           label = plot_list$text1$value,
           color= colors[3],
           family = "Palatino",
           size = font_size_title) + 
  theme_void()  +
  xlim(-1,0) + 
  theme(plot.margin= grid::unit(c(0, 0, 0, 0), "cm"),panel.spacing = unit(0,"null"))

if(!is.null(plot_list$text2)){
  plot_text2 <- ggplot() + 
    annotate(geom="text",x=-1,
             y=0,vjust=-0.5,hjust=0,
             label = paste(plot_list$text2$title,": "),
             color= colors[2],
             family = "Palatino",
             size = font_size_title) + 
    annotate(geom="text",x=-1,
             y=0, vjust = 1,hjust=0,
             label = plot_list$text2$value,
             color= colors[3],
             family = "Palatino",
             size = font_size_title) + 
    theme_void()  +
    xlim(-1,0) + 
    theme(plot.margin= grid::unit(c(0, 0, 0, 0), "cm"),panel.spacing = unit(0,"null"))
}else{
  plot_text2 <- NULL
}

if(!is.null(plot_list$multiple_choices)){
  plot_multiple_choices <- ggplot() + 
    annotate(geom="text",
             x=-1,
             y=0.9,
             vjust=0,
             hjust=0,
             label = paste(plot_list$multiple_choices$title,": "),
             color= colors[2],
             family = "Palatino",
             size = font_size_title) +
    annotate(geom="text",
             x=-1,
             y=0.5,
             vjust=0,
             hjust=0,
             label = plot_list$multiple_choices$label1,
             color= colors[2],
             family = "Palatino",
             size = font_size_title) + 
    annotate(geom="text",
             x=-0.50,
             y=0.5,
             vjust=0,
             hjust=0,
             label = plot_list$multiple_choices$value1,
             color= colors[3],
             family = "Palatino",
             size = font_size_title) + 
    annotate(geom="text",
             x=-1,
             y=0.2, 
             vjust=0,
             hjust=0,
             label = plot_list$multiple_choices$label2,
             color= colors[2],
             family = "Palatino",
             size = font_size_title) + 
    annotate(geom="text",
             x=-0.5,
             y=0.2,
             vjust=0,
             hjust=0,
             label = plot_list$multiple_choices$value2,
             color= colors[3],
             family = "Palatino",
             size = font_size_title) + 
    theme_void() +
    xlim(-1,0) +
    ylim(0,1.1) + 
    theme(plot.margin= grid::unit(c(0, 0, 0, 0), "cm"),panel.spacing = unit(0,"null"))
}else{
  plot_multiple_choices <- NULL
}

plot_hist1 = plot_geom_bar(d_dictionnary,
                           plot_list$hist1$col_to_plot,
                           plot_list$hist1$col_to_mean,
                           plot_list$hist1$label_x,
                           plot_list$hist1$breaks_x,
                           mean_med = plot_list$hist1$mean_med)

plot_hist2 = plot_geom_bar(d_dictionnary,plot_list$hist2$col_to_plot,plot_list$hist2$col_to_mean,plot_list$hist2$label_x,plot_list$hist2$breaks_x,mean_med = plot_list$hist2$mean_med)

plot_col1 <- plot_grid(plot_text1, NULL, plot_text2, NULL, plot_multiple_choices,NULL, plot_hist1, NULL, plot_hist2,
                  ncol = 1, 
                  align = 'h',
                  rel_heights = c(1,-0.5,1,-0.5,1.3,0,2,0.1,2))

#####################################################################################
####################### Second and thirdcolumn ######################################
#####################################################################################

plot_hist3 = plot_geom_bar(d_dictionnary,plot_list$hist3$col_to_plot,
                           plot_list$hist3$col_to_mean,
                           plot_list$hist3$label_x,
                           plot_list$hist3$breaks_x,
                           plot_list$hist3$mean_med)

plot_binary1 <- plot_binary(d_dictionnary,plot_list$binary1$col_to_plot,plot_list$binary1$value_to_plot,plot_list$binary1$label_x)

d_ttt <- d_dictionnary %>% dplyr::select(one_of(c('dat_first_adj_ct',
                                                  "dat_first_rt",
                                                  "dat_first_ht",
                                                  "dat_first_adj_antiher2",
                                                  'dat_first_surg',
                                                  'dat_first_neo_ct',
                                                  'dat_first_neo_ht',
                                                  'dat_first_neo_rt',
                                                  'dat_first_neo_antiher2' 
                  ))) %>% ncol()
print(d_ttt)

if(d_ttt > 1){
  label_ttt <- ggdraw() + draw_label("Treatment path",x=0,hjust=-0.1,colour=colors[2])
  print(colors_sunburst)
  print("Here")
  legend_plot_ttt <- sunburst_ttt(d_dictionnary,colors_sunburst)
  file_name = file.path(
    Sys.getenv("PROJECT_PATH"),
    "core",
    opt$db_name,
    "docs",
    sprintf("%s_sunburst_treatment.pdf",opt$db_name))
  plot <- magick::image_read_pdf(file_name)
  plot_ttt <- ggdraw(clip="on") + draw_image(plot)
}else{
  label_ttt <- NULL
  plot_ttt <- NULL
  legend_plot_ttt <- NULL
}

plot_hist4 = plot_geom_bar(d_dictionnary,plot_list$hist4$col_to_plot,
                           plot_list$hist4$col_to_mean,
                           plot_list$hist4$label_x,
                           plot_list$hist4$breaks_x,
                           plot_list$hist4$mean_med)

plot_binary2 <- plot_binary(d_dictionnary,plot_list$binary2$col_to_plot,
                        plot_list$binary2$value_to_plot,
                        plot_list$binary2$label_x)

plot_hist5 = plot_geom_bar(d_dictionnary,plot_list$hist5$col_to_plot,
                           plot_list$hist5$col_to_mean,
                           plot_list$hist5$label_x,
                           plot_list$hist5$breaks_x,
                           plot_list$hist5$mean_med)

col2_row1 <- plot_grid(plot_hist3, plot_binary1, ncol=1, align = 'h',rel_heights = c(2,1))

#col2_row1 <- plot_grid(hist_subtype, ncol=1, align = 'h',rel_heights = c(2))

col3_row1 <- plot_grid(plot_binary2 , plot_hist4, ncol=1, align = 'h',rel_heights = c(1,2))

if(plot_list$survival_or_hist$type=="survival"){
  plot_survival(d_dictionnary,
                delay=plot_list$survival_or_hist$delay,
                event=plot_list$survival_or_hist$event,
                fill=plot_list$survival_or_hist$fill,
                colors = colors,
                legend_title = plot_list$survival_or_hist$legend_title,
                legend_labels = plot_list$survival_or_hist$legend_labels)
  
  file_name = file.path(
    Sys.getenv("PROJECT_PATH"),
    "core",
    opt$db_name,
    "docs",
    sprintf("%s_survival.png",opt$db_name))
  
  plot <- magick::image_read(file_name)
  legend_event <- get_levels_labels_name(plot_list$survival_or_hist$event,dictionnary)
  
  plot_survival <- ggdraw(clip="on",ylim = c(0,1.2)) + 
    draw_image(plot) +
    draw_label(legend_event$name,y=1.1,x=0,hjust=-0.1,colour=colors[2])
    
}else{
  plot_survival = plot_geom_bar(d_dictionnary,
                                plot_list$survival_or_hist$col_to_plot,
                                plot_list$survival_or_hist$col_to_mean,
                                plot_list$survival_or_hist$label_x,
                                plot_list$survival_or_hist$breaks_x,
                                plot_list$survival_or_hist$mean_med)
}

col23_row1 <- plot_grid(col2_row1,col3_row1,
                      nrow = 1, 
                      align = 'v',
                      rel_heights = c(1,1))  

col2_row23 <- plot_grid(label_ttt,plot_ttt,legend_plot_ttt,
                             ncol=1,
                             align="h",
                             rel_heights = c(0.5,5,1))

col3_row23 <- plot_grid(plot_hist5,NULL,plot_survival,NULL,
                        ncol=1,
                        align="h",
                        rel_heights = c(1.5,0.05,4,-0.2))

col23_row23 <- plot_grid(col2_row23,col3_row23,
                         nrow = 1,
                         align="v",
                         rel_widths = c(1,1))

plot_col23<-plot_grid(col23_row1, col23_row23,
                 ncol  = 1, 
                 align = 'h',
                 rel_heights = c(2,4))  

#####################################################################################
############################### Whole plot ##########################################
#####################################################################################

plot_title <- paste0("Overview of the dataset ",opt$db_name)
title <- ggdraw() + 
  draw_label(
    plot_title,
    fontface = 'bold',
    x = 0,
    hjust = -1
  ) +
  theme(
    plot.background = element_rect(fill=colors[1])
  ) 

plot_notitle <- plot_grid(plot_col1, plot_col23, nrow=1, align = 'v',rel_widths = c(1,2))
plot_final <- plot_grid(
  title, plot_notitle,
  ncol = 1,
  rel_heights = c(0.1, 1)
)
return(plot_final)
}
