sunburst_ttt <- function(d_dictionnary,colors_plot,n_seq_max_plot = 4, height = 8, width = 8 ){
  
  #Variable name for patient id 
  id = "numdos_curie"
  if(! id %in% colnames(d_dictionnary)){
    id = "cletri"
  }
  
  #Select relevant columns
  d_ttt <- d_dictionnary %>% dplyr::select(one_of(c(id,
                                                    'dat_first_adj_ct',
                                                    "dat_first_rt",
                                                    "dat_first_ht",
                                                    "dat_first_adj_antiher2",
                                                    'dat_first_surg',
                                                    'dat_first_neo_ct',
                                                    'dat_first_neo_ht',
                                                    'dat_first_neo_rt',
                                                    'dat_first_neo_antiher2' 
                                            )))
  
  #Pivot longer database
  texttoparse = paste("d_ttt %>% pivot_longer(-",id,',names_to = "ttt",values_to ="dat_ttt")')
  d_ttt_seq <- eval(parse(text=texttoparse))
  d_ttt_seq <- na.omit(d_ttt_seq)

  #Pretty names for treatment
  d_ttt_seq$ttt <- factor(d_ttt_seq$ttt)
  levels(d_ttt_seq$ttt) <- list("Surgery"=c("dat_first_surg"), 
                                "Radiotherapy"=c("dat_first_rt", "dat_first_neo_rt"),
                                "Hormonotherapy" = c("dat_first_ht", "dat_first_neo_ht"),
                                "Chemotherapy"= c("dat_first_neo_ct","dat_first_adj_ct"),
                                "Targeted therapy" = c("dat_first_neo_antiher2","dat_first_adj_antiher2"))

  #Summarise patients as string : sequence of treatments
  texttoparse = paste("d_ttt_seq %>% group_by(",id,") %>% 
    arrange(dat_ttt) %>% summarise(sequence = paste(ttt,collapse=' - '))")
  d_ttt_seq <- eval(parse(text=texttoparse))
  d_ttt_seq_list <- sapply(d_ttt_seq$sequence,function(x){strsplit(x," - ")}) 
  
  #Maximum number of events per sequences
  n_seq = sapply(1:length(d_ttt_seq_list),function(i) length(d_ttt_seq_list[[i]]))
  n_seq_max = max(n_seq)
  
  #Create database as tree for sunburst
  sunburst0 <- t(as.data.frame(c("root","","","white","",nrow(d_dictionnary))))
  colnames(sunburst0) <- c("ids","labels","labels_count","colors","parents",'values')
  sunburst_i = list()
  colors_sunburst =list("Chemotherapy"=colors_plot[1],"Surgery" =colors_plot[2],
                        "Hormonotherapy"=colors_plot[3],"Radiotherapy"=colors_plot[4],
                        "Targeted therapy"=colors_plot[5])
  
  n_seq_max = min(n_seq_max_plot,n_seq_max)
  for (i in 1:n_seq_max){
    d_ttt_seq_list_i <- lapply(1:length(d_ttt_seq_list), function(j) d_ttt_seq_list[[j]][1:i])
    df <- as.data.frame(matrix(unlist(d_ttt_seq_list_i),nrow = length(d_ttt_seq_list),byrow = T))
    df <- na.omit(df)
    df <- df  %>% group_by_all() %>% summarise(count = n())
    if(i == 1){
      parents =  rep("root",nrow(df))
    }else{
      parents =  apply( df[,1:(ncol(df)-2)] , 1 , paste0 , collapse = "-" )
    }
    ids = apply( df[,1:(ncol(df)-1)] , 1 , paste0 , collapse = "-" )
    labels = df[,paste0("V",i)]
    labels_count = df$count
    colors_mat = matrix(unlist(
      sapply(1:nrow(labels),function(x) colors_sunburst[as.character(labels[x,paste0("V",i)][[1]])]))
    )
    suni = data.frame(
      ids = ids,
      labels = labels,
      labels_count= labels_count,
      colors = colors_mat,
      parents =  parents,
      values = df$count,
      stringsAsFactors = FALSE
    )
    colnames(suni) <- c("ids","labels","labels_count","colors","parents",'values')
    sunburst_i[[i]] = suni
  }
  
  sunburst_i[[n_seq_max+1]] = sunburst0
  sunburst = do.call(rbind,sunburst_i)
  
  #Plot with plotly
  plot_ttt <- plot_ly() %>%
    add_trace(ids=~sunburst$ids, labels = ~sunburst$labels_count, 
              parents = ~sunburst$parents, 
              values=~sunburst$values, 
              marker = list(
                colors = ~sunburst$colors
              ),
              leaf = list(opacity = 1),
              type = 'sunburst',
              branchvalues = 'total') 
  
  #Save plot as pdf
  file_name = file.path(
    "core",
    opt$db_name,
    "docs",
    sprintf("%s_sunburst_treatment.pdf",opt$db_name))
  print(file_name)
  #plotly::orca(plot_ttt, file_name)
  withr::with_dir(Sys.getenv("PROJECT_PATH"), plotly::orca(plot_ttt, file_name))

  #Create legend for plot
  sunburst_nona <- sunburst %>% filter(ids != "root")
  my_hist <- ggplot(sunburst_nona, aes(labels, fill = labels)) + 
    geom_bar() +scale_fill_manual(values = colors_sunburst) + theme(legend.key.size = unit(1, "cm"), 
                                                                    legend.text = element_text(colour = 'black', 
                                                                                               angle = 0, 
                                                                                               size = 8,
                                                                                               face = 'bold'),
                                                                    legend.title = element_blank(),
                                                                    legend.key.width = unit(1, "line")) +
    guides(fill=guide_legend(nrow=2))
  
  #Return legend
  legend <- cowplot::get_legend(my_hist)
  legend_plot_ttt <- as_ggplot(legend)
  return(legend_plot_ttt)
}

plot_geom_bar <- function(d_dictionnary,col_to_plot,col_to_mean,label_x,breaks_x,mean_med=T){
  print("Here")
  print(breaks_x)
  col_to_plot_form = parse(text=col_to_plot)
  d <- d_dictionnary %>% filter(!is.na(eval(col_to_plot_form))) 
  d <- d %>% group_by(eval(col_to_plot_form)) %>% 
    summarise(count=n(),percentage = round(100*n()/nrow(d),0)) %>%
    ungroup()
  d <- d %>% mutate(percentage_label = ifelse(percentage>15 ,paste0(percentage,"%"),""))
  colnames(d) <- c("col","count","percentage","percentage_label")
  d <- d %>% arrange(as.integer(as.character(col)))
  d$col <- factor(d$col)
  if(mean_med){
    median <- round(median(d_dictionnary[[col_to_mean]],na.rm=T))
    mean <- round(mean(d_dictionnary[[col_to_mean]],na.rm=T),1)
    position_median_mean_x <- ifelse(which.max(d$count)<=nrow(d)/2,nrow(d)-1,1)
    position_median_mean_y <- max(mean(c(as.numeric(d[position_median_mean_x+1,"count"]),max(d$count))),
                                  mean(c(as.numeric(d[position_median_mean_x,"count"]),max(d$count))))
    label_mean_med <- paste0("Median : ",median , '\nMean : ' , mean )
  }else{
    label_mean_med = ""
    position_median_mean_x = 0
    position_median_mean_y = 0
  }
  
  hist <- ggplot(data=d, aes(x=factor(0:(nrow(d)-1)), y=count)) +
    geom_bar(stat="identity",fill=colors[5])+
    theme_bw() +
    geom_text(aes(label = count), vjust = -0.5, size = font_size_legend,col = colors[4]) + 
    geom_text(aes(label = percentage_label) ,y=0, vjust=-0.5, size = font_size_legend,col =colors[6]) +
    scale_x_discrete(name   = label_x,
                     breaks = factor(0:(nrow(d)-1)),
                     labels = as.vector(breaks_x[as.vector(d$col)])) + 
    scale_y_continuous(name = '', breaks = NULL, labels = NULL, limits = c(0,1.2*max(d$count)), expand = c(0,0)) + 
    ggtitle(label_x) + 
    annotate(geom="text",
             x = position_median_mean_x, 
             y =position_median_mean_y,
             label=label_mean_med,
             hjust= 0,
             size= font_size_legend,
             color = colors[3]) + 
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 2*font_size_title, face = "bold",colour = colors[4]), 
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.border=element_blank(),
          plot.background = element_blank(),
          legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"),
          plot.title = element_text(colour = colors[2],hjust = 0.15),
          plot.margin = margin(l = -10))
  
  return(hist)
}

plot_binary <- function(d_dictionnary,col_to_plot,value_to_plot,label_x){
  print(col_to_plot)
  col_to_plot_form = parse(text=col_to_plot)
  d <- d_dictionnary %>% filter(!is.na(eval(col_to_plot_form))) 
  n_na <- nrow(d)
  d <- d %>% group_by(eval(col_to_plot_form)) %>% 
    summarise(count=n(),percentage = round(100*n()/n_na,0))
  d <- d %>% mutate(percentage_label = ifelse(percentage>2 ,paste0(percentage,"%"),""))
  colnames(d) <- c("col","count","percentage","percentage_label")
  d <- d %>% filter(d$col == value_to_plot)
  text_in <- d$percentage > 70 
  hist <- ggplot(data=d, aes(x=col, y=count)) +
    geom_bar(stat="identity",fill=colors[5])+
    theme_bw() +
    geom_text(aes(label = percentage_label) ,y=0, hjust=-0.5, size = font_size_legend,col=colors[6]) + 
    scale_x_discrete(name="",label=NULL) + 
    scale_y_continuous(name = '', breaks = NULL, labels = NULL, limits = c(0,n_na)) + 
    coord_flip() +
    theme_bw() + 
    ggtitle(label_x) + 
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          panel.border=element_blank(),
          plot.background = element_blank(),
          legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"),
          plot.title = element_text(colour = colors[2])) 
  
  if(text_in){
    hist <- hist + geom_text(aes(label = count), hjust = 1, size = font_size_legend,col = colors[6])
  }else{
    hist <- hist + geom_text(aes(label = count), hjust = -0.5, size = font_size_legend,col = colors[4]) 
  }
  
  return(hist)
}

plot_survival <- function(d_dictionnary,delay,event,fill,colors, legend_title,legend_labels,save = TRUE, width = 4,height = 5){
  
  d_surv <- d_dictionnary[,c(delay,event,fill)]
  db_survival <- d_surv[which(!is.na(d_surv[,event]) & !is.na(d_surv[,delay]) & !is.na(d_surv[,fill]) & d_surv[,delay]>0),]
  db_survival[,event]<- as.numeric(as.character(db_survival[[event]]))

  #Trick to give a formula to survfit 
  #Found in https://github.com/kassambara/survminer/issues/228
  # construct formula, given column labels for time and event indicator
  survFormula <- as.formula(paste0("Surv(",delay,",",event,") ~ ",fill))
  # do some non-standard evaluation acrobatics to make survfit think is was called with the formula typed
  fitFunc <- function() {
    eval(substitute( expr = {
      survfit(survFormula, data = db_survival)
    },
    env = parent.frame()
    ))
  }
  
  survival <- ggsurvplot(
    data = db_survival,
    fit = fitFunc(), 
    xlab = "Months to diagnosis", 
    ylab = "",
    legend = "bottom",
    legend.title = legend_title,
    legend.labs= legend_labels,
    palette = colors[1:3],
    risk.table = TRUE,
    risk.table.title = "Number at risk",
    tables.theme = clean_theme()
  )
  
  survival$plot <- survival$plot+ 
    theme(legend.text = element_text(size = 10, color = "black", face = "bold"),
          legend.title =element_text(size = 10, color = "black", face = "bold"))
  
  if(save){
    file_name = file.path(
      Sys.getenv("PROJECT_PATH"),
      "core",
      opt$db_name,
      "docs",
      sprintf("%s_survival.png",opt$db_name))
    print(file_name)
    ggsave(file = file_name, print(survival),width = unit(width,"cm"),height=unit(height,"cm"))
  }else{
    return(print(survival))
  }
  
}
