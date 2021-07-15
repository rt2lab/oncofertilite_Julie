colors_plot = colors_sunburst
n_seq_max_plot = 4

id = "numdos_curie"
if(! id %in% colnames(d_dictionnary)){
  print("Here in sunburst")
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

texttoparse = paste("d_ttt %>% pivot_longer(-",id,',names_to = "ttt",values_to ="dat_ttt")')
d_ttt_seq <- eval(parse(text=texttoparse))
d_ttt_seq <- na.omit(d_ttt_seq)

#3 patientes qui n'ont rien du tout : "Pt_195" "Pt_373" "Pt_482"
# ces patientes sont en NA en date de first_bc_ttt. Est-ce que c'est normal? 

#Je n'ai que 172 patientes en NAC (variable neoadj_chemo == Yes).
#Pourquoi dans tes chiffres tu en as 173???? 

#2/3 patientes en trop qui commencent par chimio neo-adjuvante
#
# inc1 = d_ttt %>% filter(dat_first_surg >= dat_first_adj_ct) %>% mutate(REASON = "Surg after adj_ct")
# inc2 = d_ttt %>% filter(dat_first_surg >= dat_first_adj_ht) %>% mutate(REASON = "Surg after adj_ht")
# inc3 = d_ttt %>% filter(dat_first_surg >= dat_first_adj_rt) %>% mutate(REASON = "Surg after adj_rt")
# inc4 = d_ttt %>% filter(dat_first_surg >= dat_first_adj_antiher2) %>% mutate(REASON = "Surg after adj_antiher2")
# inc5 = d_ttt %>% filter(dat_first_adj_ct >= dat_first_adj_rt) %>% mutate(REASON = "Adj CT after adj RT")
# inc6 = d_ttt %>% filter(dat_first_adj_ct >= dat_first_adj_ht) %>% mutate(REASON = "Adj CT after adj HT")
# inc7 = d_ttt %>% filter(dat_first_adj_rt >= dat_first_adj_ht) %>% mutate(REASON = "Adj RT after adj HT")
# inc8 = d_ttt %>% filter(dat_first_adj_ct >= "2018-03-15" | 
#                         dat_first_adj_rt >= "2018-03-15" |
#                         dat_first_adj_ht >= "2018-03-15" | 
#                         dat_first_adj_antiher2 >= "2018-03-15" |
#                         dat_first_surg >= "2018-03-15" |
#                         dat_first_neo_ct >= "2018-03-15" |
#                         dat_first_neo_ht >= "2018-03-15" |  
#                         dat_first_neo_rt >= "2018-03-15" |
#                         dat_first_neo_antiher2 >= "2018-03-15" ) %>% mutate(REASON = "A treatment begins after March 2018")
# 
# inc <- rbind(inc1,inc2,inc3,inc4,inc4,inc5,inc6,inc7,inc8) %>% group_by_at(setdiff(names(inc), "REASON")) %>% summarise(REASON = paste(REASON,collapse =" and "))
# write.xlsx(inc,"/Users/elisedumas/Code/databases/core/05_feeric/data/incoherences_dates.xlsx")

#d_ttt_seq  <- d_ttt_seq %>% filter(! cletri %in% remove_from_sunburst)
d_ttt_seq$ttt <- factor(d_ttt_seq$ttt)
levels(d_ttt_seq$ttt) <- list("Surgery"=c("dat_first_surg"), 
                  "Radiotherapy"=c("dat_first_rt", "dat_first_neo_rt"),
                  "Hormonotherapy" = c("dat_first_ht", "dat_first_neo_ht"),
                  "Chemotherapy"= c("dat_first_neo_ct","dat_first_adj_ct"),
                  "Targeted therapy" = c("dat_first_neo_antiher2","dat_first_adj_antiher2"))

print(head(d_ttt_seq))
texttoparse = paste("d_ttt_seq %>% group_by(",id,") %>% 
  arrange(dat_ttt) %>% summarise(sequence = paste(ttt,collapse=' - '))")
d_ttt_seq <- eval(parse(text=texttoparse))

#Clara : parcours à tester. 
path1 = "Surgery - Chemotherapy - Radiotherapy - Targeted therapy"
d_ttt_seq$cletri[which(startsWith(d_ttt_seq$sequence,path1))]
path2 = "Chemotherapy - Radiotherapy - Surgery"
d_ttt_seq$cletri[which(startsWith(d_ttt_seq$sequence,path2))]
path3 = "Chemotherapy - Targeted therapy - Radiotherapy - Surgery"
d_ttt_seq$cletri[which(startsWith(d_ttt_seq$sequence,path3))]
path4 = "Chemotherapy - Targeted therapy - Surgery - Targeted therapy - Radiotherapy - Hormonotherapy"
d_ttt_seq$cletri[which(startsWith(d_ttt_seq$sequence,path4))]
path5 = "Chemotherapy - Targeted therapy - Surgery - Chemotherapy - Targeted therapy - Radiotherapy - Hormonotherapy"
d_ttt_seq$cletri[which(startsWith(d_ttt_seq$sequence,path5))]
path6 = "Chemotherapy - Targeted therapy - Surgery - Chemotherapy - Targeted therapy - Radiotherapy"
d_ttt_seq$cletri[which(startsWith(d_ttt_seq$sequence,path6))]
path7 = "Chemotherapy - Targeted therapy - Surgery - Targeted therapy"
d_ttt_seq$cletri[which(startsWith(d_ttt_seq$sequence,path7))]


d_ttt_seq_list <- sapply(d_ttt_seq$sequence,function(x){strsplit(x," - ")}) 
n_seq = sapply(1:length(d_ttt_seq_list),function(i) length(d_ttt_seq_list[[i]]))
n_seq_max = max(n_seq)
sunburst0 <- t(as.data.frame(c("root","","","white","",nrow(d_dictionnary))))
colnames(sunburst0) <- c("ids","labels","labels_count","colors","parents",'values')
sunburst_i = list()
colors_sunburst =list("Chemotherapy"=colors_plot[1],"Surgery" =colors_plot[2],
                      "Hormonotherapy"=colors_plot[3],"Radiotherapy"=colors_plot[4],
                      "Targeted therapy"= colors_plot[5])

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

file_name = file.path(
  Sys.getenv("PROJECT_PATH"),
  "core",
  opt$db_name,
  "docs/sunburst_treatment.pdf")
print(file_name)
plotly::orca(plot_ttt, file_name)

sunburst_nona <- sunburst %>% filter(ids != "root")
my_hist <- ggplot(sunburst_nona, aes(labels, fill = labels)) + 
  geom_bar() +scale_fill_manual(values = colors_sunburst) + theme(legend.key.size = unit(1, "cm"), 
                                                                  legend.text = element_text(colour = 'black', 
                                                                                             angle = 0, 
                                                                                             size = 8,
                                                                                             face = 'bold'),
                                                                  legend.title = element_blank(),
                                                                  legend.key.width = unit(1, "line")) +
  guides(fill=guide_legend(ncol=2))

legend <- cowplot::get_legend(my_hist)
legend_plot_ttt <- as_ggplot(legend)
