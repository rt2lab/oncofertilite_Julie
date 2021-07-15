server <- shinyServer(function(input, output, session) {
  
  output$page_filter_database <- renderUI({
    
    sidebarLayout(
       sidebarPanel(
        textInput("filter",
                  "Database filter",
                  value = "",
                  placeholder = "Ex: subtype==1 & !is.na(pCR)")
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        h2("Initial cohort size"),
        h3(nrow(d_dictionnary)),
        h2("Filtered cohort size"),
        h3(textOutput("new_cohort_size"))
      )
    )
  })
  
  output$new_cohort_size <- renderPrint({
    if(!input$filter == ""){
      as.numeric(nrow(d_dictionnary %>% filter(eval(parse(text=input$filter))))[1])
    }

  })

  output$page_hist1 <- renderUI({

    sidebarLayout(
      sidebarPanel(
        radioButtons("new_former_hist1", "Variable in data dictionnary",
                     c("Yes" = "Yes",
                       "No" = "No"),
                     selected = "Yes"),

        conditionalPanel(
          condition = "input.new_former_hist1 == 'Yes'",
          selectInput("var_hist1", "Choose a colname for Histogram 1:",
                      split(intersect(dictionnary$var,colnames(d_dictionnary)),dictionnary$family_var),
                      selected = "age_cl_10_1",
          ),
          selectInput("var_hist1_mean", "Choose a colname for mean/median Histogram 1:",
                      c(None = "None",split(dictionnary$var,dictionnary$family_var)),
                      selected = "age")
        ),

        conditionalPanel(
          condition = "input.new_former_hist1 == 'No'",
          selectInput("var_hist1_no", "Choose a colname for Histogram 1:",
                      #split(intersect(dictionnary$var,colnames(d_dictionnary)),dictionnary$family_var),
                      list("Former"=colnames(d_dictionnary)[!colnames(d_dictionnary) %in%  dictionnary$var]),
                      selected = "TYPANTEC12"
          ),
          selectInput("var_hist1_mean_no", "Choose a colname for mean/median Histogram 1:",
                      c(None = "None",
                        list("Former"=colnames(d_dictionnary)[!colnames(d_dictionnary) %in%  dictionnary$var]))

          ),
          textInput("hist1_label",
                    "Plot title (if empty, the variable name will be used)",
                    value = ""
          ),
          textInput("hist1_breaks",
                    "Plot breaks (follow model in placeholder",
                    value = "",
                    placeholder = "list('0'='Mastect','1'='Tum','2'='No surgery')"
          )
        ),
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot_hist1")
      )
    )
  })

  output$plot_hist1 <- renderPlot({
    if(input$new_former_hist1 == "Yes"){
      print("Former hist 1 ")
      levels_labels_names_hist1 <- get_levels_labels_name(input$var_hist1,dictionnary)
      listed <- setNames(sort(unique(d_dictionnary[,input$var_hist1])),
                        sort(unique(d_dictionnary[,input$var_hist1])))
      hist1 = list(col_to_plot = input$var_hist1,
                   col_to_mean = input$var_hist1_mean,
                   label_x = levels_labels_names_hist1$name,
                   breaks_x = if (is.null(levels_labels_names_hist1$labels)) listed else levels_labels_names_hist1$labels,
                   mean_med = (input$var_hist1_mean != "None")
      )
      d_dictionnary_plot = d_dictionnary
      if(!input$filter == ""){
        d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
      }
    }
    if(input$new_former_hist1 == "No"){
      list_breaks = eval(parse(text=input$hist1_breaks))
      hist1 = list(col_to_plot = input$var_hist1_no,
                   col_to_mean = input$var_hist1_mean_no,
                   label_x = ifelse(input$hist1_label=="",input$var_hist1_no,input$hist1_label),
                   breaks_x = list_breaks,
                   mean_med = (input$var_hist1_mean_no != "None")
      )
      d_dictionnary_plot = d_dictionnary
      if(!input$filter == ""){
        d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
      }
    }
    plot_geom_bar(d_dictionnary_plot,hist1$col_to_plot,hist1$col_to_mean,hist1$label_x,hist1$breaks_x,mean_med = hist1$mean_med)
  })

  output$page_hist2 <- renderUI({

    sidebarLayout(
      sidebarPanel(
        radioButtons("new_former_hist2", "Variable in data dictionnary",
                     c("Yes" = "Yes",
                       "No" = "No"),
                     selected = "Yes"),

        conditionalPanel(
          condition = "input.new_former_hist2 == 'Yes'",
          selectInput("var_hist2", "Choose a colname for Histogram 2:",
                      split(intersect(dictionnary$var,colnames(d_dictionnary)),dictionnary$family_var),
                      selected = "bmi_5cl",
          ),
          selectInput("var_hist2_mean", "Choose a colname for mean/median Histogram 2:",
                      c(None = "None",split(dictionnary$var,dictionnary$family_var)),
                      selected = "bmi")
        ),

        conditionalPanel(
          condition = "input.new_former_hist2 == 'No'",
          selectInput("var_hist2_no", "Choose a colname for Histogram 2:",
                      #split(intersect(dictionnary$var,colnames(d_dictionnary)),dictionnary$family_var),
                      list("Former"=colnames(d_dictionnary)[!colnames(d_dictionnary) %in%  dictionnary$var]),
                      selected = "TYPANTEC12"
          ),
          selectInput("var_hist2_mean_no", "Choose a colname for mean/median Histogram 2:",
                      c(None = "None",
                        list("Former"=colnames(d_dictionnary)[!colnames(d_dictionnary) %in%  dictionnary$var]))

          ),
          textInput("hist2_label",
                    "Plot title (if empty, the variable name will be used)",
                    value = ""
          ),
          textInput("hist2_breaks",
                    "Plot breaks (follow model in placeholder",
                    value = "",
                    placeholder = "list('0'='Mastect','1'='Tum','2'='No surgery')"
          )
        ),
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot_hist2")
      )
    )
  })

  output$plot_hist2 <- renderPlot({
    if(input$new_former_hist2 == "Yes"){
      levels_labels_names_hist2 <- get_levels_labels_name(input$var_hist2,dictionnary)
      listed <- setNames(sort(unique(d_dictionnary[,input$var_hist2])),
                         sort(unique(d_dictionnary[,input$var_hist2])))
      
      hist2 = list(col_to_plot = input$var_hist2,
                   col_to_mean = input$var_hist2_mean,
                   label_x = levels_labels_names_hist2$name,
                   breaks_x = if (is.null(levels_labels_names_hist2$labels)) listed else levels_labels_names_hist2$labels,
                   mean_med = (input$var_hist2_mean != "None")
      )
      d_dictionnary_plot = d_dictionnary
      if(!input$filter == ""){
        d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
      }
    }
    if(input$new_former_hist2 == "No"){
      list_breaks = eval(parse(text=input$hist2_breaks))
      hist2 = list(col_to_plot = input$var_hist2_no,
                   col_to_mean = input$var_hist2_mean_no,
                   label_x = ifelse(input$hist2_label=="",input$var_hist2_no,input$hist2_label),
                   breaks_x = list_breaks,
                   mean_med = (input$var_hist2_mean_no != "None")
      )
      d_dictionnary_plot = d_dictionnary
      if(!input$filter == ""){
        d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
      }
    }
    plot_geom_bar(d_dictionnary_plot,hist2$col_to_plot,hist2$col_to_mean,hist2$label_x,hist2$breaks_x,mean_med = hist2$mean_med)
  })

  output$page_hist3 <- renderUI({

    sidebarLayout(
      sidebarPanel(
        radioButtons("new_former_hist3", "Variable in data dictionnary",
                     c("Yes" = "Yes",
                       "No" = "No"),
                     selected = "Yes"),

        conditionalPanel(
          condition = "input.new_former_hist3 == 'Yes'",
          selectInput("var_hist3", "Choose a colname for Histogram 3:",
                      split(intersect(dictionnary$var,colnames(d_dictionnary)),dictionnary$family_var),
                      selected = "subtype",
          ),
          selectInput("var_hist3_mean", "Choose a colname for mean/median Histogram 3:",
                      c(None = "None",split(dictionnary$var,dictionnary$family_var)),
                      selected = "None")
        ),

        conditionalPanel(
          condition = "input.new_former_hist3 == 'No'",
          selectInput("var_hist3_no", "Choose a colname for Histogram 3:",
                      #split(intersect(dictionnary$var,colnames(d_dictionnary)),dictionnary$family_var),
                      list("Former"=colnames(d_dictionnary)[!colnames(d_dictionnary) %in%  dictionnary$var]),
                      selected = "TYPANTEC12"
          ),
          selectInput("var_hist3_mean_no", "Choose a colname for mean/median Histogram 3:",
                      c(None = "None",
                        list("Former"=colnames(d_dictionnary)[!colnames(d_dictionnary) %in%  dictionnary$var]))

          ),
          textInput("hist3_label",
                    "Plot title (if empty, the variable name will be used)",
                    value = ""
          ),
          textInput("hist3_breaks",
                    "Plot breaks (follow model in placeholder",
                    value = "",
                    placeholder = "list('0'='Mastect','1'='Tum','2'='No surgery')"
          )
        ),
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot_hist3")
      )
    )
  })

  output$plot_hist3 <- renderPlot({
    if(input$new_former_hist3 == "Yes"){
      levels_labels_names_hist3 <- get_levels_labels_name(input$var_hist3,dictionnary)
      listed <- setNames(sort(unique(d_dictionnary[,input$var_hist3])),
                         sort(unique(d_dictionnary[,input$var_hist3])))
      hist3 = list(col_to_plot = input$var_hist3,
                   col_to_mean = input$var_hist3_mean,
                   label_x = levels_labels_names_hist3$name,
                   breaks_x = if (is.null(levels_labels_names_hist3$labels)) listed else levels_labels_names_hist3$labels,
                   mean_med = (input$var_hist3_mean != "None")
      )
      d_dictionnary_plot = d_dictionnary
      if(!input$filter == ""){
        d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
      }
    }
    if(input$new_former_hist3 == "No"){
      list_breaks = eval(parse(text=input$hist3_breaks))
      hist3 = list(col_to_plot = input$var_hist3_no,
                   col_to_mean = input$var_hist3_mean_no,
                   label_x = ifelse(input$hist3_label=="",input$var_hist3_no,input$hist3_label),
                   breaks_x = list_breaks,
                   mean_med = (input$var_hist3_mean_no != "None")
      )
      d_dictionnary_plot = d_dictionnary
      if(!input$filter == ""){
        d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
      }
    }
    plot_geom_bar(d_dictionnary_plot,hist3$col_to_plot,hist3$col_to_mean,hist3$label_x,hist3$breaks_x,mean_med = hist3$mean_med)
  })

  output$page_hist4 <- renderUI({

    sidebarLayout(
      sidebarPanel(
        radioButtons("new_former_hist4", "Variable in data dictionnary",
                     c("Yes" = "Yes",
                       "No" = "No"),
                     selected = "Yes"),

        conditionalPanel(
          condition = "input.new_former_hist4 == 'Yes'",
          selectInput("var_hist4", "Choose a colname for Histogram 4:",
                      split(intersect(dictionnary$var,colnames(d_dictionnary)),dictionnary$family_var),
                      selected = "grade_3cl",
          ),
          selectInput("var_hist4_mean", "Choose a colname for mean/median Histogram 4:",
                      c(None = "None",split(dictionnary$var,dictionnary$family_var)),
                      selected = "None")
        ),

        conditionalPanel(
          condition = "input.new_former_hist4 == 'No'",
          selectInput("var_hist4_no", "Choose a colname for Histogram 4:",
                      list("Former"=colnames(d_dictionnary)[!colnames(d_dictionnary) %in%  dictionnary$var]),
                      selected = "TYPANTEC12"
          ),
          selectInput("var_hist4_mean_no", "Choose a colname for mean/median Histogram 4:",
                      c(None = "None",
                        list("Former"=colnames(d_dictionnary)[!colnames(d_dictionnary) %in%  dictionnary$var]))

          ),
          textInput("hist4_label",
                    "Plot title (if empty, the variable name will be used)",
                    value = ""
          ),
          textInput("hist4_breaks",
                    "Plot breaks (follow model in placeholder",
                    value = "",
                    placeholder = "list('0'='Mastect','1'='Tum','2'='No surgery')"
          )
        ),
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot_hist4")
      )
    )
  })

  output$plot_hist4 <- renderPlot({
    if(input$new_former_hist4 == "Yes"){
      levels_labels_names_hist4 <- get_levels_labels_name(input$var_hist4,dictionnary)
      listed <- setNames(sort(unique(d_dictionnary[,input$var_hist4])),
                         sort(unique(d_dictionnary[,input$var_hist4])))
      hist4 = list(col_to_plot = input$var_hist4,
                   col_to_mean = input$var_hist4_mean,
                   label_x = levels_labels_names_hist4$name,
                   breaks_x = if (is.null(levels_labels_names_hist4$labels)) listed else levels_labels_names_hist4$labels,
                   mean_med = (input$var_hist4_mean != "None")
      )
      d_dictionnary_plot = d_dictionnary
      if(!input$filter == ""){
        d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
      }
    }
    if(input$new_former_hist4 == "No"){
      list_breaks = eval(parse(text=input$hist4_breaks))
      hist4 = list(col_to_plot = input$var_hist4_no,
                   col_to_mean = input$var_hist4_mean_no,
                   label_x = ifelse(input$hist4_label=="",input$var_hist4_no,input$hist4_label),
                   breaks_x = list_breaks,
                   mean_med = (input$var_hist4_mean_no != "None")
      )
      d_dictionnary_plot = d_dictionnary
      if(!input$filter == ""){
        d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
      }
    }
    plot_geom_bar(d_dictionnary_plot,hist4$col_to_plot,hist4$col_to_mean,hist4$label_x,hist4$breaks_x,mean_med = hist4$mean_med)
  })

  output$page_hist5 <- renderUI({

    sidebarLayout(
      sidebarPanel(
        radioButtons("new_former_hist5", "Variable in data dictionnary",
                     c("Yes" = "Yes",
                       "No" = "No"),
                     selected = "Yes"),

        conditionalPanel(
          condition = "input.new_former_hist5 == 'Yes'",
          selectInput("var_hist5", "Choose a colname for Histogram 5:",
                      split(intersect(dictionnary$var,colnames(d_dictionnary)),dictionnary$family_var),
                      selected = "histo_3cl",
          ),
          selectInput("var_hist5_mean", "Choose a colname for mean/median Histogram 5:",
                      c(None = "None",split(dictionnary$var,dictionnary$family_var)),
                      selected = "None")
        ),

        conditionalPanel(
          condition = "input.new_former_hist5 == 'No'",
          selectInput("var_hist5_no", "Choose a colname for Histogram 5:",
                      #split(intersect(dictionnary$var,colnames(d_dictionnary)),dictionnary$family_var),
                      list("Former"=colnames(d_dictionnary)[!colnames(d_dictionnary) %in%  dictionnary$var]),
                      selected = "TYPANTEC12"
          ),
          selectInput("var_hist5_mean_no", "Choose a colname for mean/median Histogram 5:",
                      c(None = "None",
                        list("Former"=colnames(d_dictionnary)[!colnames(d_dictionnary) %in%  dictionnary$var]))

          ),
          textInput("hist5_label",
                    "Plot title (if empty, the variable name will be used)",
                    value = ""
          ),
          textInput("hist5_breaks",
                    "Plot breaks (follow model in placeholder",
                    value = "",
                    placeholder = "list('0'='Mastect','1'='Tum','2'='No surgery')"
          )
        ),
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot_hist5")
      )
    )
  })

  output$plot_hist5 <- renderPlot({
    if(input$new_former_hist5 == "Yes"){
      listed <- setNames(sort(unique(d_dictionnary[,input$var_hist5])),
                         sort(unique(d_dictionnary[,input$var_hist5])))
      levels_labels_names_hist5 <- get_levels_labels_name(input$var_hist5,dictionnary)
      hist5 = list(col_to_plot = input$var_hist5,
                   col_to_mean = input$var_hist5_mean,
                   label_x = levels_labels_names_hist5$name,
                   breaks_x = if (is.null(levels_labels_names_hist5$labels)) listed else levels_labels_names_hist5$labels,
                   mean_med = (input$var_hist5_mean != "None")
      )
      d_dictionnary_plot = d_dictionnary
      if(!input$filter == ""){
        d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
      }
    }
    if(input$new_former_hist5 == "No"){
      list_breaks = eval(parse(text=input$hist5_breaks))
      hist5 = list(col_to_plot = input$var_hist5_no,
                   col_to_mean = input$var_hist5_mean_no,
                   label_x = ifelse(input$hist5_label=="",input$var_hist5_no,input$hist5_label),
                   breaks_x = list_breaks,
                   mean_med = (input$var_hist5_mean_no != "None")
      )
      d_dictionnary_plot = d_dictionnary
      if(!input$filter == ""){
        d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
      }
    }
    plot_geom_bar(d_dictionnary_plot,hist5$col_to_plot,hist5$col_to_mean,hist5$label_x,hist5$breaks_x,mean_med = hist5$mean_med)
  })

  output$page_binary <- renderUI({

    sidebarLayout(
      sidebarPanel(
        radioButtons("new_former_binary1", "Binary 1 : Variable in data dictionnary",
                     c("Yes" = "Yes","No" = "No"),
                     selected = "Yes"),

        conditionalPanel(
          condition = "input.new_former_binary1 == 'Yes'",

          selectInput("var_binary1", "Choose a colname for binary bar 1:",
                      split(dictionnary$var,dictionnary$family_var),
                      selected = "status_rfs_txt"
          ),

          selectInput("value_to_plot_binary1", "Choose a value to plot for binary bar 1:",
                      choices =list("Yes","No"),
                      selected ="Yes"
          )
        ),

        conditionalPanel(
          condition = "input.new_former_binary1 == 'No'",
          selectInput("var_binary1_no", "Choose a colname for binary bar 1:",
                      list("Former"=colnames(d_dictionnary)[!colnames(d_dictionnary) %in%  dictionnary$var])
          ),

          selectInput("value_to_plot_binary1_no", "Choose a value to plot for binary bar 1:",
                      list("Yes","No"),
                      selected = "Yes"
          ),
          textInput("binary1_label",
                    "Plot title (if empty, the variable name will be used)",
                    value = ""
          )
        ),

        radioButtons("new_former_binary2", "Binary 2 : Variable in data dictionnary",
                     c("Yes" = "Yes","No" = "No"),
                     selected = "Yes"),

        conditionalPanel(
          condition = "input.new_former_binary2 == 'Yes'",

          selectInput("var_binary2", "Choose a colname for binary bar 2:",
                      split(dictionnary$var,dictionnary$family_var),
                      selected = "status_drfs_txt"
          ),

          selectInput("value_to_plot_binary2", "Choose a value to plot for binary bar 2:",
                      choices =list("Yes","No"),
                      selected ="Yes"
          )
        ),

        conditionalPanel(
          condition = "input.new_former_binary2 == 'No'",
          selectInput("var_binary2_no", "Choose a colname for binary bar 2:",
                      list("Former"=colnames(d_dictionnary)[!colnames(d_dictionnary) %in%  dictionnary$var]),
                      selected = ""
          ),

          selectInput("value_to_plot_binary2_no", "Choose a value to plot for binary bar 2:",
                      list("Yes","No"),
                      selected = "Yes"
          ),
          textInput("binary2_label",
                    "Plot title (if empty, the variable name will be used)",
                    value = ""
          )
        )

      ),


      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot_binary1"),
        plotOutput("plot_binary2")
      )
    )

  })

  output$plot_binary1 <- renderPlot({
    if(input$new_former_binary1 == "Yes"){
      levels_labels_names_binary1 <- get_levels_labels_name(input$var_binary1,dictionnary)
      binary1 = list(col_to_plot = input$var_binary1,
                     value_to_plot = names(levels_labels_names_binary1$labels[which(levels_labels_names_binary1$labels==input$value_to_plot_binary1)]),
                     label_x = paste(levels_labels_names_binary1$name,"(",input$value_to_plot_binary1,")"))
      d_dictionnary_plot = d_dictionnary
      if(!input$filter == ""){
        d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
      }
    }
    if(input$new_former_binary1 == "No"){
      bin1_name = ifelse(input$binary1_label=="",input$var_binary1_no,input$binary1_label)
      binary1 = list(col_to_plot = input$var_binary1_no,
                     value_to_plot = input$value_to_plot_binary1_no,
                     label_x = paste(bin1_name,"(",input$value_to_plot_binary1_no,")"))
      d_dictionnary_plot = d_dictionnary
      if(!input$filter == ""){
        d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
      }
    }
    plot_binary(d_dictionnary_plot,binary1$col_to_plot,binary1$value_to_plot,binary1$label_x)
  })

  observe({
    levels_labels_names_binary1 <- get_levels_labels_name(input$var_binary1,dictionnary)
    # Can also set the label and select items
    updateSelectInput(session, "value_to_plot_binary1",
                      choices = as.vector(levels_labels_names_binary1$labels)
    )
  })

  observe({
      updateSelectInput(session, "value_to_plot_binary1_no",
                        choices = as.vector(levels(factor(d_dictionnary[,input$var_binary1_no])))
      )

  })

  observe({
    levels_labels_names_binary2 <- get_levels_labels_name(input$var_binary2,dictionnary)
    # Can also set the label and select items
    updateSelectInput(session, "value_to_plot_binary2",
                      choices = as.vector(levels_labels_names_binary2$labels)
    )
  })

  observe({
    updateSelectInput(session, "value_to_plot_binary2_no",
                      choices = as.vector(levels(factor(d_dictionnary[,input$var_binary2_no])))
    )
  })

  output$plot_binary2 <- renderPlot({
    if(input$new_former_binary2 == "Yes"){
      levels_labels_names_binary2 <- get_levels_labels_name(input$var_binary2,dictionnary)
      binary2 = list(col_to_plot = input$var_binary2,
                     value_to_plot = names(levels_labels_names_binary2$labels[which(levels_labels_names_binary2$labels==input$value_to_plot_binary2)]),
                     label_x = paste(levels_labels_names_binary2$name,"(",input$value_to_plot_binary2,")"))
      d_dictionnary_plot = d_dictionnary
      if(!input$filter == ""){
        d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
      }
    }
    if(input$new_former_binary2 == "No"){
      bin2_name = ifelse(input$binary2_label=="",input$var_binary2_no,input$binary2_label)
      binary2 = list(col_to_plot = input$var_binary2_no,
                     value_to_plot = input$value_to_plot_binary2_no,
                     label_x = paste(bin2_name,"(",input$value_to_plot_binary2_no,")"))
      d_dictionnary_plot = d_dictionnary
      if(!input$filter == ""){
        d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
      }
    }
    plot_binary(d_dictionnary_plot,binary2$col_to_plot,binary2$value_to_plot,binary2$label_x)
  })
  
  output$page_survival <- renderUI({
    
    sidebarLayout(
      sidebarPanel(
        radioButtons("survival_or_hist", "Which plot do you want?",
                     c("Survival Curve" = "survival",
                       "Bar plot" = "hist"),
                     selected = "survival"),
        
        conditionalPanel(
          condition = "input.survival_or_hist == 'survival'",
          selectInput("survival_event", "Choose a colname event:",
                      split(intersect(dictionnary$var,colnames(d_dictionnary)),dictionnary$family_var),
                      selected = "status_rfs",
          ),
          selectInput("survival_delay", "Choose a colname for delay:",
                      split(intersect(dictionnary$var,colnames(d_dictionnary)),dictionnary$family_var),
                      selected = "delay_rfs"),
          selectInput("survival_fill", "Choose a colname for fill:",
                      split(intersect(dictionnary$var,colnames(d_dictionnary)),dictionnary$family_var),
                      selected = "subtype")
        ),

        conditionalPanel(
          condition = "input.survival_or_hist == 'hist'",
          radioButtons("new_former_hist_survival", "Variable in data dictionnary",
                       c("Yes" = "Yes",
                         "No" = "No"),
                       selected = "Yes"),
          
          conditionalPanel(
            condition = "input.new_former_hist_survival == 'Yes'",
            selectInput("var_hist_survival", "Choose a colname for Geom Bar:",
                        split(intersect(dictionnary$var,colnames(d_dictionnary)),dictionnary$family_var),
                        selected = "histo_3cl",
            ),
            selectInput("var_hist_survival_mean", "Choose a colname for mean/median Geom Bar:",
                        c(None = "None",split(dictionnary$var,dictionnary$family_var)),
                        selected = "None")
          ),
          
          conditionalPanel(
            condition = "input.new_former_hist_survival == 'No'",
            selectInput("var_hist_survival_no", "Choose a colname for Geom Bar:",
                        list("Former"=colnames(d_dictionnary)[!colnames(d_dictionnary) %in%  dictionnary$var]),
                        selected = "TYPANTEC12"
            ),
            selectInput("var_hist_survival_mean_no", "Choose a colname for mean/median Geom Bar:",
                        c(None = "None",
                          list("Former"=colnames(d_dictionnary)[!colnames(d_dictionnary) %in%  dictionnary$var]))
            ),
            textInput("hist_survival_label",
                      "Plot title (if empty, the variable name will be used)",
                      value = ""
            ),
            textInput("hist_survival_breaks",
                      "Plot breaks (follow model in placeholder",
                      value = "",
                      placeholder = "list('0'='Mastect','1'='Tum','2'='No surgery')"
            )
          )
          
        ),
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot_survival_or_hist")
      )
    )
  })
  
  output$plot_survival_or_hist <- renderPlot({
    if(input$survival_or_hist == 'survival'){
      legend_info <- get_levels_labels_name(input$survival_fill,dictionnary)
      d_dictionnary_plot = d_dictionnary
      if(!input$filter == ""){
        d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
      }
      plot_survival(d_dictionnary,
                    delay=input$survival_delay,
                    event=input$survival_event,
                    fill=input$survival_fill,
                    colors = colors,
                    legend_title = legend_info$name,
                    legend_labels = as.vector(legend_info$labels),
                    save = FALSE)

    }
    
    if(input$survival_or_hist == 'hist'){
      if(input$new_former_hist_survival == "Yes"){
        levels_labels_names_hist_survival <- get_levels_labels_name(input$var_hist_survival,dictionnary)
        listed <- setNames(sort(unique(d_dictionnary[,input$var_hist_survival])),
                           sort(unique(d_dictionnary[,input$var_hist_survival])))
        hist_survival = list(type ="hist",
                                col_to_plot = input$var_hist_survival,
                                col_to_mean = input$var_hist_survival_mean,
                                label_x = levels_labels_names_hist_survival$name,
                                breaks_x = if (is.null(levels_labels_names_hist_survival$labels)) listed else levels_labels_names_hist_survival$labels,
                                mean_med = (input$var_hist_survival_mean != "None")
        )
        d_dictionnary_plot = d_dictionnary
        if(!input$filter == ""){
          d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
        }
      }
      if(input$new_former_hist_survival == "No"){
        list_breaks = eval(parse(text=input$hist_survival_breaks))
        
        hist_survival = list(col_to_plot = input$var_hist_survival_no,
                     col_to_mean = input$var_hist_survival_mean_no,
                     label_x = ifelse(input$hist_survival_label=="",input$var_hist_survival_no,input$hist_survival_label),
                     breaks_x = list_breaks,
                     mean_med = (input$var_hist_survival_mean_no != "None")
        )
        d_dictionnary_plot = d_dictionnary
        if(!input$filter == ""){
          d_dictionnary_plot = d_dictionnary %>% filter(eval(parse(text=input$filter)))
        }
      }
      plot_geom_bar(d_dictionnary_plot,hist_survival$col_to_plot,hist_survival$col_to_mean,hist_survival$label_x,hist_survival$breaks_x,mean_med = hist_survival$mean_med)
      
    }

  })
  
  output$page_create_plot <- renderUI({

    # Show a plot of the generated distribution
    mainPanel(
      h3("Press button to create plot (This may take a few seconds)"),
      useShinyjs(),
      extendShinyjs(text = jscode, functions = c("closeWindow")),
      actionButton("button_create_plot", "I'm done!")
    )
  })

  observeEvent(input$button_create_plot, {
    print("Clicking")
    #Create plot list

    if(!input$filter == ""){
      d_dictionnary = d_dictionnary %>% filter(eval(parse(text=input$filter)))
    }
    ncohort <- length(unique(d_dictionnary$numdos_curie))
    nrows <- nrow(d_dictionnary)

    text1 = list(title = "Cohort size", value = ifelse(ncohort == 0,nrows,ncohort))
    
    if("year_diag" %in% colnames(d_dictionnary)){
      text2 = list(title = "Period of diagnosis", value = paste0(min(d_dictionnary$year_diag,na.rm=T)," - ",max(d_dictionnary$year_diag,na.rm=T)))
    }else{
      text2 = NULL
    }

    if("center" %in% colnames(d_dictionnary)){
      count_center = d_dictionnary %>% group_by(center) %>% dplyr::summarise(count=n())
      multiple_choices =  list(title = "Treatment Center",
                               label1 = "Institut Curie",
                               value1 = ifelse(length(which(count_center$center==1)>0),count_center[which(count_center$center==1),"count"],0),
                               label2 = "Other",
                               value2 = ifelse(length(which(count_center$center==2)>0),count_center[which(count_center$center==2),"count"],0)
      )
    }else{
      multiple_choices = NULL 
    }
    

    print(input$new_former_binary1)

    if(input$new_former_hist1 == "Yes"){
      levels_labels_names_hist1 <- get_levels_labels_name(input$var_hist1,dictionnary)
      listed <- setNames(sort(unique(d_dictionnary[,input$var_hist1])),
                         sort(unique(d_dictionnary[,input$var_hist1])))
      hist1 = list(col_to_plot = input$var_hist1,
                   col_to_mean = input$var_hist1_mean,
                   label_x = levels_labels_names_hist1$name,
                   breaks_x = if (is.null(levels_labels_names_hist1$labels)) listed else levels_labels_names_hist1$labels,
                   mean_med = (input$var_hist1_mean != "None")
      )
    }

    if(input$new_former_hist1 == "No"){
      list_breaks = eval(parse(text=input$hist1_breaks))
      hist1 = list(col_to_plot = input$var_hist1_no,
                   col_to_mean = input$var_hist1_mean_no,
                   label_x = ifelse(input$hist1_label=="",input$var_hist1_no,input$hist1_label),
                   breaks_x = list_breaks,
                   mean_med = (input$var_hist1_mean_no != "None")
      )
    }
    if(input$new_former_hist2 == "Yes"){
      levels_labels_names_hist2 <- get_levels_labels_name(input$var_hist2,dictionnary)
      listed <- setNames(sort(unique(d_dictionnary[,input$var_hist2])),
                         sort(unique(d_dictionnary[,input$var_hist2])))
      hist2 = list(col_to_plot = input$var_hist2,
                   col_to_mean = input$var_hist2_mean,
                   label_x = levels_labels_names_hist2$name,
                   breaks_x = if (is.null(levels_labels_names_hist2$labels)) listed else levels_labels_names_hist2$labels,
                   mean_med = (input$var_hist2_mean != "None")
      )
    }
    if(input$new_former_hist2 == "No"){
      list_breaks = eval(parse(text=input$hist2_breaks))
      hist2 = list(col_to_plot = input$var_hist2_no,
                   col_to_mean = input$var_hist2_mean_no,
                   label_x = ifelse(input$hist2_label=="",input$var_hist2_no,input$hist2_label),
                   breaks_x = list_breaks,
                   mean_med = (input$var_hist2_mean_no != "None")
      )
    }
    if(input$new_former_hist3 == "Yes"){
      levels_labels_names_hist3 <- get_levels_labels_name(input$var_hist3,dictionnary)
      listed <- setNames(sort(unique(d_dictionnary[,input$var_hist3])),
                         sort(unique(d_dictionnary[,input$var_hist3])))
      hist3 = list(col_to_plot = input$var_hist3,
                   col_to_mean = input$var_hist3_mean,
                   label_x = levels_labels_names_hist3$name,
                   breaks_x = if (is.null(levels_labels_names_hist3$labels)) listed else levels_labels_names_hist3$labels,
                   mean_med = (input$var_hist3_mean != "None")
      )
    }
    if(input$new_former_hist3 == "No"){
      list_breaks = eval(parse(text=input$hist3_breaks))
      hist3 = list(col_to_plot = input$var_hist3_no,
                   col_to_mean = input$var_hist3_mean_no,
                   label_x = ifelse(input$hist3_label=="",input$var_hist3_no,input$hist3_label),
                   breaks_x = list_breaks,
                   mean_med = (input$var_hist3_mean_no != "None")
      )
    }
    print("After hist")
    print(input$new_former_binary1)

    if(input$new_former_hist4 == "Yes"){
      levels_labels_names_hist4 <- get_levels_labels_name(input$var_hist4,dictionnary)
      listed <- setNames(sort(unique(d_dictionnary[,input$var_hist4])),
                         sort(unique(d_dictionnary[,input$var_hist4])))
      hist4 = list(col_to_plot = input$var_hist4,
                   col_to_mean = input$var_hist4_mean,
                   label_x = levels_labels_names_hist4$name,
                   breaks_x = if (is.null(levels_labels_names_hist4$labels)) listed else levels_labels_names_hist4$labels,
                   mean_med = (input$var_hist4_mean != "None")
      )
    }
    if(input$new_former_hist4 == "No"){
      list_breaks = eval(parse(text=input$hist4_breaks))
      hist4 = list(col_to_plot = input$var_hist4_no,
                   col_to_mean = input$var_hist4_mean_no,
                   label_x = ifelse(input$hist4_label=="",input$var_hist4_no,input$hist4_label),
                   breaks_x = list_breaks,
                   mean_med = (input$var_hist4_mean_no != "None")
      )
    }

    if(input$new_former_hist5 == "Yes"){
      levels_labels_names_hist5 <- get_levels_labels_name(input$var_hist5,dictionnary)
      listed <- setNames(sort(unique(d_dictionnary[,input$var_hist5])),
                         sort(unique(d_dictionnary[,input$var_hist5])))
      hist5 = list(col_to_plot = input$var_hist5,
                   col_to_mean = input$var_hist5_mean,
                   label_x = levels_labels_names_hist5$name,
                   breaks_x = if (is.null(levels_labels_names_hist5$labels)) listed else levels_labels_names_hist5$labels,
                   mean_med = (input$var_hist5_mean != "None")
      )
    }
    if(input$new_former_hist5 == "No"){
      list_breaks = eval(parse(text=input$hist5_breaks))
      hist5 = list(col_to_plot = input$var_hist5_no,
                   col_to_mean = input$var_hist5_mean_no,
                   label_x = ifelse(input$hist5_label=="",input$var_hist5_no,input$hist5_label),
                   breaks_x = list_breaks,
                   mean_med = (input$var_hist5_mean_no != "None")
      )
    }

    if(input$new_former_binary1 == "Yes"){
      levels_labels_names_binary1 <- get_levels_labels_name(input$var_binary1,dictionnary)
      binary1 = list(col_to_plot = input$var_binary1,
                     value_to_plot = names(levels_labels_names_binary1$labels[which(levels_labels_names_binary1$labels==input$value_to_plot_binary1)]),
                     label_x = paste(levels_labels_names_binary1$name,"(",input$value_to_plot_binary1,")"))
    }
    if(input$new_former_binary1 == "No"){
      bin1_name = ifelse(input$binary1_label=="",input$var_binary1_no,input$binary1_label)
      binary1 = list(col_to_plot = input$var_binary1_no,
                     value_to_plot = input$value_to_plot_binary1_no,
                     label_x = paste(bin1_name,"(",input$value_to_plot_binary1_no,")"))    }

    if(input$new_former_binary2 == "Yes"){
      levels_labels_names_binary2 <- get_levels_labels_name(input$var_binary2,dictionnary)
      binary2 = list(col_to_plot = input$var_binary2,
                     value_to_plot = names(levels_labels_names_binary2$labels[which(levels_labels_names_binary2$labels==input$value_to_plot_binary2)]),
                     label_x = paste(levels_labels_names_binary2$name,"(",input$value_to_plot_binary2,")"))
    }
    if(input$new_former_binary2 == "No"){
      bin2_name = ifelse(input$binary2_label=="",input$var_binary2_no,input$binary2_label)
      binary2 = list(col_to_plot = input$var_binary2_no,
                     value_to_plot = input$value_to_plot_binary2_no,
                     label_x = paste(bin2_name,"(",input$value_to_plot_binary2_no,")"))
    }
    
    if(input$survival_or_hist == 'survival'){
      legend_info <- get_levels_labels_name(input$survival_fill,dictionnary)
      survival_or_hist = list(
        type ="survival",
        delay=input$survival_delay,
        event=input$survival_event,
        fill=input$survival_fill,
        legend_title = legend_info$name,
        legend_labels = as.vector(legend_info$labels)
      )
    }
    
    if(input$survival_or_hist == 'hist'){
      if(input$new_former_hist_survival == "Yes"){
        levels_labels_names_hist_survival <- get_levels_labels_name(input$var_hist_survival,dictionnary)
        listed <- setNames(sort(unique(d_dictionnary[,input$var_hist_survival])),
                           sort(unique(d_dictionnary[,input$var_hist_survival])))
        survival_or_hist = list(type ="hist",
                             col_to_plot = input$var_hist_survival,
                             col_to_mean = input$var_hist_survival_mean,
                             label_x = levels_labels_names_hist_survival$name,
                             breaks_x = if (is.null(levels_labels_names_hist_survival$labels)) listed else levels_labels_names_hist_survival$labels,
                             mean_med = (input$var_hist_survival_mean != "None")
        )
      }
      if(input$new_former_hist_survival == "No"){
        list_breaks = eval(parse(text=input$hist_survival_breaks))
        survival_or_hist = list(type ="hist",
                             col_to_plot = input$var_hist_survival_no,
                             col_to_mean = input$var_hist_survival_mean_no,
                             label_x = ifelse(input$hist_survival_label=="",input$var_hist_survival_no,input$hist_survival_label),
                             breaks_x = list_breaks,
                             mean_med = (input$var_hist_survival_mean_no != "None")
        )

      }
    }

    plot_list <- list(text1 = text1,
                      text2 = text2,
                      multiple_choices = multiple_choices,
                      hist1 = hist1,
                      hist2 = hist2,
                      hist3 = hist3,
                      binary1 = binary1,
                      hist4 = hist4,
                      binary2 = binary2,
                      hist5 = hist5,
                      survival_or_hist = survival_or_hist)

    print("Sourcing")
    source(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/src/general_plot.R"))
    plot_final = get_plot_final(d_dictionnary, plot_list)
    print("End Sourcing")

    pdf(file.path(output_folder,"general_plot.pdf"), paper = "a4r", width=20, height=20)
    print(plot_final)
    dev.off()

    print("Done for plot pdf")
    stopApp()
    js$closeWindow()
  })

  #To test
  output$result <- renderText({
    paste("You chose", input$var_hist1, input$var_hist2)
  })
  
})