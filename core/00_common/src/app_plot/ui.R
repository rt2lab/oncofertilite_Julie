ui <- shinyUI(
  navbarPage("General plot",
             tabPanel("Filter database", uiOutput('page_filter_database')),
             tabPanel("Histogram 1", uiOutput('page_hist1')),
             tabPanel("Histogram 2",uiOutput('page_hist2')),
             tabPanel("Histogram 3",uiOutput('page_hist3')),
             tabPanel("Histogram 4",uiOutput('page_hist4')),
             tabPanel("Histogram 5",uiOutput('page_hist5')),
             tabPanel("Binary bars",uiOutput('page_binary')),
             tabPanel("Survival",uiOutput('page_survival')),
             tabPanel("Create plot",uiOutput('page_create_plot'))
  )
)