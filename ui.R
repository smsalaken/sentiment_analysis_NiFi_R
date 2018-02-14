## ui.R ##
library(shinydashboard)
library(gganimate)


dashboardPage(
  dashboardHeader(title = "Ni-Fi Sentiment Analyzer"),
  dashboardSidebar(
    sidebarMenu()
  ),
  dashboardBody(
    # radar plots
    fluidRow(
     column(6, box(width = 12, height = 600, solidHeader = T, status = "primary", title = "Sentiment - Sports",imageOutput('sentPlot'))),
     column(6, box(width = 12, height = 600, solidHeader = T, status = "danger", title = "Sentiment - Politics",imageOutput('sentPlotOnSub')))
    )
    
    # aggreagted bar plots
    ,fluidRow(
      column(6, box(width = 12, solidHeader = T, status = "primary", title = "Data Distribution - Sports",plotOutput('sentTable'))),
      column(6, box(width = 12, solidHeader = T, status = "danger", title = "Data Distibution - Politics",plotOutput('sentTableOnSUb')))
    )
    
  )
)