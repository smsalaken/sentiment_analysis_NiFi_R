library(shiny)
library(shinydashboard)
library(gganimate)

# source the function file
source('sent.R', local = T)
source('sent_onSub.R', local = T)
source('extras.R', local = T)



shinyServer(function(input, output, session) {
  
  # radar plot for subject/one topic
  output$sentPlot <- renderImage({
    # create the gif
    p <- sentiment_plot()$plot 
    
    # save the animation
    gganimate(p, "anim_subject.gif")
    
    
    # Return a list containing the filename
    list(src = "anim_subject.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
    
  # aggregated bar plot for subject/one topic
  output$sentTable <- renderPlot({
    sentiment_plot()$bar
  })
  
  # radar plot sentiments on subject/another topic
  output$sentPlotOnSub <- renderImage({
    # create the gif
    p <- sentiment_plot_onSub()$plot
    
    # save the animation    
    gganimate(p, "anim_ONsubject.gif")
    
    # Return a list containing the filename
    list(src = "anim_ONsubject.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )
  }, deleteFile = TRUE)
  
  # aggregated bar plot for 'on subject'/another topic
  output$sentTableOnSUb <- renderPlot({
    sentiment_plot_onSub()$bar
  })
  
  
  
  
})