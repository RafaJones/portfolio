#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        library(ggplot2)
        
        set.seed(4)
      
   visitsA <- reactive({ 
           
           rnorm(input$sampleA, mean = input$meanA, sd = input$sdA)
           })
   visitsB <- reactive({
           rnorm(input$sampleB, mean = input$meanB, sd = input$sdB) * (1 + input$trueDif)
   })
        
  output$freq1 <- renderPlot({
          
          dfA <- cbind.data.frame(visitsA(), rep("A",length(visitsA()))) 
          colnames(dfA) <- c("visits","Test")
          dfB <- cbind.data.frame(visitsB(), rep("B", length(visitsB())))
          colnames(dfB) <- c("visits","Test")
          df <- rbind.data.frame(dfA,dfB)
          
    gg <- ggplot(df, aes(visits, colour = Test)) + geom_freqpoly(bins = 10)
    gg
  })
  x <- reactive({ 
          if(input$varEqual){ 
                  t.test(visitsB(),visitsA(), var.equal = TRUE)}
          else t.test(visitsB(),visitsA(), var.equal = FALSE)
          })
  output$ttest <- renderPrint(x())
})
