
library(shiny)
library(ggplot2)


#data cleaned with NBACLEANER see relevant github 
dataURL <- "https://raw.githubusercontent.com/RafaJones/NBA/master/allNBA.csv"
download.file(dataURL, destfile = "./allNBA.csv")
allNBA <- read.csv("./allNBA.csv",stringsAsFactors = FALSE)
allNBA <- allNBA[,c(10,31,33)]
colnames(allNBA) <- c("Results","GameScore","Player")
allNBA <- as.data.frame(allNBA)
# Define server logic 
shinyServer(function(input, output) {
       
               
        SlopeTable <- reactive({ 
                brushed_data <- brushedPoints(allNBA, input$brush1, 
                                              xvar = "GameScore", yvar = "Results")
                if(nrow(brushed_data) < 2){return(NULL)} 
                else {
                coeftbl = NULL 
                slope = NULL
                playerlist <- unique(brushed_data$Player)
                for(i in 1:length(playerlist)){ 
                        slope[i] <- lm(Results~GameScore, 
                  data = brushed_data[brushed_data$Player == playerlist[i],])$coefficients[2]
                coeftbl <- rbind.data.frame(coeftbl, c(playerlist[i],slope[i]), 
                                            stringsAsFactors = FALSE)
                }
        colnames(coeftbl) <- c("Player","Marginal Result")
        return(coeftbl)
                }
        })
       
       
                
       output$SlopeTable <- renderTable({
               if(length(SlopeTable()) == 0){
                       "No Points have been Brushed"
               }
               else SlopeTable()
       })
       

        
  output$gameplot <- renderPlot({
          durant <- if(input$durant) allNBA[allNBA$Player == "durant",] else NULL
          harden <- if(input$harden) allNBA[allNBA$Player == "harden",] else NULL
          westbrook <- if(input$westbrook) allNBA[allNBA$Player == "westbrook",] else NULL
          james <- if(input$james) allNBA[allNBA$Player == "james",]else NULL
          leonard <- if(input$leonard) allNBA[allNBA$Player == "leonard",] else NULL
          
          allNBA <- rbind.data.frame(durant,harden,westbrook,james,leonard,
                                               stringsAsFactors = FALSE)
          
          
          g <- ggplot(aes(x = GameScore, y = Results, color = Player), data = allNBA)
        gg <- g + geom_point() + geom_smooth(method = "lm") + labs(title = "Is Basketball a Team Game?",
                                                      x = "Individual Game Score",
                                                      y = "Point Difference")
        gg
        })
  
  output$documentation <- renderText({
"Selecting NBA players and assessing them by GameScore (a single stat that combines
all the points, assists, and other stats for a single player) and the Result (Point Differential).
This allows us to see how individual players stats correlate to the entire game. 
Over large samples, we can assess the importance of a player's individual impact to the game as a whole."
  })
  })

