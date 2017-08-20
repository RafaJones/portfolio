#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny)
library(ggplot2)
library(reshape)
library(ggthemes)

fileUrl <- "http://databank.worldbank.org/data/download/PovStats_csv.zip"
#download.file(fileUrl, destfile = "./PovStats.zip")
filenames <- unzip("PovStats.zip", list = TRUE) #returns file names
#unzip("PovStats.zip")
PovData <- read.csv(filenames[1,1]) #PovStatsData.csv
Indicators <- unique(PovData$Indicator.Name)
population <- PovData[PovData$Indicator.Name == Indicators[13],]
population <- population[,c(1,2,5:46)]
colnames(population) <- c("country","abbr",1974:2015)

pop <- melt(population, id = c("country","abbr"))
colnames(pop) <- c("country","abbr","year","population")

pop <- na.omit(pop)
pop$population <- log(pop$population)

# Define server logic
shinyServer(function(input, output) {
        
        country <- pop$country
        year <- pop$year
        population <- pop$population
  df <- data.frame(country,year,population)
   
        specific.country <- reactive({
            a <- subset(df,country == input$place)
            return(a) 
            })
        
  output$PopPlot <- renderPlot({
          plotdata <- specific.country()
         
          g <- ggplot(plotdata, aes(x = year, y = population)) + 
                  geom_point()
          g <- g + theme_economist_white() + labs(x="Year",y = "Log Population")
          g <- g + ggtitle("Interactive Population Growth")
          g <- g + scale_x_discrete(breaks = c("1975","1985","1995","2005","2015"))
                         
          print(g)
        
  })
  
})
