#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

fileUrl <- "http://databank.worldbank.org/data/download/PovStats_csv.zip"
download.file(fileUrl, destfile = "./PovStats.zip")
filenames <- unzip("PovStats.zip", list = TRUE) #returns file names
unzip("PovStats.zip")
countries.list <- read.csv(filenames[2,1], stringsAsFactors = FALSE)
countries.list <- countries.list[,2]



# Make the data frame as a list
choice.country <- as.list(countries.list)


# Define UI for application that draws population for a country 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Population Growth for a Select Country"),
  
  # Sidebar with a selectInput to choose a country
  sidebarLayout(
    sidebarPanel(
            selectizeInput("place","Select a Country:",
                        choices = choice.country)
    ),
    
    # Show a plot of the population over time
    mainPanel(
       plotOutput("PopPlot")
    )
  )
))


