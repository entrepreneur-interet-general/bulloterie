#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/






setwd('/Users/raph/Documents/Etalab/Bulloterie/bulloterie')
# Libraries
library(tidyverse)
library(hrbrthemes)
#devtools::install_github("jeromefroe/circlepackeR")
library(circlepackeR)
library(shiny)
library(shinydashboard)
library(dplyr)
# Load dataset from github
data <- read.csv2("Bulloterie_Clean.csv", header=T, sep=";")
#data[ which(data$value==-1),"value"] <- 1

data <- as.data.frame(gsub("[[:punct:]]", "", as.matrix(data)))

# Change the format. This use the data.tree library. This library needs a column that looks like root/group/subgroup/..., so I build it
library(data.tree)

data$pathString <- paste("EIG", data$Metier, data$Theme, data$Sujet, sep = "/")

data <- data %>% group_by(Sujet) %>% mutate(count = n())

bulle <- as.Node(data)

# You can custom the minimum and maximum value of the color range.
circlepackeR(bulle, size = "Prend", color_min = "hsl(152,80%,80%)", color_max = "hsl(228,30%,40%)")





library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = "all.min.css",

    # Logo and application title
    tags$img(height = 100, src = "https://raw.githubusercontent.com/entrepreneur-interet-general/site-eig/811e28c13afa2d6d981d6ec1ae2cc1a7876cf6d1/img/eig_.svg"),
    titlePanel("La Bulloterie EIG"),
    tags$br(),
    tags$p("L'ensemble des centres d'intérêts de la promo EIG 4 sont rassemblés ici. Pour trouver qui veut apprendre ou
           transmettre les mêmes sujets que toi, utilise la barre de recherche !"),
    tags$img({circlepackeR(bulle, size = "Prend",
                           color_min = "hsl(152,80%,80%)", color_max = "hsl(228,30%,40%)")}),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

