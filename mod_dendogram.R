#### Dendogram Module ####




# Libraries
library(tidyverse)
library(hrbrthemes)
#devtools::install_github("jeromefroe/circlepackeR")
library(circlepackeR)
library(dplyr)
library(DT)
library(data.tree)
library(data.table)
library(lubridate)
library(collapsibleTree)
# Load the Shiny libraries
library(shiny)
library(shinythemes)
library(shinyjs)
# install.packages("remotes")
# library(remotes)
# install_github("AnalytixWare/ShinySky") # to get customized buttons
library(shinysky)
library(shinyalert)
useShinyalert()


dendogram_ui <- function(id){
  
    collapsibleTreeOutput("dendogram")
  
}



server <- function(input, output, session) {
  ### CirclePackeR

  ### Dendogram
  output$dendogram = renderCollapsibleTree({
    data <- readRDS("testdt.rds")
    data <- as.data.frame(gsub("[[:punct:]]", "", as.matrix(data)))
    
    data$pathString <-
      paste("EIG", data$Metier, data$Theme, data$Sujet, sep = "/")
    data$Statut <-
      ifelse(data$Prend == 1,
             "Curieux / Curieuse",
             "Connaisseuse / Connaisseur")
    
    data <- data %>%
      mutate(Prend = as.integer(Prend)) %>%
      mutate(Donne = as.integer(Donne)) %>%
      group_by(Sujet) %>%
      mutate(Total = n())
    
    collapsibleTree(
      data,
      c("Metier", "Theme", "Sujet", "Statut", "Nom"),
      tooltip = TRUE,
      root = "Bulloterie des EIG4",
      zoomable = TRUE
    )
    
  })

}


# c. Run the application
shinyApp(ui = ui, server = server)
