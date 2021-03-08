#### La Bulloterie ####


### 1. Setup

# setting directory
setwd('/Users/raph/Documents/Etalab/Bulloterie/bulloterie')

# Libraries
library(tidyverse)
library(hrbrthemes)
#devtools::install_github("jeromefroe/circlepackeR")
library(circlepackeR)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(DT)


# Load dataset
data <- read.csv2("Bulloterie_Clean.csv", header=T, sep=";")

# Remove punctuation characters
data <- as.data.frame(gsub("[[:punct:]]", "", as.matrix(data)))

# Change data format using the data.tree library to produce the Bubble chart. We need a column like root/group/subgroup/...
library(data.tree)

data$pathString <- paste("EIG", data$Metier, data$Theme, data$Sujet, sep = "/")

data <- data %>% 
    mutate(Prend = as.integer(Prend)) %>%
    mutate(Donne = as.integer(Donne)) %>%
    group_by(Sujet) %>% 
    mutate(Total = n())

bulle <- as.Node(data)





# You can custom the minimum and maximum value of the color range.
Bubblechart <- circlepackeR(bulle, size = "Total", color_min = "rgb(0,0,145)", color_max = "rgb(206,206,206)")


### 2. Shiny interface

library(shiny)

# a. Define the UI


########### Dashboard page style
# 
# sidebar <- dashboardSidebar(
#     sidebarMenu(
#         menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
#         menuItem("Widgets", icon = icon("th"), tabName = "widgets",
#                  badgeLabel = "new", badgeColor = "green")
#     )
# )
# 
# body <- dashboardBody(
#     tags$style(HTML("<!doctype html>
#                   <html lang='fr' data-rf-reset>
#                   <head>
#                   <meta charset='utf-8'>
#                   <meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
#                   <link rel='stylesheet' href='css/all.min.css'>
#                   <title>Design System</title>
#                   </head>
#                   <body>
#                   
#                   <!-- 
#                   code de la page
#               -->
#                   
#                   <script src='js/all.min.js'></script>
#                   </body>
#                   </html>")),
#     tags$img(height = 100, src = "https://raw.githubusercontent.com/entrepreneur-interet-general/site-eig/811e28c13afa2d6d981d6ec1ae2cc1a7876cf6d1/img/eig_.svg"),
#     titlePanel("La Bulloterie EIG"),
#     tags$br(),
#     tags$body(
#         h1("Vue d'ensemble de la promo"),
#         p("L'ensemble des centres d'intérêts de la promo EIG 4 sont rassemblés ici. Pour trouver qui veut apprendre ou
#            transmettre les mêmes sujets que toi, utilise la barre de recherche !")),
#     tags$a(href="https://entrepreneur-interet-general.etalab.gouv.fr/", "This is a link to EIG"),
#     tags$img({Bubblechart}),
#     tabItems(
#         tabItem(tabName = "dashboard",
#                 h2("Dashboard tab content")
#         ),
#         
#         tabItem(tabName = "widgets",
#                 h2("Widgets tab content")
#         )
#     )
# )
# 
# ui2 <- dashboardPage(
#         skin = "red",
#         dashboardHeader(title = "La Bulloterie"),
#         sidebar,
#         body
#     )





######## FluidPage style
# 
# ui3 <- fluidPage(
#     # theme = shinytheme("flatly"),
#     # theme = "www/css/all.min.css", 
#     # Logo and application title
#     tags$img(height = 100, src = "https://raw.githubusercontent.com/entrepreneur-interet-general/site-eig/811e28c13afa2d6d981d6ec1ae2cc1a7876cf6d1/img/eig_.svg"),
#     titlePanel("La Bulloterie EIG"),
#     fluidRow(col(5,
#                 tags$body(
#                     h1('This is a level 1 title'))),
#                 col(5,
#                     p("L'ensemble des centres d'intérêts de la promo EIG 4 sont rassemblés ici. Pour trouver qui veut apprendre ou
#                        transmettre les mêmes sujets que toi, utilise la barre de recherche !"),
#                     #tags$a(href="https://entrepreneur-interet-general.etalab.gouv.fr/", "This is a link to EIG")
#                     )),
#                     
#     # Bubble chart
#     tags$img({Bubblechart}),
#     
#     # Add searchable input
#     #selectizeInput('foo', choices = NULL, ...),
#     
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 



ui <- navbarPage(
    
    # theme = shinytheme("flatly"),
    # theme = "www/css/all.min.css",
    # Logo and application title
    title = "La Bulloterie des EIG4",
    tabPanel("Bienvenue !",
             tags$img(height = 80, src = "https://raw.githubusercontent.com/entrepreneur-interet-general/site-eig/811e28c13afa2d6d981d6ec1ae2cc1a7876cf6d1/img/eig_.svg"),
             titlePanel("Bienvenue dans la Bulloterie des EIG 4 !"),
             tags$body(br(),
                       p("En Septembre 2020, les" , 
                        tags$b(tags$a(href="https://entrepreneur-interet-general.etalab.gouv.fr/","Entrepreneur.e.s d'Intérêt Général")), 
                        "se sont réuni.e.s lors du séminaire de lancement de la 4ème promotion.",
                        "Ensemble, ils et elles ont mis à plat leurs centres d'intérêts (outils, méthodes, mais aussi passions et loisirs !"),
                       br(),
                       p("Pour naviguer dans la Bulloterie, utilise les onglets ci-dessus."),
                       br(),
                       p("La", tags$b("vue d'ensemble"),"offre")
             ),
        ),
    tabPanel("Vue d'ensemble",
        tags$img(height = 80, src = "https://raw.githubusercontent.com/entrepreneur-interet-general/site-eig/811e28c13afa2d6d981d6ec1ae2cc1a7876cf6d1/img/eig_.svg"),
        titlePanel("La Bulloterie des EIG 4 : panorama des centres d'intérêts"),
        tags$body(p("L'ensemble des centres d'intérêts de la promo EIG 4 sont rassemblés ici. Pour trouver qui veut apprendre ou
                     transmettre les mêmes sujets que toi, utilise la barre de recherche !"),
                     #tags$a(href="https://entrepreneur-interet-general.etalab.gouv.fr/", "This is a link to EIG")
                 ),
        # Bubble chart
        fluidRow(column(8, offset = 2, 
                        tags$img({Bubblechart}))
                 )
        ),
    tabPanel("En détails",
        tags$img(height = 80, src = "https://raw.githubusercontent.com/entrepreneur-interet-general/site-eig/811e28c13afa2d6d981d6ec1ae2cc1a7876cf6d1/img/eig_.svg"),
             
        fluidRow(
            column(3,
                   h4("Bulloterie"),
            ),
            column(3,
                   h4("Explore la Bulloterie !"),
                   checkboxGroupInput("domainofinterest", 
                                      h3("Quel domaine t'intéresse ?"), 
                                      choices = list("Data" = "Data", 
                                                     "Design" = "Design", 
                                                     "Dev" = "Dev",
                                                     "Fun" = "Fun",
                                                     "Transverse" = "Transverse"),
                                      selected = 1)),
            br(),
            checkboxInput('Donne', 'Donne'),
            checkboxInput('Prend', 'Prend'),
    
            column(6,
                   DTOutput('tbl'))
            )
    )
)



# b. Define server logic required

server <- function(input, output, session) {
    # Try to update searchable text
    #updateSelectizeInput(session, 'foo', choices = data, server = TRUE) 
    output$tbl = renderDT(
        data, options = list(lengthChange = FALSE)
    )
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# c. Run the application 
shinyApp(ui = ui, server = server)

