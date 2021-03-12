#### La Bulloterie ####


### 1. Setup

# setting directory
setwd('/Users/raph/Documents/Etalab/Bulloterie/bulloterie')


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



# Load dataset
data <- readRDS("bulloterie.rds")

# Remove punctuation characters
data <- as.data.frame(gsub("[[:punct:]]", "", as.matrix(data)))

# Change data format using the data.tree library to produce the Bubble chart. We need a column like root/group/subgroup/...
library(data.tree)

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


p <-
  collapsibleTree(
    data,
    c("Metier", "Theme", "Sujet", "Statut", "Nom"),
    tooltip = TRUE,
    root = "Bulloterie des EIG4",
    zoomable = TRUE
  )


bulle <- as.Node(data)


# You can custom the minimum and maximum value of the color range.
Bubblechart <-
  circlepackeR(bulle,
               size = "Total",
               color_min = "rgb(0,0,145)",
               color_max = "rgb(206,206,206)")


input_data <- as.data.frame(data)
#saveRDS(input_data, "updated_bulloterie.rds")




# Making Data Tables editable
modFunction <- function(input, output, session, data, reset) {
  v <- reactiveValues(data = input_data)
  
  proxy = dataTableProxy("mod_table")
  
  observeEvent(input$mod_table_cell_edit, {
    print(names(v$data))
    info = input$mod_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    k = info$value
    str(info)
    
    isolate(if (j %in% match(c("Nom", "Sujet", "Theme"), names(v$data))) {
      print(match(c("Nom", "Sujet", "Theme"), names(v$data)))
      v$data[i, j] <<- DT::coerceValue(k, v$data[i, j])
      print(v$data)
      
      if (j %in% match("cost", names(v$data))) {
        v$data$updated_price <<- v$data$cost * v$data$ratio
      }
      if (j %in% match("ratio", names(v$data))) {
        v$data$updated_price <<- v$data$cost * v$data$ratio
      }
    } else {
      stop("You cannot change this column.") # check to stop the user from editing only few columns
    })
    replaceData(proxy, v$data, resetPaging = FALSE)  # replaces data displayed by the updated table
  })
  ### Reset Table
  observeEvent(reset(), {
    v$data <- data # your default data
  })
  
  print(isolate(colnames(v$data)))
  output$mod_table <- DT::renderDataTable({
    DT::datatable(v$data, editable = TRUE)
    
  })
}




ui <- navbarPage(
  theme = shinytheme("flatly"),
  # theme = "www/css/all.min.css",
  # Logo and application title
  title = "La Bulloterie des EIG4",
  
  
  tabPanel(
    "Bienvenue !",
    tags$img(height = 80, src = "https://raw.githubusercontent.com/entrepreneur-interet-general/site-eig/811e28c13afa2d6d981d6ec1ae2cc1a7876cf6d1/img/eig_.svg"),
    br(),
    titlePanel("Bienvenue dans la Bulloterie des EIG 4 !"),
    tags$body(
      br(),
      p(
        "En Septembre 2020, la 4ème promotion des" ,
        tags$b(
          tags$a(href = "https://entrepreneur-interet-general.etalab.gouv.fr/", "Entrepreneur.e.s d'Intérêt Général")
        ),
        "se retrouvaient pour la première fois, en collectif, à l'occasion du Bootcamp d'embarquement.",
        br(),
        br(),
        "Un des objectifs de cette semaine consistait à initier un esprit de promotion et encourager la collaboration malgré le contexte sanitaire.",
        " La ",
        tags$b("Bulloterie "),
        "- socialement distanciée bien sûr ! - y a contribué. ",
        " Avec cet exercice, chacun et chacune a mis à plat ses centres d'intérêts, qu'il s'agisse d'outils, de méthodes, ou encore de passions et loisirs personnels."
      ),
      br(),
      p(
        "Pour explorer plus en détails la",
        tags$b("Bulloterie"),
        "et même mettre à jour tes entrées, tu peux utiliser les onglets ci-dessus."
      ),
      br(),
      p(
        "La visualisation ci-dessous offre une vue d'ensemble de la diversité des centres d'intérêt et de la façon dont ils se recoupent."
      )
    ),
    circlepackeROutput("circle1", height = 1100)
  ),
  
  
  tabPanel(
    "Vue d'ensemble",
    tags$img(height = 80, src = "https://raw.githubusercontent.com/entrepreneur-interet-general/site-eig/811e28c13afa2d6d981d6ec1ae2cc1a7876cf6d1/img/eig_.svg"),
    titlePanel("La Bulloterie des EIG 4 : panorama des centres d'intérêts"),
    collapsibleTreeOutput("dendogram")
  ),
  
  tabPanel(
    "Dans le détail",
    tags$img(height = 80, src = "https://raw.githubusercontent.com/entrepreneur-interet-general/site-eig/811e28c13afa2d6d981d6ec1ae2cc1a7876cf6d1/img/eig_.svg"),
    
    fluidRow(
      column(
        6,
        h4("Explore la Bulloterie !"),
        br(),
        checkboxGroupInput(
          "domainofinterest",
          h3("Quel domaine t'intéresse ?"),
          choices = list(
            "Data" = "Data",
            "Design" = "Design",
            "Dev" = "Dev",
            "Fun" = "Fun",
            "Transverse" = "Transverse"
          ),
          selected = 1
        )
      ),
      br(),
      br(),
      column(
        6,
        checkboxInput('Donne', 'Donne'),
        checkboxInput('Prend', 'Prend')
      )
    ),
    
    fluidRow(DTOutput('tbl'))
  ),
  
  tabPanel(
    "Pour compléter tes entrées",
    tags$img(height = 80, src = "https://raw.githubusercontent.com/entrepreneur-interet-general/site-eig/811e28c13afa2d6d981d6ec1ae2cc1a7876cf6d1/img/eig_.svg"),
    
    fluidRow(
      tags$head(tags$style(HTML(
        '

                            .modal-lg {
                            width: 1200px;
                            }
                            '
      ))),
      helpText("Attention, n'oublie pas d'enregistrer tes modifications !"),
      br(),
      ### tags$head() is to customize the download button
      tags$head(
        tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")
      ),
      downloadButton("Trich_csv", "Download in CSV", class =
                       "butt"),
      useShinyalert(),
      # Set up shinyalert
      uiOutput("MainBody_trich"),
      actionButton(inputId = "Updated_trich", label = "Save")
    )
  )
)




server <- function(input, output, session) {
  ### CirclePackeR
  
  output$circle1 = renderCirclepackeR({
    data <- readRDS("bulloterie.rds")
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
    
    bulle <- as.Node(data)
    
    circlepackeR(bulle,
                 size = "Total",
                 color_min = "rgb(0,0,145)",
                 color_max = "rgb(206,206,206)")
    
  })
  ### Dendogram
  output$dendogram = renderCollapsibleTree({
    data <- readRDS("bulloterie.rds")
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
  
  
  ### DataTable
  demodata <- input_data
  callModule(modFunction, "editable", demodata,
             reset = reactive(input$reset))
  output$tbl = renderDT(data, options = list(lengthChange = FALSE))
  
  ### interactive dataset
  vals_trich <- reactiveValues()
  vals_trich$Data <- readRDS("bulloterie.rds")
  
  #### MainBody_trich is the id of DT table
  output$MainBody_trich <- renderUI({
    fluidPage(
      hr(),
      column(
        6,
        offset = 6,
        HTML(
          '<div class="btn-group" role="group" aria-label="Basic example" style = "padding:10px">'
        ),
        ### tags$head() This is to change the color of "Add a new row" button
        tags$head(
          tags$style(
            ".butt2{background-color:#231651;} .butt2{color: #e6ebef;}"
          )
        ),
        div(
          style = "display:inline-block;width:30%;text-align: center;",
          actionButton(
            inputId = "Add_row_head",
            label = "Ajouter",
            class = "butt2"
          )
        ),
        tags$head(
          tags$style(
            ".butt4{background-color:#4d1566;} .butt4{color: #e6ebef;}"
          )
        ),
        div(
          style = "display:inline-block;width:30%;text-align: center;",
          actionButton(
            inputId = "mod_row_head",
            label = "Modifier",
            class = "butt4"
          )
        ),
        tags$head(
          tags$style(
            ".butt3{background-color:#590b25;} .butt3{color: #e6ebef;}"
          )
        ),
        div(
          style = "display:inline-block;width:30%;text-align: center;",
          actionButton(
            inputId = "Del_row_head",
            label = "Supprimer",
            class = "butt3"
          )
        ),
        ### Optional: a html button
        # HTML('<input type="submit" name="Add_row_head" value="Add">'),
        HTML('</div>')
      ),
      
      column(12, dataTableOutput("Main_table_trich")),
      tags$script(
        "$(document).on('click', '#Main_table_trich button', function () {
                   Shiny.onInputChange('lastClickId',this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });"
      )
      
    )
  })
  
  #### render DataTable part ####
  output$Main_table_trich <- renderDataTable({
    DT = vals_trich$Data
    datatable(DT, selection = 'single',
              escape = F)
  })
  
  
  observeEvent(input$Add_row_head, {
    ### This is the pop up board for input a new row
    showModal(
      modalDialog(
        title = "Ajoute une nouvelle entrée",
        textInput(paste0("Nom_add", input$Add_row_head), "Nom"),
        textInput(paste0("Sujet_add", input$Add_row_head), "Sujet"),
        numericInput(paste0("Theme_add", input$Add_row_head), "Theme:", 0),
        selectInput(
          paste0("CMetier_add", input$Add_row_head),
          "Status:",
          choices = c("Yes", "On progress")
        ),
        textInput(paste0("Donne_add", input$Add_row_head), "Donne"),
        actionButton("go", "Sauvegarder l'entrée"),
        easyClose = TRUE,
        footer = NULL
      )
    )
    
  })
  ### Add a new row to DT
  observeEvent(input$go, {
    new_row = data.frame(
      Date = as.character(input[[paste0("Date_add", input$Add_row_head)]]),
      Description = input[[paste0("Description_add", input$Add_row_head)]],
      Names = input[[paste0("Names_add", input$Add_row_head)]],
      Request = input[[paste0("Request_add", input$Add_row_head)]],
      Completed = input[[paste0("Completed_add", input$Add_row_head)]],
      Comments = input[[paste0("Comments_add", input$Add_row_head)]]
    )
    vals_trich$Data <- rbind(vals_trich$Data, new_row)
    removeModal()
  })
  
  
  
  
  ### save to RDS part
  observeEvent(input$Updated_trich, {
    saveRDS(vals_trich$Data, "bulloterie.rds")
    shinyalert(title = "Saved!", type = "Tes modifications ont bien été enregistrées !")
  })
  
  
  
  ### delete selected rows part
  ### this is warning messge for deleting
  observeEvent(input$Del_row_head, {
    showModal(if (length(input$Main_table_trich_rows_selected) >= 1) {
      modalDialog(
        title = "Attention",
        paste(
          "Veux-tu vraiment supprimer",
          length(input$Main_table_trich_rows_selected),
          "entrées ?"
        ),
        footer = tagList(
          modalButton("Annuler"),
          actionButton("ok", "Oui, vas-y")
        ),
        easyClose = TRUE
      )
    } else{
      modalDialog(
        title = "Attention",
        paste("Sélectionne l'entrée que tu souhaites supprimer"),
        easyClose = TRUE
      )
    })
  })
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
    vals_trich$Data = vals_trich$Data[-input$Main_table_trich_rows_selected]
    removeModal()
  })
  
  ### edit button
  observeEvent(input$mod_row_head, {
    showModal(if (length(input$Main_table_trich_rows_selected) >= 1) {
      modalDialog(fluidPage(
        h3(strong("Modifier une entrée"), align = "center"),
        hr(),
        dataTableOutput('row_modif'),
        actionButton("save_changes", "Sauvegarder les modifications"),
        tags$script(
          HTML(
            "$(document).on('click', '#save_changes', function () {
                             var list_value=[]
                             for (i = 0; i < $( '.new_input' ).length; i++)
                             {
                             list_value.push($( '.new_input' )[i].value)
                             }
                             Shiny.onInputChange('newValue', list_value) });"
          )
        )
      ),
      size = "l")
    } else{
      modalDialog(
        title = "Attention",
        paste("Sélectionne l'entrée que tu souhaites modifier !"),
        easyClose = TRUE
      )
    })
  })
  
  
  
  
  #### modify part
  output$row_modif <- renderDataTable({
    selected_row = input$Main_table_trich_rows_selected
    old_row = vals_trich$Data[selected_row]
    row_change = list()
    for (i in colnames(old_row))
    {
      if (is.numeric(vals_trich$Data[[i]]))
      {
        row_change[[i]] <-
          paste0(
            '<input class="new_input" value= ',
            '"',
            old_row[[i]],
            '"',
            '  type="number" id=new_',
            i,
            ' ><br>'
          )
      }
      else if (is.Date(vals_trich$Data[[i]])) {
        row_change[[i]] <-
          paste0(
            '<input class="new_input" value= ',
            '"',
            old_row[[i]],
            '"',
            ' type="date" id=new_  ',
            i,
            '  ><br>'
          )
      }
      else
        row_change[[i]] <-
          paste0(
            '<input class="new_input" value= ',
            '"',
            old_row[[i]],
            '"',
            ' type="textarea"  id=new_',
            i,
            '><br>'
          )
    }
    row_change = as.data.table(row_change)
    setnames(row_change, colnames(old_row))
    DT = row_change
    DT
  }, escape = F, options = list(
    dom = 't',
    ordering = F,
    scrollX = TRUE
  ))
  
  
  
  ### This is to replace the modified row to existing row
  observeEvent(input$newValue,
               {
                 newValue = lapply(input$newValue, function(col) {
                   if (suppressWarnings(all(!is.na(as.numeric(
                     as.character(col)
                   ))))) {
                     as.numeric(as.character(col))
                   } else {
                     col
                   }
                 })
                 DF = data.frame(lapply(newValue, function(x)
                   t(data.frame(x))))
                 colnames(DF) = colnames(vals_trich$Data)
                 vals_trich$Data[input$Main_table_trich_rows_selected] <-
                   DF
                 
               })
  ### This is nothing related to DT Editor but I think it is nice to have a download function in the Shiny so user
  ### can download the table in csv
  output$Trich_csv <- downloadHandler(
    filename = function() {
      paste("Trich Project-Progress", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data.frame(vals_trich$Data), file, row.names = F)
    }
  )
  
}


# c. Run the application
shinyApp(ui = ui, server = server)
