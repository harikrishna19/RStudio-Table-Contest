# Shiny Interface for reactable -------------------------------------------
library(shiny)
#library(shinydashboard)
library(tidyverse)
library(reactable)
library(reactablefmtr)
options(shiny.autoreload = TRUE)
### app.R ##
# dat=read_csv("players_22.csv")
source("table_contst_data_cleaning.R")
ui <- pageWithSidebar(
  headerPanel('FIFA 22 Players Info'),
  sidebarPanel(
    selectizeInput("lea", "Select League", choices = unique(dat$league_name)),
    selectizeInput("team", "Select Team", choices = NULL),
    selectizeInput("pos", "Select Position", choices = NULL),width=2
  ),
  mainPanel(
    reactableOutput("react_table")
  )
)

server <- function(input, output,session) {
  
  
observeEvent(input$lea, {
    updateSelectizeInput(
      session = session,
      inputId = "team",
      "Select Team",
      choices = unique(dat$club_name[dat$league_name1 %in% input$lea ]))
    
  })
  
  observeEvent(input$team, {
    updateSelectizeInput(
      session = session,
      inputId = "pos",
      "Select Position",
      choices = unique(dat$player_position[dat$league_name1 %in% input$lea &
                                       dat$club_name %in% input$team])
    )
  }) 
  
  
  rf <- reactiveValues(
    df = NULL,
    league = NULL,
    team = NULL,
    position = NULL
  )
  rf$df <- dat
  rf$league <- dat$league_name1
  rf$team <- dat$club_name
  rf$position <- dat$player_position
  
  aa <- reactive({
    rf$df %>% filter(rf$league %in% input$lea &
                       rf$team %in% input$team &
                       rf$position %in% input$pos)
  })
    df <-reactive({
      if (input$pos =='GoalKeeper') {
       df <-  aa() %>% select(Player = short_name, Position=player_position,
                        Overall=overall,
                        Potential=potential,
                        Value,
                        'GK Diving'= goalkeeping_diving,   
                        'GK Handling'=goalkeeping_handling,   
                        'GK Kicking'=goalkeeping_kicking,
                        'GK Positioning'=goalkeeping_positioning,
                        'GK Reflexes'=goalkeeping_reflexes,   
                        'GK Speed'=goalkeeping_speed,)
      }
    else {
     df <-  aa() %>% select('Player' = short_name, 
                      'Position'=player_position,
                      'Overall'=overall,
                      'Potential'=potential,
                      Value,
                      'Pace'=pace,
                      'Shooting'=shooting,
                      'Dribbling'=dribbling,
                      'Passing'=passing,
                      'Defending'=defending,
                      'Physic'=physic)
    }
      return(df)
    })
  output$react_table <- renderReactable({
    # reactable(
    #   df(),
    #   pagination = FALSE,
    #   highlight=TRUE,
    #   compact=TRUE,
    #   defaultColDef = colDef(
    #     cell = data_bars(df(),
    #                      fill_color = c("lightblue","orange"),
    #                      text_position = "inside-end")
    #   )
    # )
    library(viridis)
    
    reactable(
      df(),
      columns = list(
        Value = colDef(
          style = function(value) {
            if (value > 0) {
              color <- "#008000"
            } else if (value < 0) {
              color <- "#e00000"
            } else {
              color <- "#777"
            }
            list(color = color, fontWeight = "bold")
          }
        )),
      theme=hoverdark(),
      resizable = TRUE, wrap = FALSE, bordered = TRUE,
        pagination = FALSE,
        highlight=TRUE,
        compact=TRUE,
      defaultSorted = "Player",
      defaultSortOrder = "desc",
      defaultColDef = colDef(
        cell = color_tiles(df(), colors = viridis::plasma(5))
      )
    )
    
    })
  
}

shinyApp(ui, server)
