data <- get_cbb_data()

ui <- fluidPage(
  
    theme = shinytheme("sandstone"),
  
    titlePanel("College Basketball Data"),
  
    navbarPage("Menu",
      tabPanel("Individual Team Data",
        sidebarLayout(
          sidebarPanel(
            selectInput('team', 'Teams', team_list(data), 
                        selected = "Southern Utah"),
            textOutput("text1")
          ),
          
          mainPanel(
            plotOutput("plot1"),
          ),
        ),
        
        hr(),
        
        fluidRow(
          column(8,
            DTOutput("table1")
          ),
          column(4,
            plotOutput("plot2")
          )
        ),
        
        hr(),
        
        fluidRow(
          column(12,
            plotOutput("plot3")
          )
        ),
        
        hr()
        
      ),
      
      tabPanel("All Team Data",
          fluidRow(
            column(8, offset = 3,
              textOutput("text2")
            )
          ),
          DTOutput("table2")
      )
    )

)

server <- function(input, output, session) {
  
  team <- reactive({input$team})
  
  output$text1 <- renderPrint({
    team_win_record(data,team(),1)
  })
  
  output$text2 <- renderPrint({
    "This page takes a minute to load. Thank you for your patience."
  })
    
  output$plot1 <- renderPlot({
       bbgraph(data, team())
     })  
  
  output$plot2 <- renderPlot({
    violingraph(data, team())
  })
  
  output$plot3 <- renderPlot({
    time_series_graph(data, team())
  })
  
  output$table1 <- renderDT(
      team_filter(data, team()),
      options = list(pageLength = 10, 
                     bFilter = FALSE,
                     dom = 'tp'),
      rownames = FALSE,
      colnames = c("Date", "Opponent", "Team Score", "Opponent Score", 
                   "Location", "Score Difference", "Result")
  )
  
  output$table2 <- renderDT(
    all_teams_records(data) %>% select(-weighted_wins),
    options = list(pageLength = 10,
                   dom = 'ftp'),
    colnames = c("Rank","Team","Wins","Total Games", "Win Percentage"),
    caption = "Rankings are based off a weighted win percentage: (# wins)^2 / (total games)"
  )
}

shinyApp(ui = ui, server = server)


