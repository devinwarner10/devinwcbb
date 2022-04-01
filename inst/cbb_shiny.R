data <- get_cbb_data(0)
conf_data <- get_cbb_data(1)

ui <- fluidPage(
  
    theme = shinytheme("sandstone"),
  
    titlePanel("College Basketball Data"),
  
    navbarPage("Menu",
      tabPanel("Individual Team Data",
        
          fluidRow(
            wellPanel(
              selectInput('team', 'Team', team_list(data), 
                          selected = "Southern Utah"),
              textOutput("text1")
            )
          ),
          
          navbarPage("Graphs",
              tabPanel("Scatter Plot",
                       plotOutput("plot1")),
              tabPanel("Violin Graph",
                       plotOutput("plot2")),
              tabPanel("Time Series",
                       wellPanel(
                         radioButtons("plotType", "Plot Type",
                                      c("Line" = 0, "Smooth" = 1))
                         ),
                       plotOutput("plot3")),
              tabPanel("Table",
                       DTOutput("table1")),
          ),
        
      ),
      
      tabPanel("Conference Data",
               fluidRow(
                 wellPanel(
                   selectInput('conf', 'Conference', conf_list(conf_data), 
                               selected = "Big Sky")
                 )
               ),
               
               DTOutput("table3")
               
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
  smooth <- reactive({input$plotType})
  conf <- reactive({input$conf})
  
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
    time_series_graph(data, team(), smooth())
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
  
  output$table3 <- renderDT(
    conf_data %>%
      filter(conference == conf()) %>%
      all_teams_records() %>%
      select(-weighted_wins),
    options = list(pageLength = 15,
                   dom = 'tp'),
    colnames = c("Rank","Team","Conference Wins","Conference Games", "Conference Win Percentage"),
    caption = "Rankings are based off a weighted win percentage: (# wins)^2 / (total games)"
  )
}

shinyApp(ui = ui, server = server)


