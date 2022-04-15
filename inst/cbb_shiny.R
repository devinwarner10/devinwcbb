data <- get_cbb_data(0)
conf_data <- get_cbb_data(1)
team_records <- read_csv("data/team_records.csv")
bb_lasso <- cbb_lasso(team_records)
lasso.cv <- bb_lasso[[1]]
best_lambda <- lasso.cv$lambda.min
umap_bball <- umap(team_records[,-c(1,2,16)])

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
                       DTOutput("table1"))
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
      ),
      
      tabPanel("LASSO",
          fluidRow(
            wellPanel(
              textOutput("text3"),
              sliderInput('lambda', 'Log(\u03BB)', min = round(log(best_lambda)-1,2), 
                          max = -1, value = log(best_lambda), step = 0.05)
            )
          ),
          fluidRow(
            column(6,plotOutput('plot4')),
            column(6,plotOutput('plot5'))
          ),
          
          fluidRow(
            DTOutput("table4")
          )     
      ),
      
      tabPanel("Dimension Reduction",
          navbarPage("Method",
            tabPanel("Principle Component",
               sidebarPanel(
                 selectInput('xcol', 'X Variable', names(pca_bball(team_records)[,-c(1,2,3)])),
                 selectInput('ycol', 'Y Variable', names(pca_bball(team_records)[,-c(1,2,3)]),
                             selected = names(pca_bball(team_records)[,-c(1,2,3)])[[2]]),
                 radioButtons("d1", "Filter for D1 Teams",
                              c("No" = 0, "Yes" = 1))

               ),

               mainPanel(
                 plotOutput('plot6')
               ),

               fluidRow(
                 DTOutput('table5')
               ),
               fluidRow(
                 DTOutput('table6')
               )
            ),
            tabPanel("UMAP",
                plotOutput('plot7')
            )
          )

      )
    )

)

server <- function(input, output, session) {
  
  team <- reactive({input$team})
  smooth <- reactive({input$plotType})
  conf <- reactive({input$conf})
  loglam <- reactive({input$lambda})
  d1_choice <- reactive({input$d1})
  
  
  output$text1 <- renderPrint({
    team_win_record(data,team(),TRUE)
  })
  
  output$text2 <- renderPrint({
    "This page takes a minute to load. Thank you for your patience."
  })
  
  output$text3 <- renderPrint({
    cbb_lasso(team_records, exp(loglam()))[[2]]
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
  
  output$plot4 <- renderPlot({
    plot(lasso.cv, main = "Training MSE Plot") + 
      abline(v = loglam(), col = "steelblue")
  })
  
  output$plot5 <- renderPlot({
    plot(log(lasso.cv$lambda),bb_lasso[[3]], main = "R^2 Plot",
         xlab = "Log(\u03BB)", ylab = "R^2") + 
      abline(v = loglam(), col = "steelblue")
  })
  
  output$plot6 <- renderPlot({
    plot_pca(team_records, input$xcol, input$ycol, d1_choice())
  })
  
  output$plot7 <- renderPlot({
    data.frame(umap_bball$layout, 
               Conference=team_records$conference, 
               Tournament=as.factor(team_records$tournament)) %>%
      ggplot(aes(X1,X2, fill = Conference, shape = Tournament))+
      geom_point(cex=3, pch=21) +
      coord_fixed(ratio = 1)
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
    options = list(pageLength = 20,
                   dom = 't'),
    colnames = c("Rank","Team","Conference Wins","Conference Games", "Conference Win Percentage"),
    caption = "Rankings are based off a weighted win percentage: (# wins)^2 / (total games)"
  )
  
  output$table4 <- renderDT(
    tibble("Variable" = names(predict(lasso.cv, s = exp(loglam()), type = "coefficient")[,1]),
           "Coefficient" = round(predict(lasso.cv, s = exp(loglam()), type = "coefficient")[,1],6)) %>%
      filter(Coefficient != 0),
    options = list(dom = 't')
  )
  
  output$table5 <- renderDT(
    pca_bball(team_records,0,d1_choice()),
    options = list(dom = '')
  )
  
  output$table6 <- renderDT(
    pca_bball(team_records,-1,d1_choice()),
    options = list(dom = '')
  )

  }

shinyApp(ui = ui, server = server)
