#Creating Dashboard for state-level Diagnostics
library(shiny)
library(tidyverse)

data_coal <- read_csv(file.path("C:","Users","henni","Documents","App State","Classes","Theses","Thesis - MSADA","Github_Repo",
                "Coal_Mining-Thesis","Data", "full_data_no_outliers.csv"))

ui <- fluidPage(
  titlePanel("Multilevel Regression Model Diagnostics App"),
  sidebarLayout(
    # CODE BELOW: Add a sidebarLayout, sidebarPanel, and mainPanel
    sidebarPanel(
      selectInput('state', 'Select State', choices = unique(data_coal$State) ,selected = 'AZ', multiple = F),
    ),
    mainPanel(
      tableOutput("number_obs"),
      plotOutput('diag_plot'),
      plotOutput("diag_plot_2")
    )
  ))

server <- function(input, output, session) {
  output$diag_plot <- renderPlot({
    
    data_coal%>%
      filter(State == input$state)%>% 
      ggplot(aes(x=fit_value, y=resid_value))+
      geom_point()+
      theme_bw()+
      labs(title = "Regression Model Diagnostics",
           subtitle = "Residuals vs. Fitted Values",
           x="Fitted Value",
           y="Residual Value")
  })
  
  output$diag_plot_2 <- renderPlot({
    
    data_coal%>%
      filter(State == input$state)%>% 
      ggplot(aes(y=fit_value, x=mortality))+
      geom_point()+
      theme_bw()+
      labs(title = "Regression Model Diagnostics",
           subtitle = "Observed Values vs. Fitted Values",
           x="Mortality Rate",
           y="Fitted Value")
  })
  
  output$number_obs <- renderTable({
    data_coal%>%
      filter(State == input$state)%>% 
      summarize(Number_of_Observations = n())
  })
}
shinyApp(ui = ui, server = server)
