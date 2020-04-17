
#Creating Dashboard for state-level Diagnostics
library(shiny)
library(tidyverse)
library(rsconnect)


data_coal <- read_csv(file.path("C:","Users","henni","Documents","App State","Classes","Theses","Thesis - MSADA","Github_Repo",
                                "Coal_Mining-Thesis","Code","Shiny_App", "full_data_no_outliers.csv"))


ui <- fluidPage(
  titlePanel("Multilevel Regression Model Diagnostics App"),
  tabsetPanel(
      tabPanel("State-Level Diagnostics",
               sidebarLayout(
                 sidebarPanel(
                 selectInput('state', 'Select State', choices = unique(data_coal$State), 
                             selected = 'Alabama', multiple = F),
                 ), #SidebarPanel close
                 mainPanel(
                 tableOutput("number_obs"),
                 plotOutput('diag_plot'),
                 plotOutput("diag_plot_2")
                 ) #Main panel close
                ) #sidebar layout close
         ), #Tab Panel close
      tabPanel("Year-level Diagnostics",
               sidebarLayout(
                 sidebarPanel(
                   sliderInput('year_diag', 'Select Year', min = 2010, max = 2017, step = 1, value = 2010),
                 ), #SidebarPanel close
                 mainPanel(
                   tableOutput("summary_year"),
                   plotOutput('diag_plot_year'),
                   plotOutput("diag_plot_2_year")
                 ) #Main panel close
               ) #sidebar layout close
        )#tab panel close
      )#Tab-set Panel close
    )



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
      ggplot(aes(x=resid_value))+
      geom_density()+
      theme_bw()+
      labs(title = "Regression Model Diagnostics",
           subtitle = "Residuals Density",
           x="Residual Value",
           y="Density")
  })
  
  output$number_obs <- renderTable({
    data_coal%>%
      filter(State == input$state)%>% 
      summarize(Number_of_Observations = n(),
                Number_of_Counties = length(unique(County)),
                Proportion_Coal_Mining_Counties = mean(coal_mining),
                Average_Mortality = mean(mortality))
  })
  output$summary_year <- renderTable({
    data_coal%>%
      filter(year == input$year_diag)%>% 
      summarize(Number_of_Observations = n(),
                Number_of_Counties = length(unique(County)),
                Proportion_Coal_Mining_Counties = mean(coal_mining),
                Average_Mortality = mean(mortality))
  })
  output$diag_plot_year <- renderPlot({
    
    data_coal%>%
      filter(year == input$year_diag)%>%
      ggplot(aes(y=fit_value, x=resid_value))+
      geom_point()+
      theme_bw()+
      labs(title = "Regression Model Diagnostics",
           subtitle = "Residuals vs. Fitted Values, Facetted by Year",
           y="Fitted Value",
           x="Residual Value")
    })
  output$diag_plot_2_year <- renderPlot({
    data_coal%>%
      filter(year == input$year_diag)%>%
      ggplot(aes(x=resid_value))+
      geom_density()+
      theme_bw()+
      labs(title = "Regression Model Diagnostics",
           subtitle = "Residuals Density",
           x="Residual Value",
           y="Density")
  })
}

shinyApp(ui, server)
