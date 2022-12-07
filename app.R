# INSERT Libraries HERE
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)

# INSERT Dataset HERE
df<-read.csv("Donnees .csv")

##########################
##### User interface #####
##########################
ui <- fluidPage(
  
  # Title 
  titlePanel(
    h1("Analyse du taux de change en Haiti", style = "padding-center: 20px")
  ),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(h2("Par", style = "padding-center: 15px"),
                 h2("Melissa Plaisir", style = "padding-center: 15px"),
                 h2("Et", style = "padding-center: 15px"),
                 h2("Maulie Ruben", style = "padding-center: 15px")),
    
    
    mainPanel(
      plotlyOutput(outputId = "timeSeries"),
      plotlyOutput(outputId = "timeSeries1"),
      plotlyOutput(outputId = "timeSeries2"),
      plotlyOutput(outputId = "timeSeries3"),
    )
  )
)


###########################
##### Server function #####
###########################
server <- function(input, output, session) {
  
  
  # Time series for air temperatures
  df1=filter(df, Banque %in% c("BRH","SI"))
  output$timeSeries <- renderPlotly({
    df1 %>% 
      plot_ly(
        x = ~Jours, 
        y = ~Taux,
        color = ~Banque, 
        type = 'scatter', 
        mode='lines',
        line = list(color = '#251d5a', width = 1.5)
      ) %>%
      layout(
        title = "Taux de change moyen par jour BRH vs SI",
        xaxis = list(title = "Jours"),
        yaxis = list(title = "Taux")
      )
  })
  # Time series for air temperatures
  df2=filter(df, !(Banque %in% c("BRH","SI")))
  output$timeSeries1 <- renderPlotly({
    df2 %>% 
      plot_ly(
        x = ~Jours, 
        y = ~Taux,
        color = ~Banque, 
        type = 'bar', 
        mode='lines',
        line = list(color = '#251d5a', width = 1.5)
      ) %>%
      layout(
        title = "Taux de change moyen par jour Banques Commerciale",
        xaxis = list(title = "Jours"),
        yaxis = list(title = "Taux")
      )
  })
  
  # Time series for air temperatures
  df3=df
  output$timeSeries2 <- renderPlotly({
    df3 %>% 
      plot_ly(
        x = ~Jours, 
        y = ~Taux,
        type = 'box', 
        mode='lines',
        line = list(color = '#251d5a', width = 1.5)
      ) %>%
      layout(
        title = "Dispersion Du taux de Change par Jour",
        xaxis = list(title = "Jours"),
        yaxis = list(title = "Taux")
      )
  })
  
  # Time series for air temperatures
  df3=df
  output$timeSeries3 <- renderPlotly({
    df3 %>% 
      plot_ly(
        x = ~Banque, 
        y = ~Taux,
        type = 'box', 
        color = ~Banque,
        mode='lines',
        line = list(color = '#251d5a', width = 1.5)
      ) %>%
      layout(
        title = "Dispersion Du taux de Change par Banque",
        xaxis = list(title = "Jours"),
        yaxis = list(title = "Taux")
      )
  })
  
  
  
}



##################################
##### Call shinyApp function #####
##################################
shinyApp(ui = ui, server = server)
