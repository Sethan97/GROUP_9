
## app.R ##
library(shinydashboard)
library(shiny)
library(tm)
library(ggplot2)
library(SentimentAnalysis)
library(RSentiment)
library(dplyr)



CAvideos <- read.csv("~/CAvideos.csv")
FRvideos <- read.csv("~/FRvideos.csv")
GBvideos <- read.csv("~/GBvideos.csv")
USvideos <- read.csv("~/USvideos.csv")
DEvideos <- read.csv("~/DEvideos.csv")



ui <- dashboardPage(skin="green",
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  
  dashboardBody(
    tabItems(
      tabItem("sentiment",
              sidebarPanel(
                width="3",
                # Input: Select the random distribution type ----
                radioButtons("Region", "Select Region:",
                             c("Canada",
                               "USA",
                               "Great Britain",
                               "France",
                               "Germany")),
                
                # br() element to introduce extra vertical spacing ----
                br()
               
              ),
    
      tabBox(
        title = "Visualizations",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "500px",width="9",
        tabPanel("Description Sentiments", "Polarity of video descriptions",plotOutput("plot")
 ),
        tabPanel("Sentiment 2", "Tab content 2"),
        tabPanel("Sentiment 3", "Tab content 3")
      )
  )
  ,
  tabItem("factors",
          h2("factors"))
  )   
  )
)

server <- function(input, output) {
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Home", tabName="home",icon = icon("home")),
      menuItem("Sentiment Analysis",tabName="sentiment"),
      menuItem("Factors for popularity",tabName="factors"),
      menuItem("Time analysis",tabName="time"),
      menuItem("ML Algorithms",tabName="ml")
    )
  })
  
  output$plot <- renderPlot({
    
  if(input$Region=="Canada")
  {
  CAvideos %>% select(category_id, likes) %>% group_by(category_id) %>% 
      ggplot(aes(x= category_id, y = likes))+geom_col()
   
   
    
     
  }
    
    
    
    
  })
}

shinyApp(ui, server)

