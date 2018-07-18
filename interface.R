#packages
library(shinydashboard)
library(shiny)
library(tm)
library(corrplot)
library(data.table)
library(ggplot2)
library(dplyr)
library(plotly)
library(plyr)

#import datasets
CAvideos <- read.csv("~/CAvideos.csv")
FRvideos <- read.csv("~/FRvideos.csv")
GBvideos <- read.csv("~/GBvideos.csv")
USvideos <- read.csv("~/USvideos.csv")
DEvideos <- read.csv("~/DEvideos.csv")

Allvideos <- as.data.table(rbind(GBvideos,CAvideos,USvideos,DEvideos,FRvideos))

#ui
ui <- dashboardPage(skin="green",
  dashboardHeader(title = "Analysis"),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  
  dashboardBody(
    tabItems(
      #tabItem sentiment
      tabItem("sentiment",
      tabBox(
        title = "Visualizations",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "500px",width="6",
        tabPanel("Description Sentiments",
                 "Polarity of video descriptions",
                 plotOutput("plot")
 ),
        tabPanel("Sentiment 2", "Tab content 2"),
        tabPanel("Sentiment 3", "Tab content 3")
 
      )
  )
  ,
 #tabItem factors
  tabItem("factors",
          
          br(),
          h3("Factor 1:Are videos with more consistent communicators popular?"),
            fluidRow(
              box(
                width=8,height="500px",
                title="Sample Study - James Charles",status="success",
                plotlyOutput("factor1"),verbatimTextOutput("event")

              ),
              box(width=4,
                  status="success",p(
                    class = "text-muted",
                    paste("From analyzing videos posted by James Charles across a 5 month period,we were able to deduce
                          that popularity of a video posted was greater than the former hence consistency of posting videos leads to a greater."
                    ))
            )
          ),
          br(),
          h3("Factor 2:Does category of a video affectits popularity?")
  ),
 tabItem("time",
         h3("How much time passes between published and trending in days? "),
         br(),
         sidebarPanel(
           width="3",
           radioButtons("Region", "Select Region:",
                        c("Canada",
                          "USA",
                          "Great Britain",
                          "France",
                          "Germany")),
           br()
           
         ),
         box(
           width=9,height="370px",status="primary",
           title="Time between published and trending"
           
           
         )
         ,
         box(
           width = 6, background = "light-blue",
           p("From the visualizations potrayed above,
           we have come with the following conclusions,"
         ),
         p(">In Germany and France,it takes little time to reach the trending date,___ days")
         ,p(">It takes long for videos to trend in the Us,__ days"))
         
   
   
 ),
 tabItem("relation",
         
         br(),
         
         
           fluidRow(
             box(width=6,title = "Views vs likes",
                 status="success",plotOutput("plot_views_likes")),
             box(width=6,title = "Views vs Dislikes",
                 status="danger",plotOutput("plot_viewsdislikes"))
           ),br(),
         fluidRow(
           box(width=6,title = "Views vs Comments",
               status="primary",plotOutput("plot_views_comments")),
           box(width=6,title = "Correlation between views
               ,likes,dislikes and comment count",
               status="primary",plotOutput("plot_correlation"))
           ),
         br(),
         fluidRow
         (
           box(
             width=6,"From the graphs above,we see views and likes have a high correlation
             ,unlike views and comment count and views and dislikes that have low correlations"
           )
         )
 )
  )
))

server <- function(input, output) {
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Home", tabName="home",icon = icon("home")),
      menuItem("Sentiment Analysis",tabName="sentiment"),
      menuItem("Factors for popularity",tabName="factors"),
      menuItem("Time analysis",tabName="time"),
      menuItem(" General Relationships",tabName="relation")
    )
  })
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  
  output$plot <- renderPlot({
    
  if(input$Region=="Canada")
  {
  CAvideos %>% select(category_id, likes) %>% group_by(category_id) %>% 
      ggplot(aes(x= category_id, y = likes))+geom_col()
   
  }
  })
  output$factor1 <- renderPlotly({
    average_likes<-trunc(mean(GBvideos$likes))
    average_likes
    
    nrow(GBvideos[GBvideos$channel_title == "James Charles",])
    
    #create dataframe from GBvideos where channel title = James Charles

    final<-GBvideos[GBvideos$channel_title == "James Charles",]
    
    #add column to final dataframe
    final$averagelikes = average_likes
    
    final$publish_time <- as.Date(final$publish_time)
    
    p <- plot_ly(final, x = ~publish_time, y = ~average_likes, name = 'Average Likes', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
      add_trace(y = ~likes, name = 'Likes', mode = 'lines+markers',line = list(color = 'rgb(22, 96, 167)', width = 4))
    p
  })
  output$plot_views_likes <- renderPlot({
       
    ggplot(Allvideos, aes(views, likes, colour = factor(category_id)))+ geom_point()+
      xlab("Views")+ ylab("Likes")+
      ggtitle("Relation between views and likes for trending\n videos ")+
      geom_smooth(method=lm,color="darkred")
  })
  
  output$plot_viewsdislikes <- renderPlot({
    
      ggplot(Allvideos, aes(views, dislikes,colour = factor(category_id)))+ geom_point() +
        xlab("Views per region")+ ylab("Dislikes per region")+
        ggtitle("Relation between views and dislikes")+
      geom_smooth(method=lm,color="darkred")
  })
  
  output$plot_views_comments <- renderPlot({
    
      ggplot(Allvideos, aes(views, comment_count,colour = factor(category_id)))+ geom_point() +
        xlab("Views per region")+ ylab("Comment count per region")+
        ggtitle("Relation between views and comment count")+
      geom_smooth(method=lm,color="darkred")
  
  })
  output$plot_correlation <- renderPlot({
    
    corrplot.mixed(corr = cor(Allvideos[,c("views","likes","dislikes","comment_count"),with=F]))
    
    
  })
  
}

shinyApp(ui, server)

