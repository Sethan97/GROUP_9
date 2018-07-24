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
library(lubridate)
library(ggExtra)
library(SnowballC)
library(wordcloud)
library(syuzhet)
library(devtools)
library(igraph)
library(anytime)
library(SocialMediaMineR)
library(SocialMediaLab)
library(RColorBrewer)
library(base)
library(tidyr)
library(scales)
library(reshape2)
library(reshape)
library(plotrix)

#import datasets
CAvideos <- read.csv("~/CAvideos.csv")
FRvideos <- read.csv("~/FRvideos.csv")
GBvideos <- read.csv("~/GBvideos.csv")
USvideos <- read.csv("~/USvideos.csv")
DEvideos <- read.csv("~/DEvideos.csv")
#merge datasets
Allvideos <- as.data.table(rbind(GBvideos,CAvideos,USvideos,DEvideos,FRvideos))

#ui
ui <- dashboardPage(skin="red",
  dashboardHeader(title = "Analysis"),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  
  dashboardBody(
    tabItems(
      #tabItem home page
      tabItem("home",
              
              fluidRow(
              box(
              title = "YouTube Trending Analysis System",
              width = 8, solidHeader = TRUE,status="danger",
              br(),
              tags$img(src="youtube-logo.png",height=200,width=400,align="center"),
              p("YouTube is a popular video sharing website where content owners can share 
                videos and these videos can viewed by anyone who accesses youtube."),
              p("Viewers can react to these videos through likes,cooments and dislikes.Our proposed system
                system helps show concerned parties some of the key analysis concerning youtube videos"),
              p("These include:"),
              p(">Sentiment Analysis"),
              p(">Factors for popularity of youtube videos-why certain youtube videos are liked more than others"),
              p(">Time analysis - what is the usual time between trending date and publish times 
                per region,what times of the day are most trending videos?"),
              p(">General relationships-This shows the correlations between certain attributes of youtube videos
                e.g views and likes,views and dislikes etc"),
              p(">Regional Statistics- most liked categories,most disliked categories,top  
                liked categories,title wordclouds,tags wordclouds"),
              p(">General Statistics - most liked categories,most disliked categories,top  
                liked categories,title wordclouds,tags wordclouds")
              )
              )
      ),
      #tabItem sentiment
      tabItem("sentiment",
              sidebarPanel(
                
                width="2",
                # Input: Select the region
                radioButtons("Region1", "Select Region:",
                             c("Canada",
                               "USA",
                               "Great Britain",
                               "France",
                               "Germany")),
                br()
              ),
              
              h4("Visualizations"),
              tabBox(
                id = "tabset1", height = "500px",width="8",
                tabPanel("Liked videos",
                         "Most liked videos",
                         plotOutput("plot_likes")
                ),
                tabPanel("Disliked videos", "Most  disliked videos",plotOutput("plot_dislikes")),
                
                tabPanel("Ratio of likes to dislike", "A graph showing likes to dislikes ratio per category",plotOutput("plot_ratio"))
              ),
              box(
                width="2",height = "300px",h4("Key"),""
              )
      )
  ,
 #tabItem factors for popularity
  tabItem("factors",
          
          br(),
          h3("Factor 1:Are videos with more consistent communicators popular?"),
            fluidRow(
              box(
                width=8,height="500px",
                title="Sample Study - NiallHoranVEVO(Great Britain)",status="success",
                plotlyOutput("factor1"),verbatimTextOutput("event")

              ),
              box(width=4,
                  status="success",p(
                    class = "text-muted",
                    paste("From analyzing videos posted by NiallHoranVEVO across several months,we were able to deduce
                          if the channel posted  multiple videos in a short time frame,these videos had a higher
                          chance of being liked compared to when few videos were posted."
                    ))
            )
          ),
          br(),
          h3("Factor 2:Does category of a video affect it's popularity?"),
          fluidRow(
          box(width=8,
              h5("Using USA as a sampled region;"),
              p("Percentage of videos above 5k likes vs categories"),
         plotOutput("factor2")
          ),
         box(width=4,
             status="success",p(
               class = "text-muted",
               paste("Based on the data analysed,we concluded that in the USA,42% of videos in category
                     24 were above 5k likes which was significantly higher than other categories
                     e.g. category 10 where 18% of videos were above 5k likes."
               ))
               )
         )
  ),
 tabItem("time",
         h3("How long does it take a  video to trend
            from the time it was published?(days)"),
         br(),
         sidebarPanel(
           width="3",
           radioButtons("Region", "Select Region:",
                        c("Canada",
                          "France",
                          "Germany",
                          "Great Britain",
                          "USA",
                          "All")),
           br()
           
         ),
         box(
           width=9,height="500px",status="primary",
           title="Time between published and trending",
           plotOutput("plot_time1")
         )
         ,
         h3("What times are videos that take a day
            or less to trend published across all regions?"),
         fluidRow(
           sidebarPanel(
             width="3",
             radioButtons("Region4", "Select Region:",
                          c("Canada",
                            "France",
                            "Germany",
                            "Great Britain",
                            "USA"
                            )),
             br()
             
           ),
           box(
             width=9,height="500px",status="primary",
             plotOutput("plot_time2")
           )
         ),
         fluidRow(
           box(
             width = 6, background = "light-blue",
             p("From the first visualization potrayed above,
               we have come with the following conclusions,"
             ),
             p(">In Germany and France,it takes little time 
               to reach the trending date,1 or 2days")
             ,p(">It is highly unlikely 
                that videos trend the same day they were published"),
             p(">In Great Britain,most videos 
               take between 4 - 10 days to trend"),
             p(">In USA,most videos take
               between 4 - 6 days to trend"),
             p(">Across all regions,videos usually take one day to trend")),
           box(
             width=6,background="light-blue",
             p("From the second visualization,we see that:"),
             p(">In Canada ,Germanyand France,videos posted at between
           16:00hrs-18:00 hrs have a high chance of trending "),
             p(">In USA,videos posted at between 16:00hrs-19:00
               hrs have a high chance of trending "),
             p(">In Great Britain,videos posted at 17:00hrs
               hrs have a high chance of trending "),
             p(">Thus from this,we recommend a video to be posted
               between 16:00 and 18:00hrs to have a
               high chance of trending in a day or less")
           )
           )
 ),
 #tabItem general relationships
 tabItem("relation",
         br(),
         selectInput("visual3", "Choose Relationship you would wish to see:",
                     list(
                       'Views vs Likes',
                       'Views vs Dislikes',
                       'Views vs Comments',
                       'Likes vs Dislikes',
                       'Likes vs Comments',
                       'Dislikes vs Comments'
                     )),
         fluidRow(
           box(width=6,title = "Visualization",
               status="primary",plotOutput("plot_relation")),
           box(width=6,title = "Correlation between views
               ,likes,dislikes and comment count",
               status="primary",plotOutput("plot_correlation"))
         ),
         br(),
         
         fluidRow
         (
           box(
             width=6,height="300px",title="Key",""
           ),
           box(
             width=6,"From the graphs above,we see views and likes have a high correlation
             ,unlike views and comment count and views and dislikes that have low correlations"
           )
         )
 ),
 #tab item regional statistics
 tabItem("regional",
         
           selectInput("Region2", "Choose a Region:",
                       list('Canada',
                            'France',
                            'Germany',
                            'Great Britain',
                            'USA'
                            )
           ),
           selectInput("visual", "Choose Visualization you would like:",
                       list(
                            'Title wordcloud',
                            'Tags wordcloud',
                            'Most commented on categories'
                       )
           ),
         br(),
         box(
           width=8,plotOutput("plotregional")
         )
  ),
 tabItem("general",
         selectInput("visual2", "Choose Visualization you would like:",
                     list(
                       'Title wordcloud',
                       'Tags wordcloud',
                       'Most commented on categories',
                       'Youtube upload per country'
                     )
         ),
         br(),
         box(
           width=8,plotOutput("plotgeneral")
         )
         ))
))

server <- function(input, output) {
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Home", tabName="home",icon = icon("home")),
      menuItem("Sentiment Analysis",tabName="sentiment"),
      menuItem("Factors for popularity",tabName="factors"),
      menuItem("Time analysis",tabName="time"),
      menuItem(" General Relationships",tabName="relation"),
      menuItem(" Regional Statistics",tabName="regional"),
      menuItem(" General Statistics",tabName="general")
    )
  })
  
  output$plot_ratio <- renderPlot({
    
    if(input$Region1=="Canada"){
      second <- CAvideos %>% select(category_id, likes, dislikes) %>% mutate(category_id = as.factor(category_id))
      second <- aggregate(. ~category_id, second,  sum)
      second <- second %>% mutate(total = likes+dislikes)
      second <- second %>% mutate(likes.ratio = likes/total) %>% mutate(dislikes.ratio = dislikes/total)
      second <- subset(second, select = c(category_id,likes.ratio, dislikes.ratio))
      third <- melt(second, id.vars = 'category_id')
      third %>% ggplot(aes(x =as.factor(category_id), y = value, fill = variable))+geom_bar(stat = 'identity')
      
    }
    
    else if(input$Region1=="USA"){
      second <- USvideos %>% select(category_id, likes, dislikes) %>% mutate(category_id = as.factor(category_id))
      second <- aggregate(. ~category_id, second,  sum)
      second <- second %>% mutate(total = likes+dislikes)
      second <- second %>% mutate(likes.ratio = likes/total) %>% mutate(dislikes.ratio = dislikes/total)
      second <- subset(second, select = c(category_id,likes.ratio, dislikes.ratio))
      third <- melt(second, id.vars = 'category_id')
      third %>% ggplot(aes(x =as.factor(category_id), y = value, fill = variable))+geom_bar(stat = 'identity')
      
    }
    
    else if(input$Region1=="Great Britain"){
      second <- GBvideos %>% select(category_id, likes, dislikes) %>% mutate(category_id = as.factor(category_id))
      second <- aggregate(. ~category_id, second,  sum)
      second <- second %>% mutate(total = likes+dislikes)
      second <- second %>% mutate(likes.ratio = likes/total) %>% mutate(dislikes.ratio = dislikes/total)
      second <- subset(second, select = c(category_id,likes.ratio, dislikes.ratio))
      third <- melt(second, id.vars = 'category_id')
      third %>% ggplot(aes(x =as.factor(category_id), y = value, fill = variable))+geom_bar(stat = 'identity')
      
    }
    
    else if(input$Region1=="France"){
      second <- FRvideos %>% select(category_id, likes, dislikes) %>% mutate(category_id = as.factor(category_id))
      second <- aggregate(. ~category_id, second,  sum)
      second <- second %>% mutate(total = likes+dislikes)
      second <- second %>% mutate(likes.ratio = likes/total) %>% mutate(dislikes.ratio = dislikes/total)
      second <- subset(second, select = c(category_id,likes.ratio, dislikes.ratio))
      third <- melt(second, id.vars = 'category_id')
      third %>% ggplot(aes(x =as.factor(category_id), y = value, fill = variable))+geom_bar(stat = 'identity')
      
    }
    
    else if(input$Region1=="Germany"){
      second <- DEvideos %>% select(category_id, likes, dislikes) %>% mutate(category_id = as.factor(category_id))
      second <- aggregate(. ~category_id, second,  sum)
      second <- second %>% mutate(total = likes+dislikes)
      second <- second %>% mutate(likes.ratio = likes/total) %>% mutate(dislikes.ratio = dislikes/total)
      second <- subset(second, select = c(category_id,likes.ratio, dislikes.ratio))
      third <- melt(second, id.vars = 'category_id')
      third %>% ggplot(aes(x =as.factor(category_id), y = value, fill = variable))+geom_bar(stat = 'identity')
      
    }
  })
  
  
  output$plot_likes <- renderPlot({
    if(input$Region1=="Canada"){
      first <- CAvideos %>% select(category_id, likes, dislikes)%>% mutate(category_id = as.factor(category_id))
      first <- aggregate(. ~category_id,first,mean )
      
      
      first %>% ggplot(aes(x = category_id, y = likes))+geom_col()+labs(x = "category id", y = "Average Number of likes")
    }
    
    else if(input$Region1=="USA"){
      first <- USvideos %>% select(category_id, likes, dislikes)%>% mutate(category_id = as.factor(category_id))
      first <- aggregate(. ~category_id,first,mean )
      
      
      first %>% ggplot(aes(x = category_id, y = likes))+geom_col()+labs(x = "category id", y = "Average Number of likes")
    }
    
    else if(input$Region1=="Great Britain"){
      first <- GBvideos %>% select(category_id, likes, dislikes)%>% mutate(category_id = as.factor(category_id))
      first <- aggregate(. ~category_id,first,mean )
      
      
      first %>% ggplot(aes(x = category_id, y = likes))+geom_col()+labs(x = "category id", y = "Average Number of likes")
    }
    
    else if(input$Region1=="France"){
      first <- FRvideos %>% select(category_id, likes, dislikes)%>% mutate(category_id = as.factor(category_id))
      first <- aggregate(. ~category_id,first,mean )
      
      
      first %>% ggplot(aes(x = category_id, y = likes))+geom_col()+labs(x = "category id", y = "Average Number of likes")
    }
    
    else if(input$Region1=="Germany"){
      first <- DEvideos %>% select(category_id, likes, dislikes)%>% mutate(category_id = as.factor(category_id))
      first <- aggregate(. ~category_id,first,mean )
      
      
      first %>% ggplot(aes(x = category_id, y = likes))+geom_col()+labs(x = "category id", y = "Average Number of likes")
    }
    
    
  })
  
  
  output$plot_dislikes <- renderPlot({
    if(input$Region1=="Canada"){
      first <- CAvideos %>% select(category_id, likes, dislikes)%>% mutate(category_id = as.factor(category_id))
      first <- aggregate(. ~category_id,first,mean )
      
      first %>% ggplot(aes(x = category_id, y = dislikes))+geom_col()
      
    }
    
    else if(input$Region1=="USA"){
      first <- USvideos %>% select(category_id, likes, dislikes)%>% mutate(category_id = as.factor(category_id))
      first <- aggregate(. ~category_id,first,mean )
      
      first %>% ggplot(aes(x = category_id, y = dislikes))+geom_col()
      
    }
    
    else if(input$Region1=="Great Britain"){
      first <- GBvideos %>% select(category_id, likes, dislikes)%>% mutate(category_id = as.factor(category_id))
      first <- aggregate(. ~category_id,first,mean )
      
      first %>% ggplot(aes(x = category_id, y = dislikes))+geom_col()
      
    }
    
    else if(input$Region1=="France"){
      first <- FRvideos %>% select(category_id, likes, dislikes)%>% mutate(category_id = as.factor(category_id))
      first <- aggregate(. ~category_id,first,mean )
      
      first %>% ggplot(aes(x = category_id, y = dislikes))+geom_col()
      
    }
    
    else if(input$Region1=="Germany"){
      first <- DEvideos %>% select(category_id, likes, dislikes)%>% mutate(category_id = as.factor(category_id))
      first <- aggregate(. ~category_id,first,mean )
      
      first %>% ggplot(aes(x = category_id, y = dislikes))+geom_col()
      
    }
    
    
  })
  
  output$factor1 <- renderPlotly({
    average_likes<-trunc(mean(GBvideos$likes))
    average_likes
    
    nrow(GBvideos[GBvideos$channel_title == "NiallHoranVEVO",])
    
    #create dataframe from GBvideos where channel title = Soccer AM
    
    
    final<-GBvideos[GBvideos$channel_title == "NiallHoranVEVO",]
    
    #add column to final dataframe
    final$averagelikes = average_likes
    
    final$publish_time <- as.Date(final$publish_time)
    
    p <- plot_ly(final, x = ~publish_time, y = ~likes, type = 'scatter', mode = 'lines')
    p
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  
  output$factor2 <- renderPlot({
    plotj <- ggplot(USvideos, aes(as.factor(category_id))) + 
      geom_bar(aes(y =((likes > 5000)/likes), fill = factor(category_id)), stat="identity") + 
      scale_y_continuous(labels=scales::percent) +
      ylab("Percentage of videos above 5k likes")+ xlab("Categories") 
    
    plotj
  })
 
  output$plot_time1  <- renderPlot({
    if(input$Region=="Canada")
    {
    CAvideos <- as.data.table(CAvideos)
    CAvideos$trending_date <-ydm(CAvideos$trending_date)
    CAvideos$publish_time <- as.Date(CAvideos$publish_time)

    CAvideos$difference = CAvideos$trending_date - CAvideos$publish_time
    
    ggplot(CAvideos[difference<10],
           aes(as.factor(difference),fill=as.factor(difference)))+
      geom_bar()+guides(fill="none")+
      labs(title=" Time between published and trending in days"
      )+xlab("Number of days")+ylab("Number of videos")
    }
    else if(input$Region=="USA")
    {
      USvideos <- as.data.table(USvideos)
      USvideos$trending_date <-ydm(USvideos$trending_date)
      USvideos$publish_time <- as.Date(USvideos$publish_time)
      
      USvideos$difference = USvideos$trending_date - USvideos$publish_time
      
      ggplot(USvideos[difference<25],
             aes(as.factor(difference),fill=as.factor(difference)))+
        geom_bar()+guides(fill="none")+
        labs(title=" Time between published and trending in days"
        )+xlab("Number of days")+ylab("Number of videos") 
    }
    else if(input$Region=="Great Britain")
    {
      GBvideos <- as.data.table(GBvideos)
      GBvideos$trending_date <-ydm(GBvideos$trending_date)
      GBvideos$publish_time <- as.Date(GBvideos$publish_time)
      
      GBvideos$difference = GBvideos$trending_date - GBvideos$publish_time
      
      ggplot(GBvideos[difference<25],
             aes(as.factor(difference),fill=as.factor(difference)))+
        geom_bar()+guides(fill="none")+
        labs(title=" Time between published and trending in days"
        )+xlab("Number of days")+ylab("Number of videos")
    }
    else if(input$Region=="France")
    {
      FRvideos <- as.data.table(FRvideos)
      FRvideos$trending_date <-ydm(FRvideos$trending_date)
      FRvideos$publish_time <- as.Date(FRvideos$publish_time)
      
     FRvideos$difference = FRvideos$trending_date - FRvideos$publish_time
      
      ggplot(FRvideos[difference<10],
             aes(as.factor(difference),fill=as.factor(difference)))+
        geom_bar()+guides(fill="none")+
        labs(title=" Time between published and trending in days"
        )+xlab("Number of days")+ylab("Number of videos")
    }
    else if(input$Region=="Germany")
    {
      DEvideos <- as.data.table(DEvideos)
      DEvideos$trending_date <-ydm(DEvideos$trending_date)
      DEvideos$publish_time <- as.Date(DEvideos$publish_time)
      
      DEvideos$difference = DEvideos$trending_date - DEvideos$publish_time
      
      ggplot(DEvideos[difference<10],
             aes(as.factor(difference),fill=as.factor(difference)))+
        geom_bar()+guides(fill="none")+
        labs(title=" Time between published and trending in days"
        )+xlab("Number of days")+ylab("Number of videos") 
    }
    else
    {
      Allvideos <- as.data.table(Allvideos)
      Allvideos$trending_date <-ydm(Allvideos$trending_date)
      Allvideos$publish_time <- as.Date(Allvideos$publish_time)
      
      Allvideos$difference = Allvideos$trending_date - Allvideos$publish_time
      
      ggplot(Allvideos[difference<10],
             aes(as.factor(difference),fill=as.factor(difference)))+
        geom_bar()+guides(fill="none")+
        labs(title=" Time between published and trending in days"
        )+xlab("Number of days")+ylab("Number of videos") 
    }
    
  }
  
  )
  output$plot_time2<-renderPlot(
    {
      if(input$Region4=="Canada")
        {
        CAvideos %>% select(category_id,likes,trending_date,publish_time)%>%
          mutate(trending_date = ydm(CAvideos$trending_date))%>%
          mutate(category_id = as.factor(CAvideos$category_id))%>%
          mutate(publish_time = anytime(as.factor(CAvideos$publish_time)))%>%
          mutate(difference=difftime(trending_date,publish_time,units=c("days")))%>%
          filter(difference < 2)%>%
          mutate(difference = as.integer(difference))%>%
          mutate(difference = as.factor(difference))%>%
          mutate(publish_time= hour(publish_time))%>%
          ggplot(aes(publish_time,fill=as.factor(publish_time)))+geom_bar()+
          guides(fill="none")+
          labs(title="Publish times of videos that have trending times of a day or less"
          )+xlab("Hours(24 hour clock)")+ylab("Number of videos")
        }
     else if(input$Region4=="France")
      {
        FRvideos %>% select(category_id,likes,trending_date,publish_time)%>%
          mutate(trending_date = ydm(FRvideos$trending_date))%>%
          mutate(category_id = as.factor(FRvideos$category_id))%>%
          mutate(publish_time = anytime(as.factor(FRvideos$publish_time)))%>%
          mutate(difference=difftime(trending_date,publish_time,units=c("days")))%>%
          filter(difference < 2)%>%
          mutate(difference = as.integer(difference))%>%
          mutate(difference = as.factor(difference))%>%
          mutate(publish_time= hour(publish_time))%>%
          ggplot(aes(publish_time,fill=as.factor(publish_time)))+geom_bar()+
          guides(fill="none")+
          labs(title="Publish times of videos that have trending times of a day or less"
          )+xlab("Hours(24 hour clock)")+ylab("Number of videos")
      }
      else if(input$Region4=="Germany")
      {
          DEvideos %>% select(category_id,likes,trending_date,publish_time)%>%
          mutate(trending_date = ydm(DEvideos$trending_date))%>%
          mutate(category_id = as.factor(DEvideos$category_id))%>%
          mutate(publish_time = anytime(as.factor(DEvideos$publish_time)))%>%
          mutate(difference=difftime(trending_date,publish_time,units=c("days")))%>%
          filter(difference < 2)%>%
          mutate(difference = as.integer(difference))%>%
          mutate(difference = as.factor(difference))%>%
          mutate(publish_time= hour(publish_time))%>%
          ggplot(aes(publish_time,fill=as.factor(publish_time)))+geom_bar()+
          guides(fill="none")+
          labs(title="Publish times of videos that have trending times of a day or less"
          )+xlab("Hours(24 hour clock)")+ylab("Number of videos")
      }
      else if(input$Region4=="Great Britain")
      {
          GBvideos %>% select(category_id,likes,trending_date,publish_time)%>%
          mutate(trending_date = ydm(GBvideos$trending_date))%>%
          mutate(category_id = as.factor(GBvideos$category_id))%>%
          mutate(publish_time = anytime(as.factor(GBvideos$publish_time)))%>%
          mutate(difference=difftime(trending_date,publish_time,units=c("days")))%>%
          filter(difference < 2)%>%
          mutate(difference = as.integer(difference))%>%
          mutate(difference = as.factor(difference))%>%
          mutate(publish_time= hour(publish_time))%>%
          ggplot(aes(publish_time,fill=as.factor(publish_time)))+geom_bar()+
          guides(fill="none")+
          labs(title="Publish times of videos that have trending times of a day or less"
          )+xlab("Hours(24 hour clock)")+ylab("Number of videos")
      }
      else
      {
          USvideos %>% select(category_id,likes,trending_date,publish_time)%>%
          mutate(trending_date = ydm(USvideos$trending_date))%>%
          mutate(category_id = as.factor(USvideos$category_id))%>%
          mutate(publish_time = anytime(as.factor(USvideos$publish_time)))%>%
          mutate(difference=difftime(trending_date,publish_time,units=c("days")))%>%
          filter(difference < 2)%>%
          mutate(difference = as.integer(difference))%>%
          mutate(difference = as.factor(difference))%>%
          mutate(publish_time= hour(publish_time))%>%
          ggplot(aes(publish_time,fill=as.factor(publish_time)))+geom_bar()+
          guides(fill="none")+
          labs(title="Publish times of videos that have trending times of a day or less"
          )+xlab("Hours(24 hour clock)")+ylab("Number of videos")
        
      }
      
    })
  output$plot_relation <- renderPlot({
    
    if(input$visual3=="Views vs Likes")
    {
      ggplot(Allvideos, aes(views, likes, colour = factor(category_id)))+ geom_point()+
        xlab("Views")+ ylab("Likes")+
        ggtitle("Relation between Views and Likes")+
        geom_smooth(method=lm,color="darkred") 
    }
    else if(input$visual3=="Views vs Dislikes")
    {
      ggplot(Allvideos, aes(views, dislikes,colour = factor(category_id)))+ geom_point() +
        xlab("Views")+ ylab("Dislikes")+
        ggtitle("Relationship between Views and Dislikes")+
        geom_smooth(method=lm,color="darkred") 
    }
    else if(input$visual3=="Views vs Comments")
    {
      ggplot(Allvideos, aes(views, comment_count,colour = factor(category_id)))+ geom_point() +
        xlab("Views")+ ylab("Comment count")+
        ggtitle("Relation between Views and Comment count")+
        geom_smooth(method=lm,color="darkred")
    }
    else if(input$visual3=="Likes vs Dislikes")
    {
      ggplot(Allvideos, aes(likes,dislikes,colour = factor(category_id)))+ geom_point() +
        xlab("Likes")+ ylab("Dislikes")+
        ggtitle("Relation between likes and dislikes")+
        geom_smooth(method=lm,color="darkred")
    }
    else if(input$visual3=="Likes vs Comments")
    {
      ggplot(Allvideos, aes(likes,comment_count,colour = factor(category_id)))+ geom_point() +
        xlab("Likes")+ ylab("Comment  count")+
        ggtitle("Relation between likes and comment count")+
        geom_smooth(method=lm,color="darkred")
    }
    else
    {
      ggplot(Allvideos, aes(dislikes,comment_count,colour = factor(category_id)))+ geom_point() +
        xlab("Disikes")+ ylab("Comment  count")+
        ggtitle("Relation between dislikes and comment count")+
        geom_smooth(method=lm,color="darkred") 
    }
    
    
  })
  output$plot_correlation <- renderPlot({
    
    corrplot.mixed(corr = cor(Allvideos[,c("views","likes","dislikes","comment_count"),with=F]))
    
    
  })
  
  output$plotregional<- renderPlot(
    {
     if(input$Region2=="Canada")
     {
       if(input$visual=="Title wordcloud"){
         
         CAvideos$title<-gsub('[^a-zA-Z0-9.]'," ", CAvideos$title)
         
         Encoding(CAvideos$title) <- "latin1"
         trial1 <- Corpus(VectorSource(iconv(CAvideos$title, "latin1", "ASCII", sub="")))
         toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
         trial1 <- tm_map(trial1, toSpace, "/")
         trial1 <- tm_map(trial1, toSpace, "@")
         trial1 <- tm_map(trial1, toSpace, "\\|")
         trial1 <- tm_map(trial1, removePunctuation)
         trial1 = tm_map(trial1, content_transformer(tolower))
         trial1 <- tm_map(trial1, removeNumbers)
         trial1 <- tm_map(trial1, stripWhitespace)
         trial1 <- tm_map(trial1, removeWords, stopwords('english'))
         trial1 <- tm_map(trial1, stemDocument)
         
         wordcloud(trial1, scale=c(5,0.5),
                   max.words=100, random.order=FALSE,
                   rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
         
       }
      else if(input$visual=="Tags wordcloud")
      {
        
        CAvideos$tags<-gsub('[^a-zA-Z0-9.]'," ", CAvideos$tags)
        
        Encoding(CAvideos$tags) <- "latin1"
    trial <- Corpus(VectorSource(iconv(CAvideos$tags, "latin1", "ASCII", sub="")))
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
        trial <- tm_map(trial, toSpace, "/")
        trial <- tm_map(trial, toSpace, "@")
        trial <- tm_map(trial, toSpace, "\\|")
        trial <- tm_map(trial, removePunctuation)
        trial = tm_map(trial, content_transformer(tolower))
        trial <- tm_map(trial, removeNumbers)
        trial <- tm_map(trial, stripWhitespace)
        trial <- tm_map(trial, removeWords, stopwords('english'))
        trial <- tm_map(trial, stemDocument)
        
        wordcloud(trial, scale=c(5,0.5),
                  max.words=100, random.order=FALSE,
                  rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
      }
       else if(input$visual=="Most commented on categories")
       {
         CAvideos %>% select(category_id,comment_count) %>% group_by(as.factor(category_id)) %>%
           ggplot(aes(x = as.factor(category_id),fill=as.factor(category_id)))+geom_bar()+xlab("Category Id")+ylab("Comment count")
       }
       
     }
     else if(input$Region2=="France") 
     {
       if(input$visual=="Title wordcloud"){
         
         FRvideos$title<-gsub('[^a-zA-Z0-9.]'," ", FRvideos$title)
         
         Encoding(FRvideos$title) <- "latin1"
         trial <- Corpus(VectorSource(iconv(FRvideos$tags, "latin1", "ASCII", sub="")))
         toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
         trial <- tm_map(trial, toSpace, "/")
         trial <- tm_map(trial, toSpace, "@")
         trial <- tm_map(trial, toSpace, "\\|")
         trial <- tm_map(trial, removePunctuation)
         trial = tm_map(trial, content_transformer(tolower))
         trial <- tm_map(trial, removeNumbers)
         trial <- tm_map(trial, stripWhitespace)
         trial <- tm_map(trial, removeWords, stopwords('english'))
         trial <- tm_map(trial, removeWords, stopwords('french'))
         trial <- tm_map(trial, stemDocument)
         
         wordcloud(trial, scale=c(5,0.5),
                   max.words=100, random.order=FALSE,
                   rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
         
       }
       else if(input$visual=="Tags wordcloud")
       {
         
         FRvideos$tags<-gsub('[^a-zA-Z0-9.]'," ", FRvideos$tags)
         
         Encoding(FRvideos$tags) <- "latin1"
         tria1l <- Corpus(VectorSource(iconv(FRvideos$tags, "latin1", "ASCII", sub="")))
         toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
         trial <- tm_map(trial1, toSpace, "/")
         trial1 <- tm_map(trial1, toSpace, "@")
         trial1 <- tm_map(trial1, toSpace, "\\|")
         trial1 <- tm_map(trial1, removePunctuation)
         trial1 = tm_map(trial1, content_transformer(tolower))
         trial1 <- tm_map(trial1, removeNumbers)
         trial1 <- tm_map(trial1, stripWhitespace)
         trial1 <- tm_map(trial1, removeWords, stopwords('english'))
         trial1 <- tm_map(trial1, removeWords, stopwords('french'))
         trial1 <- tm_map(trial1, stemDocument)
         
         wordcloud(trial1, scale=c(5,0.5),
                   max.words=100, random.order=FALSE,
                   rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
       } 
       else if(input$visual=="Most commented on categories")
       {
         FRvideos %>% select(category_id,comment_count) %>% group_by(as.factor(category_id)) %>%
           ggplot(aes(x = as.factor(category_id),fill=as.factor(category_id)))+geom_bar()+xlab("Category Id")+ylab("Comment count")
       }
     }
      else if(input$Region2=="Germany")
      {
        if(input$visual=="Title wordcloud")
        {
          DEvideos$title<-gsub('[^a-zA-Z0-9.]'," ", DEvideos$title)
          
          Encoding(DEvideos$title) <- "latin1"
          
          trial1 <- Corpus(VectorSource(iconv(DEvideos$title, "latin1","ASCII", sub="")))
          toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
          trial1 <- tm_map(trial1, toSpace, "/")
          trial1 <- tm_map(trial1, toSpace, "@")
          trial1 <- tm_map(trial1, toSpace, "\\|")
          trial1 <- tm_map(trial1, toSpace, "blm")
          trial1 <- tm_map(trial1, removePunctuation, preserve_intra_word_dashes=TRUE)
          trial1 = tm_map(trial1, content_transformer(tolower))
          trial1 <- tm_map(trial1, removeNumbers)
          trial1 <- tm_map(trial1, stripWhitespace)
          trial1 <- tm_map(trial1, removeWords, stopwords('german'))
          trial1<- tm_map(trial1, removeWords, stopwords('english'))
          trial1 <- tm_map(trial1, toSpace, "blm")
          trial1 <- tm_map(trial1, stemDocument)
          
          wordcloud(trial1, scale=c(5,0.5),
                    max.words=100, random.order=FALSE,
                    rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))  
        }
        else if(input$visual=="Tags wordcloud"){
          DEvideos$tags<-gsub('[^a-zA-Z0-9.]'," ", DEvideos$tags)
          
        Encoding(DEvideos$tags) <- "latin1"
      
        trial <- Corpus(VectorSource(iconv(DEvideos$tags, "latin1","ASCII", sub="")))
        toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
        trial <- tm_map(trial, toSpace, "/")
        trial <- tm_map(trial, toSpace, "@")
        trial <- tm_map(trial, toSpace, "\\|")
        trial <- tm_map(trial, toSpace, "blm")
        trial <- tm_map(trial, removePunctuation, preserve_intra_word_dashes=TRUE)
        trial = tm_map(trial, content_transformer(tolower))
        trial <- tm_map(trial, removeNumbers)
        trial <- tm_map(trial, stripWhitespace)
        trial <- tm_map(trial, removeWords, stopwords('german'))
        trial <- tm_map(trial, removeWords, stopwords('english'))
        trial <- tm_map(trial, toSpace, "blm")
        trial <- tm_map(trial, stemDocument)
        
        wordcloud(trial, scale=c(5,0.5),
                  max.words=100, random.order=FALSE,
                  rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
        }
        else if(input$visual=="Most commented on categories")
        {
          DEvideos %>% select(category_id,comment_count) %>% group_by(as.factor(category_id)) %>%
            ggplot(aes(x = as.factor(category_id),fill=as.factor(category_id)))+geom_bar()+xlab("Category Id")+ylab("Comment count")
        }
        
      }
      else if(input$Region2=="Great Britain")
      {
        if(input$visual=="Title wordcloud"){
          
          GBvideos$title<-gsub('[^a-zA-Z0-9.]'," ", GBvideos$title)
          
          Encoding(GBvideos$title) <- "latin1"
          trial1 <- Corpus(VectorSource(iconv(GBvideos$title, "latin1", "ASCII", sub="")))
          toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
          trial1 <- tm_map(trial1, toSpace, "/")
          trial1 <- tm_map(trial1, toSpace, "@")
          trial1 <- tm_map(trial1, toSpace, "\\|")
          trial1 <- tm_map(trial1, removePunctuation)
          trial1 = tm_map(trial1, content_transformer(tolower))
          trial1 <- tm_map(trial1, removeNumbers)
          trial1 <- tm_map(trial1, stripWhitespace)
          trial1 <- tm_map(trial1, removeWords, stopwords('english'))
          trial1 <- tm_map(trial1, stemDocument)
          
          wordcloud(trial1, scale=c(5,0.5),
                    max.words=100, random.order=FALSE,
                    rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
          
        }
        else if(input$visual=="Tags wordcloud")
        {
          
          GBvideos$tags<-gsub('[^a-zA-Z0-9.]'," ", GBvideos$tags)
          
          Encoding(GBvideos$tags) <- "latin1"
          trial <- Corpus(VectorSource(iconv(GBvideos$tags, "latin1", "ASCII", sub="")))
          toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
          trial <- tm_map(trial, toSpace, "/")
          trial <- tm_map(trial, toSpace, "@")
          trial <- tm_map(trial, toSpace, "\\|")
          trial <- tm_map(trial, removePunctuation)
          trial = tm_map(trial, content_transformer(tolower))
          trial <- tm_map(trial, removeNumbers)
          trial <- tm_map(trial, stripWhitespace)
          trial <- tm_map(trial, removeWords, stopwords('english'))
          trial <- tm_map(trial, stemDocument)
          
          wordcloud(trial, scale=c(5,0.5),
                    max.words=100, random.order=FALSE,
                    rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
        } 
        else if(input$visual=="Most commented on categories")
        {
          GBvideos %>% select(category_id,comment_count) %>% group_by(as.factor(category_id)) %>%
            ggplot(aes(x = as.factor(category_id),fill=as.factor(category_id)))+geom_bar()+xlab("Category Id")+ylab("Comment count")
        }
      }
      else
      {
        if(input$visual=="Title wordcloud"){
          
          USvideos$title<-gsub('[^a-zA-Z0-9.]'," ", USvideos$title)
          
          Encoding(USvideos$title) <- "latin1"
          trial1 <- Corpus(VectorSource(iconv(USvideos$title, "latin1", "ASCII", sub="")))
          toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
          trial1 <- tm_map(trial1, toSpace, "/")
          trial1 <- tm_map(trial1, toSpace, "@")
          trial1 <- tm_map(trial1, toSpace, "\\|")
          trial1 <- tm_map(trial1, removePunctuation)
          trial1 = tm_map(trial1, content_transformer(tolower))
          trial1 <- tm_map(trial1, removeNumbers)
          trial1 <- tm_map(trial1, stripWhitespace)
          trial1 <- tm_map(trial1, removeWords, stopwords('english'))
          trial1 <- tm_map(trial1, stemDocument)
          
          wordcloud(trial1, scale=c(5,0.5),
                    max.words=100, random.order=FALSE,
                    rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
          
        }
        else if(input$visual=="Tags wordcloud")
        {
          
          USvideos$tags<-gsub('[^a-zA-Z0-9.]'," ", USvideos$tags)
          
          Encoding(USvideos$tags) <- "latin1"
          trial <- Corpus(VectorSource(iconv(USvideos$tags, "latin1", "ASCII", sub="")))
          toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
          trial <- tm_map(trial, toSpace, "/")
          trial <- tm_map(trial, toSpace, "@")
          trial <- tm_map(trial, toSpace, "\\|")
          trial <- tm_map(trial, removePunctuation)
          trial = tm_map(trial, content_transformer(tolower))
          trial <- tm_map(trial, removeNumbers)
          trial <- tm_map(trial, stripWhitespace)
          trial <- tm_map(trial, removeWords, stopwords('english'))
          trial <- tm_map(trial, stemDocument)
          
          wordcloud(trial, scale=c(5,0.5),
                    max.words=100, random.order=FALSE,
                    rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
        } 
        else if(input$visual=="Most commented on categories")
        {
          USvideos %>% select(category_id,comment_count) %>% group_by(as.factor(category_id)) %>%
            ggplot(aes(x = as.factor(category_id),fill=as.factor(category_id)))+geom_bar()+xlab("Category Id")+ylab("Comment count")
        }
      }
    })
  output$plotgeneral <- renderPlot({
    
    if(input$visual2=="Title wordcloud")
    {
      
      Allvideos$title<-gsub('[^a-zA-Z0-9.]'," ", Allvideos$title)
      
      Encoding(Allvideos$title) <- "latin1"
      trial_all <- Corpus(VectorSource(iconv(Allvideos$title, "latin1", "ASCII", sub="")))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      trial_all <- tm_map(trial_all, toSpace, "/")
      trial_all <- tm_map(trial_all, toSpace, "@")
      trial_all <- tm_map(trial_all, toSpace, "\\|")
      trial_all <- tm_map(trial_all, removePunctuation)
      trial_all = tm_map(trial_all, content_transformer(tolower))
      trial_all <- tm_map(trial_all, removeNumbers)
      trial_all <- tm_map(trial_all, stripWhitespace)
      trial_all <- tm_map(trial_all, removeWords, stopwords('german'))
      trial_all <- tm_map(trial_all, removeWords, stopwords('french'))
      trial_all <- tm_map(trial_all, removeWords, stopwords('english'))
      trial_all <- tm_map(trial_all, stemDocument)
      
      wordcloud(trial_all, scale=c(5,0.5),
                max.words=100, random.order=FALSE,
                rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
    } 
    else if(input$visual2=="Tags wordcloud")
    {
      
      Allvideos$tags<-gsub('[^a-zA-Z0-9.]'," ", Allvideos$tags)
      
      Encoding(Allvideos$tags) <- "latin1"
      trial_all1 <- Corpus(VectorSource(iconv(Allvideos$tags, "latin1", "ASCII", sub="")))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      trial_all1 <- tm_map(trial_all1, toSpace, "/")
      trial_all1<- tm_map(trial_all1, toSpace, "@")
      trial_all1 <- tm_map(trial_all1, toSpace, "\\|")
      trial_all1 <- tm_map(trial_all1, removePunctuation)
      trial_all1 = tm_map(trial_all1, content_transformer(tolower))
      trial_all1 <- tm_map(trial_all1, removeNumbers)
      trial_all1 <- tm_map(trial_all1, stripWhitespace)
      trial_all1 <- tm_map(trial_all1, removeWords, stopwords('german'))
      trial_all1 <- tm_map(trial_all1, removeWords, stopwords('french'))
      trial_all1 <- tm_map(trial_all1, removeWords, stopwords('english'))
      trial_all1 <- tm_map(trial_all1, stemDocument)
      
      wordcloud(trial_all1, scale=c(5,0.5),
                max.words=100, random.order=FALSE,
                rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
    } 
    else if(input$visual2=="Most commented on categories")
    {
      Allvideos %>% select(category_id,comment_count) %>% group_by(as.factor(category_id)) %>%
        ggplot(aes(x = as.factor(category_id),fill=as.factor(category_id)))+geom_bar()+xlab("Category Id")+ylab("Comment count")
    }
    else if(input$visual2=="Youtube upload per country")
    {
      slices <- c(nrow(CAvideos[,]), nrow(FRvideos[,]), nrow(DEvideos[,]),
                  nrow(GBvideos[,]), nrow(USvideos[,]))
      lbls <- c("Canada", "France", "Germany", "Great Britain","USA")
      pct <- round(slices/sum(slices)*100)
      pie3D(slices,labels=lbls,explode=0.1,
            main="Pie Chart showing youtube upload per country") 
    }
  })
  
}

shinyApp(ui, server)
