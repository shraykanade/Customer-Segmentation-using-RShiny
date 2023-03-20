library(shiny)
library(shinydashboard)
library(factoextra)
library(NbClust)
library(fpc)
library (ggplot2)
library(ggthemes)

library(cluster)
library(dplyr)
library(magrittr)
library(plotly)
library(ggbiplot)
library(data.table)
library(caret)
library(tidyr)
library(reshape2)
library(summarytools)



# Load data
df <- read.csv("Mall_Customers.csv")
df$Age <- as.numeric(df$Age)
df$Work_Experience <- as.numeric(df$Work_Experience)
df$Family_Size <- as.numeric(df$Family_Size)

#df =subset(df, select = -c(CustomerID))
df_new<-data.frame(df)
df_new$Graduated<-ifelse( df_new$Graduated=="Yes",1,0)  
df_new$Ever_Married<-ifelse( df_new$Ever_Married=="Yes",1,0)  
df_new$Gender<-ifelse( df_new$Gender=="Male",1,0)  

dmy <- dummyVars(~ Profession, data = df_new)
dmy_data <- data.frame(predict(dmy, newdata = df_new))
df1<-data.frame(df_new)
df1 <- cbind(df_new,dmy_data)
df1 = subset(df1, select = -c(Profession,CustomerID))
vars<- names(df1)


ui <- shinyUI(
  dashboardPage(
    dashboardHeader(title ="Customer Segmentation"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Raw Data",tabName = "raw",icon=icon("table")),
        menuItem("EA - Data Summary",tabName = "summary",icon=icon("lock-open",lib="font-awesome")),
        menuItem("EA - Plots",tabName = "plots"),
        menuItem("Determine clusters for K means",tabName = "clusters",icon=icon("chart-area",lib="font-awesome")),
        menuItem("K-means Clustering",tabName= "kmeans",icon=icon("users",lib="font-awesome")),
        menuItem("Hierarchical Clustering",tabName = "hclust",icon=icon("users",lib="font-awesome")),
        menuItem("DBScan Clustering",tabName = "dbclust",icon=icon("users",lib="font-awesome"))
        
      )),
    dashboardBody(
      tabItems(
        tabItem(tabName = "plots",h1("Exploratory Analysis - Plots"),fluidRow(
          box(plotOutput("box_plot")),
          box(plotOutput("Age_histogram")),
          
        ),
        fluidRow(
          box(plotOutput("gender_histogram")),
          box(plotOutput("Married_histogram"))
        ),
        
        fluidRow(
          box(plotOutput("spending_histogram")),
          box(plotOutput("prof_histogram")),
          
        )
        ),
        tabItem(tabName = "raw",h1("Customer Data"),fluidRow(column(5,tableOutput("rawdata")))),
        
        tabItem(tabName = "summary",h1("Data Summary"),
                fluidRow(
                  box(htmlOutput("summary1"))
                ),
                fluidRow(
                  
                  selectInput(inputId ="summarry_column",
                              label = "Choose feature for Summary",
                              choices = names(df),
                              selected = "Work_Experience"),
                  
                  tableOutput('table_sum')
                )),
        tabItem(tabName = "clusters",h1("Number of Clusters"),
                fluidRow(
                  box(h2("Elbow Method"),plotOutput("method1")),
                  box(h2("Average silhouette method"),plotOutput("method2"))),
                  h1("K-means clustering"),
                fluidRow(
                  box(plotOutput("clusterchart")),
                  box(sliderInput("clustnum","Number of clusters",1,10,6))
                ),
                h2("K-means Statistics"),
                fluidRow(
                  box(h3("Cluster Statistics"),htmlOutput("stat1")),
                  box(h3("Dunn Index"),htmlOutput("stat2"))
                )),
        tabItem(tabName = "kmeans",h1("K Means"),
                fluidRow(
                  sidebarPanel(
                  selectInput('xcol', 'X Variable', vars),
                  selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
                  numericInput('clusters', 'Cluster count', 3, min = 1, max = 9))
                  ),
                fluidRow(
                  h2("Result:"),
                  plotOutput('kmeans_plot')
                  )),
        tabItem(tabName = "hclust",h1("Hierarchical Clustering"),
                fluidRow(
                
                         selectInput("hclustMethod", label="method", choices=list(
                           "single"="single","complete"="complete","average"="average"),selected = "complete"),
                         selectInput("metric", label="distance", choices=list(
                           "euclidian"="euclidian","maximum"="maximum","manhattan"="manhattan","minkowski"="minkowski"),selected = "euclidian"),
                         numericInput("splitTreeAt","split tree at",value=55,min=0,max=100,step=1)),
                
                fluidRow(
                  h2("Result:"),
                  plotOutput("treePlot")
                )),
        
        tabItem(tabName = "dbclust",h1("DBScan Clustering"),
                fluidRow(
                  
                  
                  numericInput("threshold","Threshold of distance",value=0.4,min=0,max=1),
                  numericInput("minpts","Minimum Data points",value=3,min=0,max=100)),
                
                fluidRow(
                  h2("Result:"),
                  plotOutput("dbPlot")
                ))

      ))))    


# Shiny Server
server <- shinyServer(function(input,output){
  meltData <- melt(df)
  box_plotv <- ggplot(meltData, aes(factor(variable), value)) 
  gender_count<-df %>% count('Gender')
  marriage_count<-df %>% count('Ever_Married')
  df_Prof_spending_score<-setNames(aggregate(df$Profession, by=list(df$Profession,df$Spending_Score), FUN=length), c("Profession","Spending_Score","freq"))
  df_marr_spendingscore<-setNames(aggregate(df$Ever_Married, by=list(df$Ever_Married,df$Spending_Score), FUN=length), c("Ever_Married","Spending_Score","freq"))
  spend_score<-df %>% count('Spending_Score')
  prof_count<-df %>% count('Profession')
  
  
  selectedData <- reactive({
    df1[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
    output$kmeans_plot <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
    
    
    
    dt_f <- reactive({
      freq(df[,c(input$summarry_column)], plain.ascii = FALSE, style = "rmarkdown")
      
    })
    
    
    
  
  
    h_cluster <- reactive({
     hclust(dist(df1, method=input$metric), method = input$hclustMethod)
    })
    

    output$treePlot <- renderPlot({
      h_cluster <- hclust(dist(df1, method=input$metric), method = input$hclustMethod)
      plot(h_cluster)
      abline(h = input$splitTreeAt, col = "green")
    })
    
    db_cluster <- reactive({
      dbscan(df1, eps =input$threshold, MinPts = input$minpts)
    })
    

    output$dbPlot <- renderPlot({
      db_cluster <- dbscan(df1, eps =input$threshold, MinPts = input$minpts)
      data2 <- bind_cols(df1, cluster = factor(db_cluster$cluster))
      ggplot(data2) +
        geom_point(mapping = aes(x=Spending_Score, y = cluster, colour = cluster),
                   position = "jitter") +
        labs(title = "DBSCAN")
    })
    
  
    
  output$box_plot <- renderPlot({
    box_plotv + geom_boxplot() + facet_wrap(~variable, scale="free")
    
  })
  
  output$Age_histogram <- renderPlot({
    ggplot(df, aes(x=Age)) + 
      geom_histogram(color="white", fill="black",binwidth=2)
  })
  
  output$gender_histogram <- renderPlot({
    ggplot(data=gender_count, aes(x=Gender,y=freq)) +
      geom_bar(stat="identity",width=0.5)
  })
  
  output$Married_histogram <- renderPlot({
    ggplot(data=marriage_count, aes(x=Ever_Married,y=freq)) +
      geom_bar(stat="identity",width=0.5)
  })
  
  output$spending_histogram <- renderPlot({
    ggplot(data=spend_score, aes(x=Spending_Score,y=freq,color=Spending_Score)) +
      geom_bar(stat="identity",width=0.5,fill="white")
  })
  
  output$prof_histogram <- renderPlot({
    ggplot(data=prof_count, aes(x=Profession,y=freq,color=Profession)) +
      geom_bar(stat="identity",width=0.5,fill="white")
  })
  
  
  output$rawdata <- renderTable(df)
  output$summary1 <- renderPrint({summary(df)}) 
  output$table_sum <- renderTable(dt_f(),  rownames =true())
  
  
  output$method1 <- renderPlot({
    
    fviz_nbclust(df1, kmeans, method = "wss") +
      geom_vline(xintercept = 4, linetype = 2)+
      labs(subtitle = "Elbow method")
  })
  output$method2 <- renderPlot({
    fviz_nbclust(df1, kmeans, method = "silhouette") +
      labs(subtitle = "Silhouette Method")
  })
  
  output$clusterchart <- renderPlot({
    fviz_cluster((eclust(df1, "kmeans", k = input$clustnum, nstart = 25, graph = FALSE)), geom = "point", ellipse.type = "norm",
                 palette = "jco", ggtheme = theme_minimal())
    
  })
  
  output$stat1 <- renderPrint({
    cluster.stats(dist(df1),(eclust(df1, "kmeans", k = input$clustnum, nstart = 25, graph = FALSE))$cluster)
  })
  output$stat2 <- renderPrint({
    (cluster.stats(dist(df1),(eclust(df1, "kmeans", k = input$clustnum, nstart = 25, graph = FALSE))$cluster))$dunn
  })
  
  km_out=kmeans(df1, 3, nstart = 25)
  df_cluster <- data.frame(df1)
  df_cluster$cluster<-km_out$cluster
  

  
})


shinyApp(ui = ui, server = server)

