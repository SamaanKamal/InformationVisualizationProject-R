library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)
library(ggplot2)
library(ggridges)



DataSet = state.x77
#matrix to dataframe
data = data.frame(DataSet)
#combine state abb and region of each state
data = cbind(state.abb,data,state.region)

#renaming first coulmn & 10th one
colnames(data)[1] <- "State abbreviation"
colnames(data)[10] <- "Region"
head(data)
summary(data)
#checking null values
is.null(data)


fun_color_range <- colorRampPalette(c("blue", "red"))
my_colors <- fun_color_range(100) 



#build the shiny app
ui <- dashboardPage(
  
  skin  = 'purple',
  dashboardHeader(title = 'The 1970s in the USA'),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem('Information', tabName = 'info', icon = icon('info')),
      menuItem('Data', tabName = 'data', icon = icon('th')),
      menuItem('Plot', tabName = 'plot', icon = icon('bar-chart-o'))
      
    )),
  dashboardBody( 

    tabItems(
      tabItem(tabName = 'data',
              
              fluidPage(
                checkboxGroupInput("choice", inline = TRUE
                                   ,
                                   "select attribuites to show",
                                   c("Population","Income","Illiteracy","Life.Exp",
                                    "Murder","HS.Grad","Frost","Area","Region"),
                                   ),
                tableOutput("data")
              )
              ),
      
      tabItem(tabName = 'plot', fluidPage(box(plotOutput("plot1"),width=25)
                                          ,box(plotOutput("plot2"),
                                               box(selectInput("vv", "x-axis attribuite :",
                                                               c("Population","Income","Illiteracy","Life.Exp",
                                                                 "Murder","HS.Grad","Frost","Area"),
                                                               selected = "Murder")
                                                   ,plotOutput("plot3"),width=25),
                                               
                                               
                                               box(plotOutput("plot4"),width=25),
                                               box(plotOutput("plot5"),width=25),
                                               box(selectInput("v1", "x-axis attribuite :",
                                                               c("Population","Income","Illiteracy","Life.Exp",
                                                                 "Murder","HS.Grad","Frost","Area","Region"),
                                                               selected = "Murder"),
                                                   selectInput("v2", "y-axis attribuite :",
                                                               c("Population","Income","Illiteracy","Life.Exp",
                                                                 "Murder","HS.Grad","Frost","Area","Region"),
                                                               selected = "Illiteracy"),
                                                   plotOutput("plot6"),width=25),
                                               box(plotOutput("plot7"),width=25),
                                               box(plotOutput("plot8"),width=25),
                                               sliderInput(inputId = "Bandwidth", label = "Bandwidth adjustment:", min = 0.2, max = 2, value = 0.4, step = 0.2),
                                               
                                               collapsible = TRUE, width = 25))),
      tabItem(tabName = 'vaccines', fluidPage(box(plotOutput("plot_vaccine"),width=25))),
      tabItem(tabName = 'info',includeMarkdown("info.Rmd"))
    )
  ))



server <- function(input, output,session) {
  
  output$data <- renderTable({
    
    dtt<-subset(data, select=input$choice)
    dff<-data.frame(data$`State abbreviation`,dtt)
    dff
  })
  
 
  
  
  
  output$plot1 <-renderPlot({
    ggplot(data, aes(x = data$`State abbreviation`, y = data$Population)) +
      labs(x="American States",y="Population", title="Lolipop chart to Show the population across the States")+
      geom_point(size = 3, color = "red") + 
      geom_segment(aes(x = data$`State abbreviation`, xend = data$`State abbreviation`, y = 0, yend = data$Population)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    
  })
  
  
  
  output$plot2 <-renderPlot({
    ggplot(data, aes(x = data$Region, y = data$Population)) +
      labs(x="American Regions",y="Population",title="Boxplot chart to Show the population across the Regions")+
      geom_boxplot() 
    
  })
  
  output$plot3 <-renderPlot({
    
    #Ridgeline plot
    zz <- input$vv
    ggplot(data, aes_string(x = zz, y = data$Region, fill = data$Region)) +
      labs(x="rate of selection attribuite through regions",y="Amreican Region",title="Ridgeline plot chart to Show the Murder Cases across the Regions")+
      geom_density_ridges() +
      theme_ridges() +                                 # No color on backgroud
      theme(legend.position = "none",                  # No show legend
            axis.title.x = element_text(hjust = 0.5),  # x axis title in the center
            axis.title.y = element_text(hjust = 0.5))  # y axis title in the center
    
    
    
  })
  output$plot4 <-renderPlot({
    barplot(data$Murder, ylab = "Murder States",names.arg=data$`State abbreviation`, col =my_colors
            , xlab = "American States",
            main = "barplot chart to show Murder States across American States chart")
    
    
    
    
  })
  
  output$plot5 <-renderPlot({
    plot(data$Income,names.arg=data$`State abbreviation`, type = "o", col = "red",
         
         xlab = "American States", ylab = "Income averge",
         main = "Income line chart")
    
    #lines(data$Population, type = "o", col = "blue")
    #lines(m, type = "o", col = "green")
    
    
  })
  output$plot6 <-renderPlot({
    xx <- input$v1
    yy <- input$v2
    
    
    ggplot(data, aes_string(y = yy, x = xx)) +
      labs(x="x-axis attribuite",y="y-axis attribuite",title="Scatter plot chart")+
      geom_point(size = 3, col = "#663399")
    
    
  })
  
  output$plot7 <-renderPlot({
    ggplot(data, aes(x = data$Illiteracy, fill = data$Region)) + geom_density(alpha = 0.3)+
      labs(x="Illiteracy Rate",y="Regions",title="Density plot chart to Show the Illiteracy Rate across the Regions")
    
    
  })
  
  output$plot8 <-renderPlot({
    hist(data$Life.Exp, breaks = 40, main ="Average Life Experince ",xlab = "Age",ylab="Average",prob = "True",col = 'darkgray', border = 'white')
    lines(density(data$Life.Exp,adjust=as.numeric(input$Bandwidth)), col = "blue")
    
  })
  
  output$info <- renderText({
    tags$h4("Background")
  })
}



shinyApp(ui = ui, server = server)