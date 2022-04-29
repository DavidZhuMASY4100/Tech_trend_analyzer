library(shiny)
library(tidyr)
library(tidyselect)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggeffects)
library(gridExtra)
library(readxl)

# create a UI on Shiny
ui <- fluidPage(
  titlePanel("Technology Trend Analyzer"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6, actionButton("go", "TRACK")),
      ),
      downloadButton("dldata","Download .csv")
    )
    ,
    mainPanel(
      plotOutput("plot1",height = '2000px'),
      hr(),
      plotOutput("plot2",height = '2000px'),
      hr(),
      plotOutput("plot3",height = '1500px'),
      hr(),
      plotOutput("plot6",height = '2000px'),
      hr(),
      plotOutput("plot7",height = '2000px'),
      hr(),
      plotOutput("plot4",height='500px'),
      hr(),
      plotOutput("plot5",height='5000px')
    )
  )
)

## create a server 
server <- function(input, output, session) {
  
  ## import pivoted table from transform file
  source("C:\\Users\\zhuha\\Indeed_project\\scrapeIndeed_David_transform.R")
  
  
  df <- pivoted_df %>% mutate(Total_Freq = rowSums(across(where(is.numeric)))) %>% 
    relocate(Total_Freq, .after = Technology)  %>%
    setNames(.,c('Category','Technology','Total_Freq',format(as.POSIXct((names(.)[4:length(.)]),format='%Y-%m-%d'),'%Y-%m-%d'))) 
  dfbyday <- df %>% select(-Total_Freq) %>% tidyr::gather('Time','Freq', 3:length(.))                                                                                    
  cates=unique(df$Category)
  tl=df
  v <- reactiveValues(data = NULL)
  
  ## create a download function for the pivoted data 
    output$dldata <- downloadHandler(
    filename = function() {c
      paste0("technologies_frequency", ".csv")
    },
    content = function(file) {
      write.csv(tl, file,row.names = FALSE)
    }
  )
  
  ## box plots on freq counts
  single.plot <- function(i){
    ggplot(tl[tl$Category==cates[i],],aes(x=reorder(Technology,desc(Total_Freq)),y=Total_Freq))+ 
      geom_bar(stat = "identity")+
      facet_wrap(vars(Category),nrow = 11)+
      theme(axis.text.x=element_text(angle=30, hjust=1))+ 
      labs(title="Mentioned times of technologies in " %>% paste0(cates[i]), 
           x="Technology", y = "Frequency")
  }
  
  ## percent plots on freq
  single.plot.perc <- function(i){
    ggplot(tl[tl$Category==cates[i],] %>% group_by(Category) %>% mutate(perc = Total_Freq / sum(Total_Freq)),
           aes(x=reorder(Technology,desc(Total_Freq)),y=perc)) + 
      geom_bar(stat = "identity")+
      facet_wrap(vars(Category),nrow = 11)+
      scale_y_continuous(labels = scales::percent) +
      theme(axis.text.x=element_text(angle=30, hjust=1))+ 
      labs(title="Mentioned times of technologies in " %>% paste0(cates[i]), 
           x="Technology", y = "Proportion of Frequency %")
  }
  
  ## boxplot 
  bps <- list()
  for (i in 1:length(cates)){
    bpi <- eval(substitute(single.plot(i),list(i=i)))
    bps[[i]] <- bpi
    # ggsave(filename = paste0('plot',i,'.png'),plot = last_plot())
  }
  
  ## boxplot 
  bps_perc <- list()
  for (i in 1:length(cates)){
    bpi <- eval(substitute(single.plot.perc(i),list(i=i)))
    bps_perc[[i]] <- bpi
    # ggsave(filename = paste0('plot',i,'.png'),plot = last_plot())
  }
  
  ## all trend chart 
  
  single.trend <- function(i){
    ggplot(data=dfbyday[dfbyday$Category==cates[i],],
           aes(x=Time,y=Freq,group=Technology))+
      geom_point()+geom_line(aes(color=Technology))+
      labs(x='Week of',y='Total Frequency')+
      theme(axis.text.x=element_text(angle=50, hjust=1))+
      facet_wrap(vars(Category) )
  }
  trends <- list()
  for (i in 1:length(cates)){
    trend <- eval(substitute(single.trend(i),list(i=i)))
    trends[[i]] <- trend
    
    ## save plots for further use
    ## ggsave(filename = paste0('plot',i,'.png'),plot = last_plot())
  }
  
  # enable "Track" after click on "Go"
  observeEvent(input$go,{
    v$data <- 1
  })
  
  observeEvent(input$reset,{
    v$data <- NULL
    
    

  ## render plot 1 - 6  
  })
  output$plot1 <- renderPlot({
    if (is.null(v$data)){
      return()} 
    else {
      return({plot(arrangeGrob(bps[[1]],bps[[2]],bps[[3]],bps[[4]],nrow=4))})
    }
  })
  output$plot2 <- renderPlot({
    if (is.null(v$data)){
      return()} else {
        return({plot(arrangeGrob(bps[[5]],bps[[6]],bps[[7]],bps[[8]],nrow=4))})
      }
  })
  output$plot3 <- renderPlot({
    if (is.null(v$data)){
      return()} else {
        return({plot(arrangeGrob(bps[[9]],bps[[10]],bps[[11]],nrow=3))})
      }
  })
  output$plot4 <- renderPlot({
    if (is.null(v$data)){
      return()} else {
        
        (ggplot(data=dfbyday%>%
                  group_by(Category,Time)%>%
                  summarise(Mean_freq=mean(Freq)),
                aes(x=Time,y=Mean_freq,color=Category,group=Category))+
           geom_point()+geom_line()+
           labs(x='Week of',y='Frequency', title='Average Frequency by Category')+
           theme(axis.text.x=element_text(angle=60, hjust=1)))
        
      }
  })
  output$plot5 <- renderPlot({
    if (is.null(v$data)){
      return()} else {
        return({plot(arrangeGrob(trends[[1]],trends[[2]],trends[[3]],trends[[4]],trends[[5]],trends[[6]],trends[[7]],trends[[8]],trends[[9]],trends[[10]],trends[[11]],nrow=11))})
      }
  })
  
  output$plot6 <- renderPlot({
    if (is.null(v$data)){
      return()} 
    else {
      return({plot(arrangeGrob(bps_perc[[1]],bps_perc[[2]],bps_perc[[3]],bps_perc[[4]],nrow=4))})
    }
  })
  
  output$plot7 <- renderPlot({
    if (is.null(v$data)){
      return()} 
    else {
      return({plot(arrangeGrob(bps_perc[[5]],bps_perc[[6]],bps_perc[[7]],bps_perc[[8]],nrow=4))})
    }
  })
}

shinyApp(ui, server)


#   Meeting Comments
## add a advanced function to let user enter their input (could be multiple technology)


