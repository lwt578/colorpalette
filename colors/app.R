library(shiny)
library(tidyverse)

# 数据准备 ----

clr <- data.frame(no=1:657,color=colors()) %>% 
  rbind(data.frame(no=658:660,color=rep('white',3)))

clr <- clr %>% 
  mutate(l=rep(1:10,each=66)) %>% 
  mutate(x=rep(1:6*5,110)) %>% 
  mutate(y=rep(rep(1:11*10,each=6),10)) %>% 
  slice_head(n=657)


# shiny 界面 ----

ui <- fluidPage(

    titlePanel("R Color Palette"),
    br(),
    fluidRow(
      column(1),
      column(9,
        numericInput("page", "page", value = 1, min = 1, max = 10),
        plotOutput("Plot")
      )
    )
)

server <- function(input, output) {
  
  df <- reactive(clr %>% filter(l == .env$input$page))
  
  output$Plot <- renderPlot({
    ggplot(df(), aes(xmin=x-2,xmax=x+2, ymin=y-2,ymax=y+2)) +
      geom_rect(aes(fill=I(color)),color = "black")+
      lims(y=c(120,0))+
      theme_void()+
      geom_text(aes(x,y-5,label=color),size=6)
    
  })
}


shinyApp(ui, server)
