library(shiny)
library(tidyverse)

# 数据准备 ----

clr <- data.frame(no=1:502,color=colors(TRUE)) %>% 
  rbind(data.frame(no=503:510,color=rep('white',8))) %>% 
  mutate(l=c(rep(1:10,each=50),rep(10,10))) %>% 
  mutate(x=rep(1:5*5,102)) %>% 
  mutate(y=c(rep(rep(1:10*4,each=5),10),rep(11*4,10))) %>% 
  slice_head(n=502)




# shiny 界面 ----

ui <- fluidPage(

    titlePanel("R Color Palette"),
    br(),
    fluidRow(
      column(1),
      column(9,
        plotOutput("Plot"),
        fluidRow(
          column(3),
          column(4,
            numericInput("page", "page", value = 1, min = 1, max = 10),
          )
        )
      )
    )
)

server <- function(input, output) {
  
  df <- reactive(clr %>% filter(l == .env$input$page))
  
  output$Plot <- renderPlot({
    ggplot(df(), aes(xmin=x-2,xmax=x+2, ymin=y-1,ymax=y+1)) +
      geom_rect(aes(fill=I(color)),colour = "black")+
      lims(y=c(50,0))+
      theme_void()+
      geom_text(aes(x,y-2,label=color),size=4)
    
  })
}


shinyApp(ui, server)
