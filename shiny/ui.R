library(shiny)

shinyUI(fluidPage( 
  tabPanel("text",
           titlePanel("Sentimental Analysis among USA"),
           p("Topic: VSFashionShow"),
           hr(),
           sidebarLayout(
             sidebarPanel(checkboxGroupInput("choices",
               label = "Attitude",
               choices = c("positive","negative","neutral")
             )
                          ),
             mainPanel(plotOutput('map'))
           )
             )))
 


