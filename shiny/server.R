library(shiny) 
library(ggplot2)
# MUST set as working dictionary

#read RDS file for plotting data
sentiment.df <- readRDS("sentiment_location.RDS")

shinyServer(function(input, output) {
    output$map <- renderPlot(
      {
       points <- sentiment.df[sentiment.df$level %in% c(input$choices),]
       map.data <- map_data("state") 
       ggplot(map.data) +
         geom_map(aes(map_id = region), 
                  map = map.data, 
                  fill = "white",       
                  color = "gray20", size = 0.25) + 
         expand_limits(x = map.data$long, y = map.data$lat) +
         ggtitle("Distribution of Attitude in USA") +
         xlab("Longitude") +
         ylab("Latitude") + 
         theme(plot.title = element_text(color = "black",size = 14,face = "bold"))+
         geom_point(data = points ,       
                    aes(x =place_lon, y = place_lat,color = level,shape = level), size = 2, 
                    alpha = 1) 
      }
    )

})


