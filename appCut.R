rm(list = ls())

require(shiny)

require(dplyr)
require(tidyr)

require(dygraphs)
require(PerformanceAnalytics)
require(leaflet)
require(RColorBrewer)

require(chron)
require(lubridate)
require(plotly)


# load data
df = read.csv("Summary by Year.csv")
#df = df[-which(is.na(df$Year)),]
df$date.time = as.Date(paste0(df$Year, "-01-01"))
d.ts = xts(df[1:6], df[,7])


df$bins.r = cut(df$AveSumMass, c(0, 200, 500, 1000, 10000))
mass = c("Over 1000kg", "500 - 1000kg", "200 - 500kg", "Under 200kg")
pal <- colorNumeric(c("#00CD00", "#FFFF00", "#FFA500", "#C30800"), df$AveSumMass)

# benchmark map
df.medium = read.csv("Strikes Medium Airports.csv", stringsAsFactors = F)
t4 = df.medium %>% 
  select(name, latitude, longitude, Year, Strikes.per.10000Flights, Total.Flights, Strikes) %>% 
  group_by(name, latitude, longitude) %>% 
  summarise(strikeRate.ave = mean(Strikes.per.10000Flights), 
            flights.ave = mean(Total.Flights),
            strike.ave = mean(Strikes))
df4 = as.data.frame(t4)
view.lat = mean(df4$latitude)
view.long = mean(df4$longitude)
pal2 = colorNumeric("YlOrRd", domain = NULL)

ui <- fluidPage(
  h1("Blank"),
  leafletOutput("map5", height = 650)
)


server <- function(input, output, session) {
  output$map <- renderLeaflet({
    map1 = leaflet(df) %>% addTiles() %>% 
      setView(153.572, -28.84838, zoom = 14)
    addLegend(map1, "bottomright",pal = pal, values = ~AveSumMass,
              title = "Average bird mass (kg) ",
              opacity = 0.7)
  })
  
  output$map5 = renderLeaflet({
    map2 = leaflet(df4) %>% addTiles() %>% 
    setView(view.long, view.lat, zoom = 4) %>%
    addCircleMarkers(lng = df4$longitude, lat = df4$latitude, 
                     radius = ~flights.ave/5000,
                     stroke = F, fill = T, opacity = 1, fillOpacity = 0.7,
                     fillColor = pal2(sort(df4$strike.ave)))
  })
}

shinyApp(ui = ui, server = server)