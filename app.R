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
mass = c("Under 200kg", "200 - 500kg", "500 - 1000kg", "Over 1000kg")
for(i in 1:nrow(df)) {
  for(l in 1:4) { 
    if(df$bins.r[i] == levels(df$bins.r)[l]) {
      df$bins.mass[i] = mass[l]
    }
  }
}
df$bins.mass = factor(df$bins.mass, levels = mass)
pal <- colorFactor(c("#00CD00", "#FFFF00", "#FFA500", "#C30800"), levels = levels(df$bins.r))

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

# map settings
map.style = list('color'= "#2A2A2A",
                 'font-family'= 'Arial',
                 'font-style'= 'normal',
                 'font-size' = '12px',
                 'border-color' = 'rgba(0,0,0,0.5)',
                 'box-shadow' = '3px 3px rgba(0,0,0,0.25)')
url = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"


ui <- fluidPage(
  tags$head(
    tags$style(type="text/css", ".irs { max-width: 500px; }")
  ),
  tabsetPanel(id = "tabs",
  tabPanel(title="Bird Mass", 
    br(),
    h1("Airport bird strike risk reduced through 'off-site' bird management"),
    h4("Animation shows the positive impact of expanding bird management to several sites neighbouring the airport, thus reducing bird mass within the critical 5km radius of the airport, dropping the associated bird strike risk."),
    p("Bird management began at the airport in 2004, spreading to 8 neighbouring areas by 2016."),
    p("The animation shows the massive reduction in bird mass (kg) per neighbouring site during the management period."),
    p("Red represents 1,000s of birds (~8,000 kg of bird mass) and a very high risk of bird strikes. Orange represents reduced bird mass and risk, followed by yellow, and finally green represents very low bird mass and risk."),
    p("Neighbouring areas are prioritised during the bird management program - firstly, based on proximity to the airstrip, and secondly, based on bird mass. Over the entire management period the closest neighbours are brought to low risk (green), and surrounding areas are brought to medium-low risk (yellow)."),
    br(),
    h4("Two key events contributed significantly to the success of the bird management program."),
    p("Firstly, in 2011 the Southern and Western paddocks (where cattle were adjisted) were divided into a 'buffer' zone and a 'cattle' zone. The buffer zone increased the distance between the cattle and the airstrip, thus distancing birds which are symbiotic/commensalistic with the cattle, from the airstrip."),
    p("Secondly, the refuse tip ('landfill') to the south-west of the airport had continued to harbour a massive Ibis population - until 2014, when egg and nest relocation was integrated into the managaement program."),
    br(), br(),
    fluidRow(column(4),
             column(6, sliderInput("Year", label = "Year", sep="", animate=animationOptions(interval=1200, loop=T), value = year(ymd(20040101)), min = year(ymd(20040101)), max = year(ymd(20160101))))),
    
    
    #dygraphOutput("dygraph"),
    leafletOutput("map", height = 650),
    tags$footer(HTML("Copyright &copy; 2016"), strong("Kirsten Zimbardi"), HTML("for Ecosure & Avisure"), 
                style = "text-align: right; color: #3366FF; padding: 2%")
    ),
  
  tabPanel(title="Bird Strike Benchmarks", 
           br(),
           h4("Bird Strikes at Australian Airports with Medium Traffic"),
           p("Mean number of bird strikes per year at Australian airports with 10,000-50,000 flights."),
           p("Colour indicates the number of strikes, size indicates the mean number of flights per anunum."),
           br(), br(),
           #fluidRow(column(4),
           #        column(6, sliderInput("Year", label = "Year", sep="", animate=animationOptions(interval=800, loop=T), value = year(ymd(20040101)), min = year(ymd(20040101)), max = year(ymd(20160101))))),
           
           
           #dygraphOutput("dygraph"),
           leafletOutput("map5", height = 650)
  )
  )
)

server <- function(input, output, session) {
  
  mstones.dy = reactive({ m.ts[which(year(index(m.ts)) %in% year(ymd(paste0(input$Year, "0101")))),] })
  
  output$dygraph <- renderDygraph({
    dygraph(mstones.dy(), main = "Off site management events")
      })
    
    
  #selectedDates = reactive({index(d.ts) %in% input$dates })  
  selectedData = reactive({df[which(year(df$date.time) %in% year(ymd(paste0(input$Year, "0101")))),] })  
  
  output$map <- renderLeaflet({
    map1 = leaflet(df) %>% addTiles() %>% 
      setView(153.572, -28.84838, zoom = 14)
      addLegend(map1, "bottomright", pal = pal, values = ~bins.mass,
                title = "Average bird mass (kg) ",
                opacity = 0.7)
    })
  
  observeEvent(input$Year, {
    proxy.map = leafletProxy("map", data = selectedData())
    proxy.map %>% clearMarkers() %>% 
      addCircleMarkers(
        selectedData()$lng, selectedData()$lat, radius = 10, 
        fill = TRUE, fillOpacity = 1, stroke = F,
        color = pal(selectedData()$bins.mass),
        label = ~LOCATION[1:10], labelOptions = labelOptions(
          noHide=T, 
          direction = "right", offset = c(15, -20),
          textOnly = T,
          style=map.style)) 
  })
  
  output$map5 = renderLeaflet({
    map2 = leaflet(df4) %>% addTiles(urlTemplate=url) %>% 
      setView(view.long, view.lat, zoom = 4) %>%
      addLegend("bottomright", pal = pal2, values = ~strike.ave,
                title = "Average bird strikes (pa)",
                opacity = 0.7) %>%
      addCircleMarkers(lng = df4$longitude, lat = df4$latitude, 
                       radius = ~flights.ave/5000,
                       stroke = F, fill = T, opacity = 1, fillOpacity = 0.7,
                       fillColor = pal2(sort(df4$strike.ave)))
  })
  
  output$plot = renderPlot(with(d, plot_ly(x = Year, y = AveSumMass, text = "Mass", mode = "markers", color = LOCATION, colors = "Spectral", size = AveSumMass)))

  }

shinyApp(ui = ui, server = server)