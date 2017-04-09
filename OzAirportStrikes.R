rm(list = ls())
require(dplyr)
require(leaflet)

setwd("~/Dropbox/New Career/Round 2/Solution49xTechInterview")

# my functions
source("HelperFunctions.R")

# gis data from: https://datahub.io/dataset/global-airports/resource/d89fd2bf-7f55-4351-9e93-31077c09797as
airports = read.csv("OzAirportsGIS.csv", header = T, stringsAsFactors = F)
# strip down to necessary variables
a = airports[,2:8]
# clean
a = rename(a, 2, "Airport")
#a = a[which(a$iaco != "")]

air.groups = read.csv("OzAirportGroupings.csv", header = T, stringsAsFactors = F)
# strip down to necessary variables
air.groups = rename(air.groups, 2, "iaco")
#ag = merge(a, air.groups[,1:2], by="iaco", all.x=T)

# load strike data
strikes = read.csv("OzAirportStrikes.csv", header = T, stringsAsFactors = F)
# strip down to necessary variables
s = strikes[,c(1:4,11:13)]
# clean
## variable names
names(s) = c("Year", "Airport", "iaco", "iata_faa", "Total.Flights", "Strikes.ATSB", "Strikes.Avisure")
## regex, calculations
for(i in 1:nrow(s)) {
  s$Airport[i] = SentenceCase(s$Airport[i])
  s$Strikes[i] = sum(s$Strikes.ATSB[i], s$Strikes.Avisure[i], na.rm = T) 
}
## replace incorrect acronyms
s$iaco[which(s$iaco == "YCFS")] = "YSCH"
s$iaco[which(s$iaco == "YBRM")] = "YPBR"

## fix variable casting
s$Total.Flights = gsub(",", "", s$Total.Flights)
s$Total.Flights = as.numeric(s$Total.Flights)

# merge clean data sets
m = merge(s, a, by="iaco", all.x=T)
m = merge(m, air.groups[,1:2], by="iaco", all.x=T)

# clean
## fix missing GIS values
m[which(m$iaco == "YCBG"), "latitude"] = -42.8281
m[which(m$iaco == "YCBG"), "longitude"] = 147.4792

# 'check-point save' and generalise to 'df' to make re-using old code easier
df = m

# check data
sort(tapply(df$Strikes, df$name, sum, na.rm=T))
sort(tapply(df$Total.Flights, df$name, sum, na.rm=T))

# normalise - industry convention is number of strikes per 10,000 flights
df = df %>% mutate("Strikes.per.10000Flights" = Strikes/(Total.Flights/10000))

t.strikeRate.ave = df %>% 
  select(name, latitude, longitude, Year, Strikes.per.10000Flights) %>% 
  group_by(name, latitude, longitude) %>% 
  summarise(strikeRate.ave = mean(Strikes.per.10000Flights))
df2 = as.data.frame(t.strikeRate.ave)

t3 = df %>% 
  select(name, latitude, longitude, Year, Strikes.per.10000Flights, Total.Flights, Strikes) %>% 
  group_by(name, latitude, longitude) %>% 
  summarise(strikeRate.ave = mean(Strikes.per.10000Flights), 
            flights.ave = mean(Total.Flights),
            strike.ave = mean(Strikes))
df3 = as.data.frame(t3)


# map strikes
view.lat = mean(df3$latitude)
view.long = mean(df3$longitude)
pal = colorNumeric("YlOrRd", domain = NULL)
map1 = leaflet(df3) %>% addTiles() %>% 
  setView(view.long, view.lat, zoom = 4) %>%
  addCircleMarkers(lng = df3$longitude, lat = df3$latitude, 
                   radius = ~flights.ave/10000,
                   stroke = F, fill = T, opacity = 1, fillOpacity = 0.7,
                   fillColor = pal(sort(df3$strike.ave)))
map1


map2 = leaflet(df3) %>% addTiles() %>% 
  setView(view.long, view.lat, zoom = 4) %>%
  addCircleMarkers(lng = df3$longitude, lat = df3$latitude, 
                   radius = ~flights.ave/10000,
                   stroke = F, fill = T, opacity = 1, fillOpacity = 0.7
                   )
map2

df.medium = subset(df, Total.Flights < 50000)
write.csv(df.medium, "Strikes Medium Airports.csv", row.names = F)
t4 = df.medium %>% 
  select(name, latitude, longitude, Year, Strikes.per.10000Flights, Total.Flights, Strikes) %>% 
  group_by(name, latitude, longitude) %>% 
  summarise(strikeRate.ave = mean(Strikes.per.10000Flights), 
            flights.ave = mean(Total.Flights),
            strike.ave = mean(Strikes))
df4 = as.data.frame(t4)

map4 = leaflet(df4) %>% addTiles() %>% 
  setView(view.long, view.lat, zoom = 4) %>%
  addCircleMarkers(lng = df4$longitude, lat = df4$latitude, 
                   radius = ~flights.ave/5000,
                   stroke = F, fill = T, opacity = 1, fillOpacity = 0.7
  )
map4

map5 = leaflet(df4) %>% addTiles() %>% 
  setView(view.long, view.lat, zoom = 4) %>%
  addCircleMarkers(lng = df4$longitude, lat = df4$latitude, 
                   radius = ~flights.ave/5000,
                   stroke = F, fill = T, opacity = 1, fillOpacity = 0.7,
                   fillColor = pal(sort(df4$strike.ave)))
map5

#addLegend(map1, "bottomright",pal = pal, values = ~AveSumMass,
#          title = "Average bird mass (kg) ",
#          opacity = 0.7)