#Interactive maps with leaflet for Photoquadrats sampling (Photoquadrats_maps) 
# The maps show
# 1- GPS track of the dive
# 2- GPS position of the Photoquadrats
# 3- Density of mobile fauna 



#Coarse gps position of photos with density of spp
library(dplyr) #for data wrangling  
library(stringr) #for wrangling strings  
library(RColorBrewer) # to get some nice colours:


#read density data to coarse with gps position
densitydata <- read.csv("Density_Data(Pardelas2019).csv",stringsAsFactors =F)

#merge two data frames 
density_photo1 = left_join(densitydata, dat2, by = c("Name.photo"="SourceFile"))

#select only horizontal photos
density_photo <- subset(density_photo1,reef.area=="horizontal")

#generate pannel for leaflet map
content <- paste(sep = "
",  
"",  
"Rocky Reefs",  
"at Pardelas Bay")

#Point of mop center
#-42.632819, -64.265467
x1 <- -64.265467  
y1 <- -42.632819 


#color gradiente for density
library(leaflet)
brks <- c(0,1,5,10,15)  
ncol <- length(brks)-1  
urchincols <- c('grey20',brewer.pal(ncol-1, 'Purples'))  
pal <- colorBin(urchincols, density_photo$Helcogrammoides.cunninghami, bins = brks)  

#types of maps :
http://leaflet-extras.github.io/leaflet-providers/preview/
  mapout <- leaflet(density_photo) %>%  
  #Use satellite image as base  
  addProviderTiles("OpenStreetMap.Mapnik") %>%  
  setView(lng = x1, lat = y1, zoom = 13) %>%  
  #Add markers for oyster quadrats  
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude,  
                   color = 'white',opacity =1, weight = 1,  
                   fillColor = ~pal(Arbacia.dufresnii),  
                   popup = as.character(density_photo$Arbacia.dufresnii),  
                   fillOpacity = 0.8,  
                   radius = 6) %>% # add a popup for number of oysters  
  #Add marker showing a picture of the survey site  
  addMarkers(x1, y1, popup = content,  
             options = markerOptions(opacity = 0.9, draggable=T)) %>%  
  #Add a legend  
  addLegend("topright", pal = pal,  
            values = brks,   
            title = "Number of urchins (Arbacia dufresnii)",  
            opacity = 1)  

#guardar archivos en carpeta aparte
htmlwidgets::saveWidget(mapout, file = 'urchins_map.html', selfcontained = F, libdir = "leafletmap_files")

#guardar archivos en el Html
htmlwidgets::saveWidget(mapout, file = 'urchins_map_v1.html', selfcontained = T)



# make a simple track line
lin = data.frame(lon = c(-65.17536, -65.37423, -65.64541, -66.06122, -66.15161),
                 lat = c(43.30837, 42.94679, 42.87448, 42.92871, 42.72985))

# make a few points
pts = data.frame(lon = c(-65.3, -65.7, -64.1),
                 lat = c(43.4, 43, 42.9))

# build a polygon (in this case the 'Roseway Basin Area To Be Avoided')
ply = data.frame(lon = c(-64.916667, -64.983333, -65.516667, -66.083333),
                 lat = c(43.266667, 42.783333, 42.65, 42.866667))

# required libraries
library(leaflet, quietly = T, warn.conflicts = F)

# start basemap
map <- leaflet() %>% 
  
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  
  # add another layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  
  # add graticules from a NOAA webserver
  addWMSTiles(
    "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
    layers = c("1-degree grid", "5-degree grid"),
    options = WMSTileOptions(format = "image/png8", transparent = TRUE),
    attribution = NULL,group = 'Graticules') %>%
  
  # focus map in a certain area / zoom level
  setView(lng = -65, lat = 43, zoom = 7) %>%
  
  # add layers control
  addLayersControl(overlayGroups = c('Place names',
                                     'Graticules',
                                     'Points',
                                     'Lines',
                                     'Polygons'),
                   options = layersControlOptions(collapsed = FALSE),
                   position = 'topright') %>%
  
  # list groups to hide on startup
  hideGroup(c('Place names'))

# show map
map
# add points
map <- map %>%
  addCircleMarkers(data = pts, ~lon, ~lat,
                   weight = 0.5,
                   col = 'black', 
                   fillColor = 'darkslategrey',
                   radius = 4, 
                   fillOpacity = 0.9, 
                   stroke = T, 
                   label = ~paste0('Point at: ', 
                                   as.character(round(lat,3)), ', ', 
                                   as.character(round(lon,3))), 
                   group = 'Points')

# add lines
map <- map %>%
  addPolylines(data = lin, ~lon, ~lat,
               weight = 3,
               color = 'red',
               popup = 'This is a line!', 
               smoothFactor = 3,
               group = 'Lines') 

# add polygons
map <- map %>%
  addPolygons(data=ply, lng=~lon, lat=~lat,
              weight = 1, 
              color = 'grey', 
              fillColor = 'grey',
              fill = T, 
              fillOpacity = 0.25, 
              stroke = T, 
              dashArray = c(5,5), 
              smoothFactor = 3,
              options = pathOptions(clickable = F),
              group = 'Polygons')

# show map
map



#Sources 
# https://www.r-bloggers.com/2016/11/create-an-interactive-web-map-with-geotagged-photos/
# https://hansenjohnson.org/post/interactive-maps-in-r/
#https://www.seascapemodels.org/rstats/2016/11/14/photos-to-spatialstat.html



