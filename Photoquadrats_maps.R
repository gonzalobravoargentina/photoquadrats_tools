#Interactive maps with leaflet for Photoquadrats(Photoquadrats_maps) 
# The maps shows
# 1- GPS track of the dive
# 2- GPS position of the Photoquadrats
# 3- Density of mobile fauna 


#Read Photoquadrats Metadata----
# Set working directory to the folder with the photos (in r Studio= session/Set Working directory /Choose directory) -OPCION 1

setwd(paste0(getwd(),"/MAP_example/photoquadrats_georeferenced"))#Set WD to photos folder- OPTION 2
photoquadrats <- list.files(pattern = ".jpg|.JPG|.png")#get a list of files .jpg in wd

library(exifr)
METADATA_photoquadrats <- read_exif(photoquadrats) #read photoquadrat metadata
METADATA_photoquadrats  <- as.data.frame(METADATA_photoquadrats) #transform to dataframe

#create a data.frame with only columms of interest 
library(dplyr)
METADATA_photoquadrats_short<- dplyr::select (METADATA_photoquadrats,SourceFile,time=TimeCreated,timeLOCAL=DateTimeOriginal,Latitude=GPSLatitude,Longitude=GPSLongitude,reef.area=ImageDescription, Diver=Artist,reef.name=Location,Depth=Title) 

## Transform time to POSIXlt
METADATA_photoquadrats_short$timeLOCAL <- strptime(METADATA_photoquadrats_short$timeLOCAL, "%Y:%m:%d %H:%M:%S")


#Coarse gps position of photos with density of spp
library(dplyr) #for data wrangling  
library(stringr) #for wrangling strings  
library(RColorBrewer) # to get some nice colours:


#read density data to coarse with gps position
setwd("..")
setwd(paste0(getwd(),"/density_data"))
densitydata <- read.csv("Density_Data_pardelas2019.csv")

#merge two data frames 
densitydata <- densitydata[,c(2,4,5,22:45)]
density_photo = full_join(METADATA_photoquadrats_short,densitydata, by = c("SourceFile"="Name.photo"))


#select only horizontal photos
#density_photo <- subset(density_photo1,reef.area=="horizontal")


# 2 READ .GPX ------------------
# GPS MODEL GARMIR ETREX 10 
# recorded one point each 3 secs

#Choose as WD folder gpx
setwd("..")
setwd(paste0(getwd(),"/gpx_transects"))
filesGPX <- list.files(pattern = "*.gpx")#get a list of files .jpg in wd

#read gpx files and store them all in a list
library(rgdal)
gpx <- lapply(setNames(filesGPX, make.names(gsub("*.gpx$", "",filesGPX))), 
              function(f) { readOGR(dsn=f, layer="track_points") }
)

#create a list with coordinates and time from all de gpx files
gpxlist <- lapply(gpx,function(f) { data.frame(f@coords,f$time) })

#Create one DATAFRAME with the gps data 
library(purrr)
GPX <- dplyr::bind_rows(gpxlist)


#As the GPX files are in UTC time, we must covert to the local time as the photos
library(lubridate)
GPX$timeUTC <- strptime(GPX$f.time,format = "%Y/%m/%d %H:%M:%OS",tz="UTC")

#local time
GPX$timeLOCAL <- force_tzs(GPX$timeUTC, "UTC", tzone_out = "America/Argentina/Catamarca", roll = FALSE)


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
pal <- colorBin(urchincols, density_photo$Arbacia.dufresnii, bins = brks)  

#types of maps :
#http://leaflet-extras.github.io/leaflet-providers/preview/
  mapout <- leaflet(density_photo) %>%  
  #Use satellite image as base  
  addProviderTiles("OpenStreetMap.Mapnik") %>%  
  setView(lng = x1, lat = y1, zoom = 13) %>%  
  #Add markers for oyster quadrats  
  addCircleMarkers(~ Longitude, ~ Latitude,  
                   color = 'white',opacity =1, weight = 1,  
                   fillColor = ~pal(Arbacia.dufresnii),  
                   popup = as.character(density_photo$Arbacia.dufresnii),  
                   fillOpacity = 0.8,  
                   radius = 6) %>% # add a popup for number of urchins  
  #Add marker showing a picture of the survey site  
  addMarkers(x1, y1, popup = content,  
             options = markerOptions(opacity = 0.9, draggable=T)) %>%  
  #Add a legend  
  addLegend("topright", pal = pal,  
            values = brks,   
            title = "Number of urchins (Arbacia dufresnii)",  
            opacity = 1)  

#guardar archivos en carpeta aparte
#htmlwidgets::saveWidget(mapout, file = 'urchins_map.html', selfcontained = F, libdir = "leafletmap_files")

#guardar archivos en el Html
#htmlwidgets::saveWidget(mapout, file = 'urchins_map_v1.html', selfcontained = T)



#Create map with photoquadrtas and reefs traks 
#upload reef tracks 
library(XML)
library(OpenStreetMap)
library(lubridate)
library(ggmap)
library(ggplot2)
library(raster)
library(sp)


options(digits=10)
# Reaf the GPX file of each reef track- read one by one and then stored in a dataframe (reef1, reef2..)
pfile <- htmlTreeParse(file = "MIII_26-MAR-19_13hs.gpx", error = function(...) {
}, useInternalNodes = T)

elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)

str(coords)
lats <- as.numeric(coords["lat",])
lons <- as.numeric(coords["lon",])


#save track reef in data frame
track_reef1 = data.frame(lon = lons,
                         lat = lats)
track_reef2 = data.frame(lon = lons,
                         lat = lats)
track_reef3 = data.frame(lon = lons,
                         lat = lats)
track_reef4 = data.frame(lon = lons,
                         lat = lats)
track_reef5 = data.frame(lon = lons,
                         lat = lats)


mapout <- leaflet(density_photo) %>%  
#Use satellite image as base  
addProviderTiles("OpenStreetMap.Mapnik") %>%  
#Add reef tracks
addPolylines(data = track_reef1, ~lon, ~lat,
               weight = 3,
               color = 'red',
               popup = 'This is a line!', 
               smoothFactor = 3,
               group = 'Reefs')%>%
addPolylines(data = track_reef2, ~lon, ~lat,
               weight = 3,
               color = 'red',
               popup = 'This is a line!', 
               smoothFactor = 3,
               group = 'Reefs')%>%
  addPolylines(data = track_reef3, ~lon, ~lat,
               weight = 3,
               color = 'red',
               popup = 'This is a line!', 
               smoothFactor = 3,
               group = 'Reefs')%>%
  addPolylines(data = track_reef4, ~lon, ~lat,
               weight = 3,
               color = 'red',
               popup = 'This is a line!', 
               smoothFactor = 3,
               group = 'Reefs')%>%
  addPolylines(data = track_reef5, ~lon, ~lat,
               weight = 3,
               color = 'red',
               popup = 'This is a line!', 
               smoothFactor = 3,
               group = 'Reefs')%>%
#Add markers for quadrats
addCircleMarkers(~ Longitude, ~ Latitude,
                   weight = 0.5,
                   col = 'black', 
                   fillColor = 'darkslategrey',
                   radius = 4, 
                   fillOpacity = 0.9, 
                   stroke = T, 
                   label = ~paste0('Urchins: ',as.character(density_photo$Arbacia.dufresnii)), 
                   group = 'Photoquadrats',popup = as.character(density_photo$Arbacia.dufresnii)) %>% 
#Add marker leggend 
addMarkers(x1, y1, popup = content, options = markerOptions(opacity = 0.9, draggable=T))%>%
addLayersControl(overlayGroups = c('Photoquadrats',
                                   'Reefs'),
                 options = layersControlOptions(collapsed = FALSE),
                 position = 'topright')%>% 
setView(lng = -64.265467, lat = -42.632819, zoom = 13)
 



#Sources 
# https://www.r-bloggers.com/2016/11/create-an-interactive-web-map-with-geotagged-photos/
# https://hansenjohnson.org/post/interactive-maps-in-r/
#https://www.seascapemodels.org/rstats/2016/11/14/photos-to-spatialstat.html



