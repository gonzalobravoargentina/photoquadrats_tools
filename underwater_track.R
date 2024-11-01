#This code is designed to read GPX files from a Garmin device that was used in conjunction with a buoy connected to a diver, recording the diver's track during a scuba diving expedition. It first specifies the location of the 'GPX_files' folder, which contains the GPX files captured during the dive. Using the 'rgdal' library, the code reads these GPX files and extracts the track points, allowing for further analysis or visualization of the diving trajectory. This code enables the efficient processing and utilization of GPX data recorded during underwater activities for marine research and exploration


#READ GPX FILE---------- 
#Folder GPS_files with .gpx files 
gpx_folder <-"GPX_files"

# get list of all .gpx files in the folder
filesGPX <- list.files(path = gpx_folder, pattern = "\\.gpx$", full.names = TRUE)

library(rgdal)
# read the .gpx files with the rdal package
gpx <- lapply(setNames(filesGPX, make.names(gsub("*.gpx$", "",filesGPX))), 
              function(f) { readOGR(dsn=f, layer="track_points") }
)

#Create a list with ONLY coordenates and time of all de gpx
gpxlist <- lapply(gpx,function(f) { data.frame(f@coords,f$time) })

names(gpxlist)# get the names of each track

#choose ONE of the tracks 
track <- gpxlist[["GPX_files.Track_2023.09.24.162651"]]
colnames(track) <- c("lon", "lat","time")

library(lubridate)
track$timeUTC <- strptime(track$time,format = "%Y/%m/%d %H:%M:%OS",tz="UTC")
track$timeLOCAL <- force_tzs(track$timeUTC, "UTC", tzone_out = "America/Argentina/Catamarca", roll = FALSE)

#plot the track in map
library(leaflet)
# Create a path on the map using the track data
leaflet()  %>% addProviderTiles(
  "Esri.WorldImagery")%>% 
  addPolylines(
    lng = track$lon,
    lat = track$lat,
    color = "blue", # You can change the color as desired
    weight = 2
  )


#Now we cut the track with the time of the first photo and last (TIP: take a pic at the beging and at the end of the dive)
#Look into the photos date and set the time of the firts photo and last
library(lubridate)
fotofirst <- as.POSIXct("2023-09-24 14:44:45")
fotolast <- as.POSIXct("2023-09-24 16:14:45")
fotolast-fotofirst #get time of total track 
int <- interval(fotofirst, fotolast)

# We create a shift function to include in the roe the next value
shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }


#Cut the track within the time interval
divetrack<- track[track$timeLOCAL %within% int,]

#plot the track in map
library(leaflet)
# Create a path on the map using the track data
leaflet()  %>% addProviderTiles(
  "Esri.WorldImagery")%>% 
  addPolylines(
    lng = divetrack$lon,
    lat = divetrack$lat,
    color = "blue", # You can change the color as desired
    weight = 2
  )

#apply function to shift.vec
divetrack$lat.p1 <- shift.vec(divetrack$lat, -1)
divetrack$lon.p1 <- shift.vec(divetrack$lon, -1)

library(raster)
divetrack$dist.to.prev <- apply(divetrack, 1, FUN = function (row) {
  pointDistance(c(as.numeric(row["lat.p1"]),
                  as.numeric(row["lon.p1"])),
                c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                lonlat = T)
})

#total distance of the track
TOTALDISTANCE <- sum(divetrack$dist.to.prev, na.rm=TRUE)
print(paste("The distance of the dive was ",TOTALDISTANCE, " meters"))


# Shift the time vector, too.
divetrack$time.p1 <- shift.vec(divetrack$timeLOCAL, -1)

# Calculate the number of seconds between two positions, this was set to 3s in the GARMIN GPS
divetrack$time.diff.to.prev <- as.numeric(difftime(divetrack$time.p1, divetrack$timeLOCAL))

# Calculate metres per seconds, LOWESS smoothers to get rid of some noise.
divetrack$speed.m.per.sec <- divetrack$dist.to.prev / divetrack$time.diff.to.prev
divetrack$speed.km.per.h <- divetrack$speed.m.per.sec * 3.6
divetrack$speed.km.per.h <- ifelse(is.na(divetrack$speed.km.per.h), 0, divetrack$speed.km.per.h)
divetrack$lowess.speed <- lowess(divetrack$speed.km.per.h, f = 0.2)$y

mean(divetrack$speed.km.per.h)
mean(divetrack$speed.m.per.sec,na.rm = T)


#function for write a NEW GPX track with the information of the dive we get rid all the point that were recorded outside the water.
writeGPX <- function(lat, lon, time, file) {
  o <- c('<gpx version="1.1" creator="R">','<trk>','<trkseg>')
  if (missing(time))
    o <- c(o, paste('<trkpt lat="',lat,'" lon="',lon,'" />', sep=''))
  else
    o <- c(o, paste('<trkpt lat="',lat,'" lon="',lon,'"><time>',paste(gsub(' ','T', as.character(time)), 'Z', sep=''),'</time></trkpt>', sep=''))
  o <- c(o, '</trkseg>', '</trk>', '</gpx>')
  if (is.character(file) || inherits(file, "connection"))
    cat(o, file=file, sep='\n')
}

writeGPX(divetrack$lat,divetrack$lon,divetrack$timeUTC,"bahiachica_lapaloma.gpx")



##COMBINE PHOTO METADATA AND GPS POSITION------

#READ photos
# Set working directory to the folder with the photos (in r Studio= session/Set Working directory /Choose directory)
files <- list.files(pattern = "*.jpg")#get a list of files .jpg in wd

library(exifr)
photometadata <- read_exif(files) #read metadata 
photometadata <- as.data.frame(photometadata )# create a dataframe

## Transform time to POSIXlt
photometadata$timeLOCAL <- strptime(photometadata$CreateDate, "%Y:%m:%d %H:%M:%S")

# Convert timeLOCAL to UTC
photometadata$timeUTC <- photometadata$timeLOCAL + hours(3)

#Get the same time format for metadata and gpx track
divetrack$timeUTC <- ymd_hms(divetrack$timeUTC, tz = "UTC")
photometadata$timeUTC <- ymd_hms(photometadata$timeUTC, tz = "UTC")


#loop for merging divetrack AND photometadata using a time between +- 3 secs
library(dplyr)
for (i in 1:length(divetrack$timeUTC)){
  isbewteen<-between(photometadata$timeUTC, divetrack$timeUTC[i], divetrack$timeUTC[i]+3)
  photometadata$GPSLongitude[isbewteen ]<-divetrack$lon[i]
  photometadata$GPSLatitude[isbewteen ]<-divetrack$lat[i]
  photometadata$timegps[isbewteen ]<-divetrack$time[i]
}



#ADD PHOTOS TO THE MAP------ 
#the photos must be available in some url to be used in the map. In this case we upload the photos to github and use their url

# URL base (here we have the photos)
base_url <- "https://raw.githubusercontent.com/gonzalobravoargentina/photoquadrats/master/Photos/"

# add the name of each photo in the photometadata dataframe
photometadata$url <- paste0(base_url, photometadata$FileName)

#setup the format for the popup in leaflet
photometadata$popup <- lapply(photometadata$url, function(url) {
  htmltools::HTML(paste0("<img src='", url, "' style='width:200px; height:150px;'>"))
})


#create the map
library(leaflet)
library(htmltools)
map <- leaflet(photometadata)%>%# add different provider tiles
  addProviderTiles(
    "Esri.WorldImagery",
    # give the layer a name
    group = "Satelite"
  ) %>%
  addProviderTiles(
    "OpenStreetMap",
    group = "Mapa para zoom"
  ) %>%
  # add a layers control
  addLayersControl(
    baseGroups = c(
      "Satelite", "Mapa para zoom"),
    # position it on the topleft
    position = "topleft"
  ) %>%
  # Agregar trayectoria con una línea delgada roja
  addPolylines(
    data = divetrack,
    lng = ~lon,
    lat = ~lat,
    color = "red",
    weight = 2
  )%>%# Agregar donde se encontraron las anemonas
  addCircleMarkers(
    data = photometadata, 
    ~GPSLongitude, 
    ~GPSLatitude,
    weight = 0.5,
    col = 'white',
    fillColor = 'white',
    radius = 5,
    fillOpacity = 0.5,
    stroke = T,
    popup = ~popup)%>%
# Agregar un control personalizado
addControl(
  html = '<div style="background-color: white; padding: 10px; border: 2px solid black; text-align: center; font-weight: bold;">Tiempo de buceo: 1.5 h, Distancia recorrida: 1170 m </div>',
  position = "bottomright"
)

map


library(htmlwidgets)
# Guardar el mapa como un archivo HTML
saveWidget(map, file = "map_withphotos.html", selfcontained = TRUE)




