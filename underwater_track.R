#READ GPX FILE---------- 
#Set working directory on folder with .gpx files 
filesGPX <- list.files(pattern = "*.gpx")#get a list of files .jpg in wd

library(rgdal)
#read GPX
gpx <- lapply(setNames(filesGPX, make.names(gsub("*.gpx$", "",filesGPX))), 
              function(f) { readOGR(dsn=f, layer="track_points") }
)


#create a list with coord and time of all de gpx
gpxlist <- lapply(gpx,function(f) { data.frame(f@coords,f$time) })

names(gpxlist)
#choose the tracks individually 
track <- gpxlist[["ALERO.20.M_A"]]
colnames(track) <- c("lon", "lat","time")

library(lubridate)
track$timeUTC <- strptime(track$time,format = "%Y/%m/%d %H:%M:%OS",tz="UTC")
track$timeLOCAL <- force_tzs(track$timeUTC, "UTC", tzone_out = "America/Argentina/Catamarca", roll = FALSE)

#plot the track in map
library(ggmap)
library(mapview)
#set lat lon min and max
lat <- c(min(track$lat), max(track$lat))
lon <- c(min(track$lon), max(track$lon))
spdf_geo <- track
coordinates(spdf_geo) <- ~ lon + lat
proj4string(spdf_geo) <- "+init=epsg:4326"
mapview(spdf_geo)

#In case you want to cut the track you can use the time of the first photo and last 
#Set the time of the firts photo and last1
library(lubridate)
fotofirst <- as.POSIXct("2023-09-24 14:44:45")
fotolast <- as.POSIXct("2023-09-24 16:14:45")
fotolast-fotofirst #get time of track 
int <- interval(fotofirst, fotolast)


#shift function to include in the rox the next value
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
#divetrack <- track
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
# Calculate the number of seconds between two positions.
divetrack$time.diff.to.prev <- as.numeric(difftime(divetrack$time.p1, divetrack$timeLOCAL))

# Calculate metres per seconds, LOWESS smoothers to get rid of some noise.
divetrack$speed.m.per.sec <- divetrack$dist.to.prev / divetrack$time.diff.to.prev
divetrack$speed.km.per.h <- divetrack$speed.m.per.sec * 3.6
divetrack$speed.km.per.h <- ifelse(is.na(divetrack$speed.km.per.h), 0, divetrack$speed.km.per.h)
divetrack$lowess.speed <- lowess(divetrack$speed.km.per.h, f = 0.2)$y

mean(divetrack$speed.km.per.h)
mean(divetrack$speed.m.per.sec,na.rm = T)


library(ggmap)
#set lat lon min and max
#Dive_Transect <- Dive_Transect[1:317,]
#divetrack <- divetrack[1:317,]
lat <- c(min(divetrack$lat), max(divetrack$lat))
lon <- c(min(divetrack$lon), max(divetrack$lon))
Dive_Transect <- divetrack
coordinates(Dive_Transect) <- ~ lon + lat
proj4string(Dive_Transect) <- "+init=epsg:4326"
library(mapview)
mapview(Dive_Transect)

#Write a NEW GPX track cutted 
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



# Set working directory to the folder with the photos (in r Studio= session/Set Working directory /Choose directory)
files <- list.files(pattern = "*.jpg")#get a list of files .jpg in wd

library(exifr)
dat <- read_exif(files) #read metadata 
dat <- as.data.frame(dat)# create a dataframe
dat$GPSLatitude
dat$GPSLongitude
dat$CreateDate
dat$Keywords



library(leaflet)
map <- leaflet(divetrack)%>%# add different provider tiles
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
    data = dat, 
    ~GPSLongitude, 
    ~GPSLatitude,
    weight = 0.5,
    col = 'white',
    fillColor = 'white',
    radius = 5,
    fillOpacity = 0.5,
    stroke = T,
    popup = ~Keywords)%>%
# Agregar un control personalizado
addControl(
  html = '<div style="background-color: white; padding: 10px; border: 2px solid black; text-align: center; font-weight: bold;">Tiempo de buceo: 1.5 h, Distancia recorrida: 1170 m </div>',
  position = "bottomright"
)

map  # Mostrar el mapa

library(htmlwidgets)

# Guardar el mapa como un archivo HTML
saveWidget(map, file = "mi_mapa.html", selfcontained = TRUE)
