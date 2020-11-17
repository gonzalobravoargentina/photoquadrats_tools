#READ GPX FILE---------- 

#Set working directory on folder with .gpx files 
setwd(paste0(getwd(),"/GolfoNuevo_Reefs/gpx"))


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
track <- gpxlist[["X2019.12.17_PARDELASMEDIO_III"]]
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
fotofirst <- as.POSIXct("2019-12-17 15:06:45")
fotolast <- as.POSIXct("2019-12-17 15:23:46")
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

writeGPX(divetrack$lat,divetrack$lon,divetrack$timeUTC,"2019-12-17_PARDELASBAJO_II_III_cut.gpx")










#Read the GPX file
track <- readOGR("Track_02-MAR-20 103837.gpx", layer = "track_points")
crd.gpx <- data.frame(track@coords,track$time)
colnames(crd.gpx) <- c("lon", "lat","time")

library(lubridate)
crd.gpx$timeUTC <- strptime(crd.gpx$time,format = "%Y/%m/%d %H:%M:%OS",tz="UTC")
crd.gpx$timeLOCAL <- force_tzs(crd.gpx$timeUTC, "UTC", tzone_out = "America/Argentina/Catamarca", roll = FALSE)

#Set the time of the firts photo and last1
library(lubridate)
fotofirst <- as.POSIXct("2020-03-02 09:54:02")
fotolast <- as.POSIXct("2020-03-02 10:11:05")
int <- interval(fotofirst, fotolast)

#Cut the track within the time interval
divetrack<- crd.gpx[crd.gpx$timeLOCAL %within% int,]

#plot the track in map
library(ggmap)
#set lat lon min and max
lat <- c(min(divetrack$lat), max(divetrack$lat))
lon <- c(min(divetrack$lon), max(divetrack$lon))
spdf_geo <- divetrack
coordinates(spdf_geo) <- ~ lon + lat
proj4string(spdf_geo) <- "+init=epsg:4326"

class(spdf_geo)
library(mapview)
mapview(spdf_geo)

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

#plot speed 
plot(divetrack$speed.km.per.h, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "",col = "grey40")
lines(divetrack$lowess.speed, col = "blue", lwd = 3)
legend(x="top", legend = c("GPS speed", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(1,3), bty = "n")
abline(h = mean(divetrack$speed.km.per.h), lty = 2, col = "blue")

library(pgirmess)
writeGPX(divetrack, filename = "PIMCPA_I",type="t")


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

writeGPX(divetrack$lat,divetrack$lon,divetrack$timeUTC,"test.gpx")
