#Objectives:
#1 read the metadata info from photos using exifr package ()
#2 read .gpx files (GPS) using rgdal package (GPX_files)
#3 read .csv from dive computer (Photos)
#3 read .csv from Paralenz camera (Paralenzcamera_files)
#4 merge all data into one dataframe using localtime (Argentina) as index
#5 export gps dato to photo metadata?


# 1 READ METADATA INFO FROM PHOTOS-------------------
#Set WD to photos folder 
setwd(paste0(getwd(),"/Photos"))
photos <- list.files(pattern = "*.jpg")#get a list of files .jpg in wd
library(exifr)
METADATA <- read_exif(photos) #read photos metadata
METADATA <- as.data.frame(METADATA) #transform to dataframe

#create a data.frame with only columms of interest 
library(dplyr)
METADATA_short<- dplyr::select (METADATA,SourceFile,time=TimeCreated,timeLOCAL=DateTimeOriginal) #only get time and photo names

## Transform time to POSIXlt
METADATA_short$timeLOCAL <- strptime(METADATA_short$timeLOCAL, "%Y:%m:%d %H:%M:%S")



# 2 READ .GPX METADATA-------------------
# GPS MODEL GARMIR ETREX 10 
# recorded one point each 3 secs

#Choose as WD folder gpx
setwd("..")
setwd(paste0(getwd(),"/GPX_files"))
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


#3 READ DIVE COMPUTER DATA-------------------
#MODEL OCEANIC GEO2
#Choose as WD the folder with csv from diving computer 
setwd("..")
setwd(paste0(getwd(),"/Divingcomputer_files"))
#Each diving computer file has the diving time in minutes and secs, recorded each 2 sec
#The file name has the date and starting time of diving
dive1 <- read.csv("2020-02-02_1108.csv")
#at the moment the begining of the dive must be set manually by each dive and add time by 2 secs. BIG PROBLEM the diving computer doesnt record the seconds on the staring time
startdivetime1 <- as.POSIXct("2020-02-02 11:08:00", tz = "America/Argentina/Catamarca")
dive1$timeLOCAL <- seq.POSIXt( from=startdivetime1, by="2 secs", length.out=nrow(dive1))

dive2 <- read.csv("2020-02-02_1146.csv")
startdivetime2 <- as.POSIXct("2020-02-02 11:46:00", tz = "America/Argentina/Catamarca")
dive2$timeLOCAL <- seq.POSIXt( from=startdivetime2, by="2 secs", length.out=nrow(dive2))

#dive 1 and dive 2 in one dataframe
COMPUTER <- rbind(dive1,dive2)
rm(dive1,dive2)

#4 READ PARALENZ CAMERA DATA---------------------------------
library(plyr)
library(readr)
#Choose as WD the folder with csv from PARALENZ camera
setwd("..")
setwd(paste0(getwd(),"/Paralenzcamera_files"))
#get a list of CSV files
filesPARALENZ <- list.files(pattern = "*.CSV",full.names=TRUE)

#Read all CSV files an import all in one dataframe
PARALENZ = ldply(filesPARALENZ, read.csv)

#Transforme time column to POSIXlt (local time Argentina)
PARALENZ$timeLOCAL <- strptime(PARALENZ$Time, "%Y:%m:%d %H:%M:%S")

#Transforme depth column into numeric 
PARALENZ$Depth <- as.numeric(PARALENZ$Depth)


# 5 MERGE DATAFRAMES (METADATA/GPX/COMPUTER/PARALENZ)----------

#https://stackoverflow.com/questions/47790397/match-posixct-in-one-dataset-to-a-range-of-posixct-in-another-dataset

library(dplyr)
#loop for merging GPX AND METADATA using a time between +- 3 secs
for (i in 1:length(GPX$timeLOCAL)){
  isbewteen<-between(METADATA_short$timeLOCAL, GPX$timeLOCAL[i], GPX$timeLOCAL[i]+3)
  METADATA_short$GPSLongitude[isbewteen ]<-GPX$coords.x1[i]
  METADATA_short$GPSLatitude[isbewteen ]<-GPX$coords.x2[i]
  METADATA_short$timegps[isbewteen ]<-GPX$f.time[i]
}

#loop for merging COMPUTER AND METADATA using a time between +- 3 secs
for (i in 1:length(COMPUTER$timeLOCAL)){
  isbewteen<-between(METADATA_short$timeLOCAL, COMPUTER$timeLOCAL[i], COMPUTER$timeLOCAL[i]+3)
  METADATA_short$Depth.dive.computer[isbewteen ]<-COMPUTER$Depth..M.[i]
  METADATA_short$Temp.dive.computer[isbewteen ]<-COMPUTER$Temp...C.[i]
  METADATA_short$TimeLOCAL.computer[isbewteen ] <-COMPUTER$timeLOCAL[i]
  METADATA_short$dive.time[isbewteen ]<-COMPUTER$Elapsed.Dive.Time..hr.min.[i]
}

METADATA_short$TimeLOCAL.computer <- as.POSIXct(METADATA_short$TimeLOCAL.computer,origin = "1970-01-01",tz = "America/Argentina/Catamarca")

#another way to merge using min or sec resolution 
#merge(cbind(round(METADATA_short$timeLOCAL, "min"), METADATA_short),cbind(round(COMPUTER$timeLOCAL, "min"), COMPUTER),by=1, suffixes=c("_photo", "_computer"))[-1]



#loop for merging PARALENZ AND METADATA using a time between +- 3 secs
for (i in 1:length(PARALENZ$timeLOCAL)){
  isbewteen<-between(METADATA_short$timeLOCAL, PARALENZ$timeLOCAL[i], PARALENZ$timeLOCAL[i]+3)
  METADATA_short$Depth.PARALENZ[isbewteen ]<-PARALENZ$Depth[i]
  METADATA_short$Temp.PARALENZ[isbewteen ]<-PARALENZ$Temperature[i]
  METADATA_short$dive.time.PARALENZ[isbewteen ]<-PARALENZ$Time[i]
  METADATA_short$video.PARALENZ[isbewteen ]<-PARALENZ$`Image/video-file`[i]
}


#Generate a file
#write.csv(METADATA_short,file="METADATA.csv")


#view photos in a map
#generate pannel for leaflet map
content <- paste(sep = "
",  
"",  
"Rocky Reefs",  
"at La Tranquera")

#Point of map center
x1 <- -67.596737
y1 <- -46.043945

#color gradiente for depth
brks <- c(0,1,5,10,12)  
ncol <- length(brks)-1  
library(RColorBrewer)
depthcols <- c('grey20',brewer.pal(ncol-1, 'Purples'))  

pal <- colorBin(depthcols, METADATA_short$Depth.dive.computer, bins = brks)

library(leaflet)
leaflet(METADATA_short) %>%  
  #Use satellite image as base  
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15) %>%  
  #Add markers for oyster quadrats  
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude,  
                   color = 'white',opacity =1, weight = 1,  
                   fillColor = ~pal(Depth.dive.computer),  
                   popup = as.character(METADATA_short$Depth.dive.computer),  
                   fillOpacity = 0.8,  
                   radius = 3) %>% # add a popup for number of oysters  
  #Add marker showing a picture of the survey site  
  addMarkers(x1, y1, popup = content,  
             options = markerOptions(opacity = 0.9, draggable=T)) %>%  
  #Add a legend  
  addLegend("topright", pal = pal,  
            values = brks,   
            title = "Depth",  
            opacity = 1)  


# 6 Write EXIF data to JPEG--------------
# Add GPS positions to the Photos 
#this will rewrite the metadata of the photos and generate a file with the original metadata
setwd("..")
setwd(paste0(getwd(),"/Photos"))
#this is for south west hemisphere, for other hemispheres you must change the -GPSLatitudeRef=S -GPSLongitudeRef=W
for (i in 1:nrow(METADATA_short)){
  output <- system(sprintf("exiftool -GPSLatitude=%f -GPSLongitude=%f -GPSLatitudeRef=S -GPSLongitudeRef=W %s",METADATA_short$GPSLatitude[i],METADATA_short$GPSLongitude[i],METADATA_short$SourceFile[i])) 
}


#Test if the photos have the GPS position
picsPath <- "/Users/gonzalobravo/Documents/GitHub/photoquadrats_metadata/Photos"
info <- system(paste("exiftool -GPSLatitude -GPSLongitude -DateTimeOriginal '", picsPath,"'",sep=""), inter=TRUE)

library(exifr)
METADATA2 <- read_exif(photos) #read photos metadata
METADATA2 <- as.data.frame(METADATA2)
METADATA2$GPSLatitude
METADATA2$GPSLongitude

leaflet(METADATA2) %>%  
  #Use satellite image as base  
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15) %>%  
  #Add markers 
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude,  
                   color = 'white',opacity =1, weight = 1,  
                   popup = as.character(METADATA2$FileName),  
                   fillOpacity = 0.8,  
                   radius = 3)


#Sources
#https://github.com/hrbrmstr/exiv

#https://stackoverflow.com/questions/57438052/write-exif-data-back-to-jpeg-in-r

#https://stackoverflow.com/questions/41849691/how-to-add-gps-latitude-and-longitude-using-exiftool-in-mac-how-to-edit-meta-da/

#https://stackoverflow.com/questions/47293978/r-write-exif-data-to-jpeg-file

#http://timelyportfolio.github.io/rCharts_catcorrjs/exif/

#https://stackoverflow.com/questions/41849691/how-to-add-gps-latitude-and-longitude-using-exiftool-in-mac-how-to-edit-meta-da/



