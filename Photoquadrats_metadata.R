#Objectives:
#1 read photos metadata using exifr package 
#2 reag .gpx files using rgdal package
#3 read .csv from dive computer
#4 merge dataframes using time


#READ PHOTO METADATA-------------------

#Choose folder with photos as wd
files <- list.files(pattern = "*.jpg")#get a list of files .jpg in wd
library(exifr)
photodata <- read_exif(files) #read metadata
photodata <- as.data.frame(photodata) #transform to dataframe


#create a data.frame with only columms of interest 
library(dplyr)
photodata_short<- dplyr::select (photodata,SourceFile,reeforientation=ImageDescription,reef= `Sub-location`, country=`Country-PrimaryLocationName`,time=TimeCreated,timeLOCAL=DateTimeOriginal,GPSLongitude, GPSLatitude)

##Configure Time and Day to get a correct format %z Signed offset in hours and minutes from UTC
photodata_short$timeLOCAL <- strptime(photodata_short$timeLOCAL, "%Y:%m:%d %H:%M:%S")

photodata_short$Date <- as.Date(photodata_short$timeLOCAL,"%Y:%m:%d %H:%M:%S")

library(hms)
photodata_short$time <- as_hms(photodata_short$time)



#READ .GPX METADATA-------------------
#Choose folder with gpx as wd
filesGPX <- list.files(pattern = "*.gpx")#get a list of files .jpg in wd

#read gpx files and store them in a list
library(rgdal)
gpx <- lapply(setNames(filesGPX, make.names(gsub("*.gpx$", "",filesGPX))), 
              function(f) { readOGR(dsn=f, layer="track_points") }
)

#create a list with coordinates and time of all de gpx
gpxlist <- lapply(gpx,function(f) { data.frame(f@coords,f$time) })

library(purrr)
GPX_alltogether <- dplyr::bind_rows(gpxlist)


#As the GPX files has UTC time, we must covert to the same local time as the photos
library(lubridate)
GPX_alltogether$timeUTC <- strptime(GPX_alltogether$f.time,format = "%Y/%m/%d %H:%M:%OS",tz="UTC")

#local time
GPX_alltogether$timeLOCAL <- force_tzs(GPX_alltogether$timeUTC, "UTC", tzone_out = "America/Argentina/Catamarca", roll = FALSE)



#READ DIVE COMPUTER DATA-------------------

#not yet 


#MERGE GPX_alltogether and photodata_short by time


#This will merge only if the cases where the time of the gpx and the photo are exactly the same
GPX_METADATA<- merge(photodata_short,GPX_alltogether, by = "timeLOCAL",all.x=T )






