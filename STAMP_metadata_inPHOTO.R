# Objectives:
# 1. Read metadata info from photos using the exifr package.
# 2. Read .gpx files (GPS data) using rgdal package.
# 3. Read CSV files from a dive computer.
# 4. Read CSV files from Paralenz camera.
# 5. Merge all data into one dataframe using local time (Argentina) as the index.
# 6. Export GPS data to photo metadata.

# Required libraries
library(exifr)
library(dplyr)
library(rgdal)
library(purrr)
library(lubridate)
library(plyr)
library(readr)
library(RColorBrewer)
library(leaflet)

# 1. READ METADATA INFO FROM PHOTOS -------------------
setwd(paste0(getwd(), "/Photos"))  # Set working directory to photos folder
photos <- list.files(pattern = "*.jpg")  # Get a list of .jpg files

# Read photo metadata
METADATA <- read_exif(photos) %>% as.data.frame()

# Create a simplified metadata dataframe with relevant columns
METADATA_short <- dplyr::select(METADATA, SourceFile, time = TimeCreated, timeLOCAL = DateTimeOriginal)
METADATA_short$timeLOCAL <- strptime(METADATA_short$timeLOCAL, "%Y:%m:%d %H:%M:%S")

# 2. READ .GPX METADATA -------------------
setwd("../GPX_files")  # Move to the GPX folder
filesGPX <- list.files(pattern = "*.gpx")

# Read GPX files and store in a list
gpx <- lapply(setNames(filesGPX, make.names(gsub("*.gpx$", "", filesGPX))),
              function(f) { readOGR(dsn = f, layer = "track_points") })

# Extract coordinates and time from all GPX files into a single dataframe
gpxlist <- lapply(gpx, function(f) { data.frame(f@coords, f$time) })
GPX <- bind_rows(gpxlist)
GPX$timeUTC <- strptime(GPX$f.time, format = "%Y/%m/%d %H:%M:%OS", tz = "UTC")
GPX$timeLOCAL <- force_tzs(GPX$timeUTC, "UTC", tzone_out = "America/Argentina/Catamarca")

# 3. READ DIVE COMPUTER DATA -------------------
setwd("../Divingcomputer_files")  # Move to dive computer folder

# Load individual dive CSV files
dive1 <- read.csv("2020-02-02_1108.csv")
startdivetime1 <- as.POSIXct("2020-02-02 11:08:00", tz = "America/Argentina/Catamarca")
dive1$timeLOCAL <- seq.POSIXt(from = startdivetime1, by = "2 secs", length.out = nrow(dive1))

dive2 <- read.csv("2020-02-02_1146.csv")
startdivetime2 <- as.POSIXct("2020-02-02 11:46:00", tz = "America/Argentina/Catamarca")
dive2$timeLOCAL <- seq.POSIXt(from = startdivetime2, by = "2 secs", length.out = nrow(dive2))

# Combine both dives into one dataframe
COMPUTER <- rbind(dive1, dive2)

# 4. READ PARALENZ CAMERA DATA -------------------
setwd("../Paralenzcamera_files")  # Move to Paralenz camera folder
filesPARALENZ <- list.files(pattern = "*.CSV", full.names = TRUE)

# Combine all Paralenz camera CSV files into one dataframe
PARALENZ <- ldply(filesPARALENZ, read.csv)
PARALENZ$timeLOCAL <- strptime(PARALENZ$Time, "%Y:%m:%d %H:%M:%S")
PARALENZ$Depth <- as.numeric(PARALENZ$Depth)

# 5. MERGE DATAFRAMES -------------------
# Merge GPX data into METADATA based on time (+/- 3 seconds)
for (i in 1:nrow(GPX)) {
  isbetween <- between(METADATA_short$timeLOCAL, GPX$timeLOCAL[i], GPX$timeLOCAL[i] + 3)
  METADATA_short$GPSLongitude[isbetween] <- GPX$coords.x1[i]
  METADATA_short$GPSLatitude[isbetween] <- GPX$coords.x2[i]
  METADATA_short$timegps[isbetween] <- GPX$f.time[i]
}

# Merge dive computer data into METADATA
for (i in 1:nrow(COMPUTER)) {
  isbetween <- between(METADATA_short$timeLOCAL, COMPUTER$timeLOCAL[i], COMPUTER$timeLOCAL[i] + 3)
  METADATA_short$Depth.dive.computer[isbetween] <- COMPUTER$Depth..M.[i]
  METADATA_short$Temp.dive.computer[isbetween] <- COMPUTER$Temp...C.[i]
  METADATA_short$TimeLOCAL.computer[isbetween] <- COMPUTER$timeLOCAL[i]
  METADATA_short$dive.time[isbetween] <- COMPUTER$Elapsed.Dive.Time..hr.min.[i]
}

# Merge Paralenz camera data into METADATA
for (i in 1:nrow(PARALENZ)) {
  isbetween <- between(METADATA_short$timeLOCAL, PARALENZ$timeLOCAL[i], PARALENZ$timeLOCAL[i] + 3)
  METADATA_short$Depth.PARALENZ[isbetween] <- PARALENZ$Depth[i]
  METADATA_short$Temp.PARALENZ[isbetween] <- PARALENZ$Temperature[i]
  METADATA_short$dive.time.PARALENZ[isbetween] <- PARALENZ$Time[i]
  METADATA_short$video.PARALENZ[isbetween] <- PARALENZ$`Image/video-file`[i]
}

# Write merged metadata to CSV
#write.csv(METADATA_short, file = "METADATA.csv", row.names = FALSE)

# 6. WRITE EXIF DATA TO JPEG -------------------
# Add GPS positions to the photos
# # WARNING: If you run this section, the photos in the 'Photos' folder 
# will be permanently stamped with the GPS positions from the 'METADATA_short' table.
# Ensure you have backups of the original files before proceeding!


setwd("../Photos")  # Set working directory to output folder
for (i in 1:nrow(METADATA_short)) {
  if (!is.na(METADATA_short$GPSLatitude[i]) && !is.na(METADATA_short$GPSLongitude[i])) {
    system(sprintf(
      "exiftool -GPSLatitude=%f -GPSLongitude=%f -GPSLatitudeRef=S -GPSLongitudeRef=W %s",
      METADATA_short$GPSLatitude[i], METADATA_short$GPSLongitude[i], METADATA_short$SourceFile[i]
    ))
  }
}

# Verify that the photos now have GPS metadata
METADATA_updated <- read_exif(photos) %>% as.data.frame()

# Plot updated metadata on a map
leaflet(METADATA_updated) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  setView(lng = -67.596737, lat = -46.043945, zoom = 15) %>%
  addCircleMarkers(~GPSLongitude, ~GPSLatitude,
                   color = 'white', opacity = 1, weight = 1,
                   popup = ~FileName, fillOpacity = 0.8, radius = 3)