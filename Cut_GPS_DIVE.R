# --- Load necessary libraries ---
library(rgdal)      # For reading GPX files
library(lubridate)  # For handling dates and times
library(leaflet)    # For creating interactive maps
library(raster)     # For spatial data manipulation


# --- 1. Read GPX Files ---
# Define the folder containing the .gpx files
#Session/Set working directory/Choose directory...

# Get a list of all .gpx files in the specified folder
filesGPX <- list.files(pattern = "\\.gpx$", full.names = TRUE)

# Read each .gpx file using 'readOGR' from the 'rgdal' package
# Create a named list where each element is a track from the .gpx file
gpx <- lapply(setNames(filesGPX, make.names(gsub("*.gpx$", "", filesGPX))),
              function(f) { readOGR(dsn = f, layer = "track_points") })

# Extract only the coordinates and time from each track and store them in a list
gpxlist <- lapply(gpx, function(f) { data.frame(f@coords, f$time) })

# Display the names of each track
names(gpxlist)


# --- 2. Select a Specific Track ---
# Choose one of the tracks from the list (modify as needed)
track <- gpxlist[["..Track_14.NOV.24.135637"]]
colnames(track) <- c("lon", "lat", "time")

# Convert the time column to UTC format
track$timeUTC <- strptime(track$time, format = "%Y/%m/%d %H:%M:%OS", tz = "UTC")

# Convert the time to the local time zone (Argentina)
track$timeLOCAL <- force_tzs(track$timeUTC, "UTC", tzone_out = "America/Argentina/Catamarca", roll = FALSE)

# --- 3. Plot the Track on a Map ---
# Create an interactive map to visualize the track
leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addPolylines(lng = track$lon, lat = track$lat, color = "blue", weight = 2)

# --- 4. Trim the Track Based on Dive Start and End Times ---
# Specify the time of the first and last photo taken during the dive
fotofirst <- as.POSIXct("2024-11-14 12:47:00")
fotolast <- as.POSIXct("2024-11-14 13:22:00")

# Calculate the total duration of the dive
fotolast - fotofirst

# Create a time interval based on the start and end times
int <- interval(fotofirst, fotolast)

# Filter the track to include only points within the time interval
divetrack <- track[track$timeLOCAL %within% int, ]

# --- 5. Plot the Trimmed Dive Track ---
leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addPolylines(lng = divetrack$lon, lat = divetrack$lat, color = "blue", weight = 2)

# --- 6. Define a Shift Function for Calculating Distances ---
# This function shifts a vector by a specified number of positions
shift.vec <- function(vec, shift) {
  if (length(vec) <= abs(shift)) {
    rep(NA, length(vec))
  } else {
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec) - shift)])
    } else {
      c(vec[(abs(shift) + 1):length(vec)], rep(NA, abs(shift)))
    }
  }
}

# Shift the latitude and longitude vectors by one position to calculate distances
divetrack$lat.p1 <- shift.vec(divetrack$lat, -1)
divetrack$lon.p1 <- shift.vec(divetrack$lon, -1)

# --- 7. Calculate Distance Between Points ---
# Calculate the distance (in meters) between consecutive points
divetrack$dist.to.prev <- apply(divetrack, 1, function(row) {
  pointDistance(c(as.numeric(row["lat.p1"]), as.numeric(row["lon.p1"])),
                c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                lonlat = TRUE)
})

# Calculate the total distance of the dive track
TOTALDISTANCE <- sum(divetrack$dist.to.prev, na.rm = TRUE)
print(paste("The distance of the dive was", TOTALDISTANCE, "meters"))

# --- 8. Calculate Speed ---
# Shift the time vector by one position to calculate time differences
divetrack$time.p1 <- shift.vec(divetrack$timeLOCAL, -1)

# Calculate the time difference (in seconds) between consecutive points
divetrack$time.diff.to.prev <- as.numeric(difftime(divetrack$time.p1, divetrack$timeLOCAL))

# Calculate speed in meters per second and convert to kilometers per hour
divetrack$speed.m.per.sec <- divetrack$dist.to.prev / divetrack$time.diff.to.prev
divetrack$speed.km.per.h <- divetrack$speed.m.per.sec * 3.6

# Replace NA values in speed with 0
divetrack$speed.km.per.h <- ifelse(is.na(divetrack$speed.km.per.h), 0, divetrack$speed.km.per.h)

# Apply a LOWESS smoother to reduce noise in the speed data
divetrack$lowess.speed <- lowess(divetrack$speed.km.per.h, f = 0.2)$y

# Calculate and print the mean speed
mean_speed_kmh <- mean(divetrack$speed.km.per.h)
mean_speed_mps <- mean(divetrack$speed.m.per.sec, na.rm = TRUE)
print(paste("Mean speed:", mean_speed_kmh, "km/h"))
print(paste("Mean speed:", mean_speed_mps, "m/s"))

# --- 9. Write a New GPX File for the Trimmed Dive Track ---
# Function to write a new GPX file using the filtered dive track data
writeGPX <- function(lat, lon, time, file) {
  o <- c('<gpx version="1.1" creator="R">', '<trk>', '<trkseg>')
  if (missing(time)) {
    o <- c(o, paste('<trkpt lat="', lat, '" lon="', lon, '" />', sep = ''))
  } else {
    o <- c(o, paste('<trkpt lat="', lat, '" lon="', lon, '"><time>',
                    paste(gsub(' ', 'T', as.character(time)), 'Z', sep = ''),
                    '</time></trkpt>', sep = ''))
  }
  o <- c(o, '</trkseg>', '</trk>', '</gpx>')
  cat(o, file = file, sep = '\n')
}

# Write the new GPX file for the dive track
writeGPX(divetrack$lat, divetrack$lon, divetrack$timeUTC, "Track_2024-11-14 115156_DIVEIII_Grego.gpx")
