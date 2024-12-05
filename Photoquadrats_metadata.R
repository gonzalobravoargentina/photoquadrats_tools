
#BIIGLE----
# Set the working directory to the folder containing the photos 
# (You can do this manually in RStudio: Session -> Set Working Directory -> Choose Directory)
files <- list.files(pattern = ".jpg|.JPG|.png") # Get a list of .jpg or .png files in the current directory

# Load necessary libraries
library(exifr) # For extracting EXIF metadata from image files
library(dplyr) # For data manipulation

# Read metadata from the photos
fotos <- read_exif(files) # Extract metadata from the files
fotos <- as.data.frame(fotos) # Convert the extracted metadata to a dataframe

# Filter and rename columns to match BIIGLE's required format
fotos <- fotos %>%
  select(
    filename = SourceFile,         # File name
    taken_at = DateTimeOriginal,   # Date and time when the photo was taken
    lng = GPSLongitude,            # Longitude coordinate
    lat = GPSLatitude,             # Latitude coordinate
    gps_altitude = GPSAltitude     # Altitude of the camera when the photo was taken
  )

# Add missing columns with default or calculated values
fotos <- fotos %>%
  mutate(
    distance_to_ground = 0.45, # Assign a default distance to the ground (adjust as needed)
    area = 0.0625, # Default area shown in the image (adjust if specific information is available)
    taken_at = ifelse(is.na(taken_at), as.character(Sys.time()), taken_at), # Use current date/time if missing
    lng = as.numeric(lng), # Ensure longitude is numeric
    lat = as.numeric(lat), # Ensure latitude is numeric
    gps_altitude = as.numeric(gps_altitude) # Ensure altitude is numeric
  )

# Export the processed metadata as a CSV file for BIIGLE
#write.csv(fotos, "metadata_biigle_cuadrats.csv") # Save the final dataset to a CSV file


#CORALNET-----
# Set working directory to the folder with the photos (in r Studio= session/Set Working directory /Choose directory)
files <- list.files(pattern = ".jpg|.JPG|.png")#get a list of files .jpg in wd

library(exifr)
dat <- read_exif(files) #read metadata 
dat <- as.data.frame(dat)# create a dataframe

#create a short data.frame with the columms of interest 
library(dplyr)
#names used as Pardelas Benthic Survey in CoralNet
#https://coralnet.ucsd.edu/source/1933/
dat2<- dplyr::select (dat,Name=SourceFile,
                      Date=CreateDate,
                      reef_name=`Sub-location`,
                      reef_area=ImageDescription,
                      #understory=CreatorAddress,
                      #Height_cm=CreatorPostalCode,
                      Latitude=GPSLatitude,Longitude=GPSLongitude,
                      Depth=GPSAltitude,
                      Camera=Model, 
                      Photographer=Creator)


#set date format
dat2$Date <- as.Date(dat2$Date,"%Y:%m:%d %H:%M:%S")
dat2$Date <- format(as.Date(dat2$Date),'%m/%d/%Y')

#add some standar columns 
dat2$Water_quality <- "good"
dat2$Strobes <- "two"
dat2$Framing_gear_used <- "25x25cm"
dat2$White_balance_card <- "yes"
dat2$Comments <- "IDLE"
dat2$region <- "ISLA DE LO ESTADOS"
dat2$site <- "NORTE"
dat2$understory <- "no"
dat2$Height_cm <- "45"
#dat2$Photographer <- "Maria Bagur & Gonzalo Bravo"
#dat2$understory <- "no"

library(dplyr)
dat2 <- dat2 %>% 
  relocate("Name","Date","region","site","reef_name","reef_area","understory","Height_cm","Latitude","Longitude","Depth","Camera","Photographer","Water_quality","Strobes","Framing_gear_used","White_balance_card","Comments")


#Modify columns names in order to be identical to CoralNet
names(dat2)<-c("Name","Date","region","site","reef name","reef area","understory","Height (cm)","Latitude","Longitude","Depth","Camera","Photographer","Water quality","Strobes","Framing gear used","White balance card","Comments")

#Height (cm) column only numbers allowed
dat2$`Height (cm)` <- 45

#in case we need to reeplace some text in the columns replace
#library(dplyr)
#dat2 <- dat2 %>%
#mutate(site = ifelse(site == "Tierra del Fuego", "Ushuaia", site))

#dat2 <- dat2 %>%
#  mutate(region = ifelse(region == "GolfoNuevo", "Golfo Nuevo", region))


#Created a csv to be imported to CoralNet
write.csv(dat2, 'metadata.csv',row.names = F)

