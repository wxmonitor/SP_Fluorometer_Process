# Storm Petrel fluorometer processing script
# Loads raw fluorometer file and ship's NMEA data including time and position
# Writes new processed fluorometer file as .txt file with the following column format:

#   GMT DATE     GMT TIME     FLUOR   LAT    LONG
# (YYYY-MM-DD)  (HH:MM:SS)     (nm)   (deg)  (deg)

############################################################################################################
############################################################################################################

### Enter filenames to be processed
### Files must be located in your working directory
### Run getwd() to determind working directory

# Ship's position data file. Must be a .txt file containing NMEA sentences

positionFile <- "SP_GPS_240509.cap"

# Fluorometry data file. Must be a .txt file containing fluorometry data.
# Filename format must match: "SP_FL_YYMMDD_HHMMSS.txt" and reflect data start time.
# Sampling rate is in units: n/second.

fluorFile <- "SP_FL_240509_173128.txt"
samplingRate <- 10

############################################################################################################
############################################################################################################

# Check and load required package
if (!require('tidyverse')) {
  install.packages('tidyverse')
}

library(tidyverse)

# Get working directory to locate files
dir <- getwd()

# Read and clean ship's position data file

fileRead <- read.csv(paste(dir, positionFile, sep = "/"),
                     header = FALSE,
                     colClasses = "character")

dateRead <- fileRead %>%
  filter(V1 == "$GPZDA") %>%
  select(c(2:5)) %>%
  'colnames<-'(c("time", "day", "month", "year")) %>%
  mutate(time = str_remove(time, pattern = "\\..*"))

posRead <- fileRead %>%
  filter(V1 == "$GPGGA") %>%
  select(c(2, 3, 5)) %>%
  'colnames<-'(c("time", "lat.raw", "long.raw")) %>%
  mutate(time = str_remove(time, pattern = "\\..*")) %>%
  mutate(lat.deg = as.numeric(substr(lat.raw, 1, 2))) %>%
  mutate(lat.min = (1/60) * as.numeric(substr(lat.raw, 3, nchar(lat.raw)))) %>%
  mutate(lat = lat.deg + lat.min)%>%
  mutate(long.deg = as.numeric(substr(long.raw, 1,3))) %>%
  mutate(long.min = (1/60) * as.numeric(substr(long.raw, 4, nchar(long.raw)))) %>%
  mutate(long = -1 * (long.deg + long.min)) %>%
  select(c("time", "lat", "long"))
 
posData <- left_join(dateRead, posRead) %>%
  mutate(time = as.POSIXct(paste(time, day, month, year),
                           format = "%H%M%S %d %m %Y",
                           tz = "GMT"))

# Read and clean fluorometry file
startTime <- paste(substr(fluorFile, 7, 12),
                   substr(fluorFile, 14,19)) %>%
  as.POSIXct(format = "%y%m%d %H%M%S", tz = "GMT")

fluorData <- read.table(paste(dir, fluorFile, sep = "/"),
                      col.names = "fluor") %>%
  mutate(time = seq(from = startTime, length.out = length(.$fluor), by = 1/samplingRate)) %>%
  mutate(time = trunc(time, "secs") %>% as.POSIXct) %>%
  group_by(time) %>%
  summarize(fluor = mean(fluor)) %>%
  mutate(fluor = round(fluor, 0))

# Join data structures by time
fluorData <- left_join(posData, fluorData) %>%
  mutate(time = format(time, "%H:%M:%S")) %>%
  unite(date, year, month, day, sep = "-", remove= TRUE) %>%
  select(c(date, time, fluor, lat, long)) %>%
  slice(which(!is.na(.$fluor)) %>% 
      c(first(.):last(.)))
  

# Write to txt file
write.table(fluorData, 
            file = paste0(fluorFile%>% substr(1,19),
            "_PROCESSED.txt"),
            sep = ",",
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE)

