# Storm Petrel fluorometer processing script
# Loads raw fluorometer file and ship's NMEA data including time and position
# Writes new processed fluorometer file as .txt file with the following column format:

#   GMT DATE     GMT TIME     FLUOR   LAT    LONG
# (YYYY-MM-DD)  (HH:MM:SS)     (nm)   (deg)  (deg)

############################################################################################################
############################################################################################################

### Enter filenames to be processed
### Files must be located in your working directory
### Run getwd() to determine working directory if unknown

# Ship's position data file. Must be a .txt file containing NMEA sentences $GPZDA and $GPGGA

positionFile <- "SP_GPS_240509.cap"

# Fluorometry data file. Must be a .txt file containing fluorometry data
# Filename format must match: "SP_FL_YYMMDD_HHMMSS.txt" and reflect data start time
# Sampling rate is in units: n/second

fluorFile <- "SP_FL_240509_173128.txt"
samplingRate <- 9

############################################################################################################
############################################################################################################

# Check and load required package
if (!require('tidyverse')) {
  install.packages('tidyverse')
}

library(tidyverse)

# Get working directory to locate files
dir <- getwd()

# Fix duplicate GPS entries
fix_duplicate <- function(time) {
  truncate <- str_remove(time, pattern = "\\..*")
  convert <- as.POSIXct(truncate, format = "%H%M%S") + 1
  return <- as.character(convert, format = "%H%M%S")
  return(return)
}

# Read and clean ship's position data file
fileRead <- read.csv(paste(dir, positionFile, sep = "/"),
                     header = FALSE,
                     colClasses = "character")

dateRead <-fileRead %>%
  filter(V1 == "$GPZDA") %>%
  transmute(time = str_remove(V2, pattern = "\\..*"),
            day = V3,
            month = V4,
            year = V5) %>% 
  mutate(time = case_when(
    time == lag(time) ~ fix_duplicate(time),
    .default = time))

posRead <- fileRead %>%
  filter(V1 == "$GPGGA") %>%
  transmute(time = str_remove(V2, pattern = "\\..*"),
            lat = (as.numeric(substr(V3, 1, 2)) + 
                     as.numeric(substr(V3, 3, nchar(V3)))/60),
            long = -1 * (as.numeric(substr(V5, 1,3)) +
                           as.numeric(substr(V5, 4, nchar(V5)))/60))

posData <- right_join(dateRead, posRead) %>%
  mutate(time = as.POSIXct(paste(time, day, month, year),
                           format = "%H%M%S %d %m %Y",
                           tz = "GMT"))

# Read and clean fluorometry file
startTime <- paste(substr(fluorFile, 7, 12),
                   substr(fluorFile, 14,19)) %>%
  as.POSIXct(format = "%y%m%d %H%M%S", tz = "GMT")

fluorData <- read.table(paste(dir, fluorFile, sep = "/"),
                        col.names = "fluor") %>%
  mutate(time = seq(from = startTime,
                    length.out = length(.$fluor), 
                    by = 1/samplingRate) %>%
           trunc("secs") %>% 
           as.POSIXct) %>% 
  group_by(time) %>%
  summarize(fluor = mean(fluor)) %>%
  mutate(fluor = round(fluor, 0))

# Join data structures by time
fluorData <- merge(posData, fluorData, all = TRUE) %>%
  mutate(time = format(time, "%H:%M:%S")) %>%
  unite(date, year, month, day, sep = "-", remove= TRUE) %>%
  select(c(date, time, fluor, lat, long)) %>%
  slice(which(!is.na(.$fluor) %>% 
          c(first(.):last(.))))
  
# Write to txt file
write.table(fluorData, 
            file = paste0(fluorFile%>% substr(1,19),
            "_PROCESSED.txt"),
            sep = ",",
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE)

