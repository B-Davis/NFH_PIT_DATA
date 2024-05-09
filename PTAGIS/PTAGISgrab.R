# install.packages(c("httr", "jsonlite"), repos='http://cran.us.r-project.org', dependencies=TRUE)
# install.packages("lubridate", repos='http://cran.us.r-project.org', dependencies=TRUE)
# install.packages("radian", repos='http://cran.us.r-project.org', dependencies=TRUE)
# install.packages("shinythemes", repos='http://cran.us.r-project.org', dependencies=TRUE)

library(httr)
library(jsonlite)
library(curl)
library(dplyr)
library(readr)
library(lubridate)

current_date <- as.Date(format(Sys.Date()))

# Retrieve Release data from csv file (summarized from CRiS), UPDATE ANNUALLY!
release_data <- read_csv("Data/Releases.csv") %>% 
  mutate(Ratio = Released/PIT.Tagged)

# Retrieve PIT detection data from Brook's PTAGIS database query, automatically updated daily at 7:30 am
url <- "https://api.ptagis.org/reporting/reports/brook/file/Complex_Interrogation_Summary.csv"
path <- "data"
file <- paste (path, "/PTAGIS_Download_", current_date, ".csv", sep="")
curl_download(url, file) 

# Format date and standardize stock names to indicate program and origin
ptagis_data <- read.csv(file, fileEncoding="UTF-16LE", stringsAsFactors = FALSE) %>%
  mutate(Last.Date = as.Date(Last.Date, format = "%m/%d/%Y"),
         Age = year(Last.Date) - Brood.Year,
         Last.Year = year(Last.Date),
# Add stock to blank fields (First four letters of Mark Site), beware of fish marked at other hatcheries or moved before their release like Warm Springs
         Stock = ifelse(
           Stock == "", toupper(substr(Mark.Site.Info.Name, 1, 4)), 
           substr(Stock, 1, 4)),
        Stock = recode(Stock, "WARM" = "WSPH"),# Warm Springs stock was entered incorrectly, replaced it with PTAGIS's stock name
        Stock = recode(Stock, "SPRI" = "SPRC"),# Spring Creek stock was entered incorrectly, replaced it with PTAGIS's stock name
        Stock = recode(Stock, "SPRING CREEK" = "SPRC"), 
        Stock = recode(Stock, "TFCS" = "SPRC"), # Spring Creek Rikeem's Experimental stock
        Stock = recode(Stock, "ROUN" = "ROBU"),# Stock was entered incorrectly, replaced it with PTAGIS's stock name
        Stock = recode(Stock, "DES" = "ROBU"),
        Stock = recode(Stock, "SMAL" = "ROBU"), # Stock was entered smalls batch of fish, replaced it with PTAGIS's stock name
        Stock = recode(Stock, "BIGS" = "ROBU"), # Stock was entered bigs batch of fish, replaced it with PTAGIS's stock name
# In 2022, Round Butte fish were marked in the Pelton Ladder        
        Release.Site = recode(Release.Site, "PELTON - Pelton Ladder (Deschutes River) Acclimation Pond" = "ROBU - Round Butte Hatchery"), 
# Willard program began with Carson stock
        Stock = ifelse(Mark.Site.Info.Name == "Willard National Fish Hatchery", recode(Stock, "CARS" = "WILL"),  Stock)) 

# Transform data to add expansion calculation
expanded_adult_return <- left_join(ptagis_data, release_data, by = c("Release.Site", "Brood.Year", "Run.Name", "Stock"))  %>% 
  ## Filter out fish that were released and detected in the same year 
  filter(Run.Name == "Spring" & Age >2 | Run.Name == "Fall" & Age > 1) %>% 
  distinct(Tag, .keep_all = TRUE) %>% # Make sure there are no duplicates
  mutate(Est.Over.BONN = Count * Ratio) %>% # Calculate expansion
  group_by(Last.Year, 
           Mark.Site.Name, 
           Release.Site, 
           Run.Name, 
           Brood.Year, 
           Species.Name, 
           Rear.Type.Name, 
           Age, 
           PIT.Tagged, 
           Released, 
           Ratio, 
           Stock) %>%
  summarise(Unique.Tags.Detected = sum(Count) , Est.Over.Bonneville= sum(Est.Over.BONN)) %>% 
  ungroup() 


# vector
# matrix
# array
# list
# data frame

####################################
### PTAGIS Grab by Year and Site ###
####################################
# Example search by Code
# https://api.ptagis.org/data/events/observation?apiKey=7C769240-0CC7-4940-B065-E326880AD4C8&tagCode=3DD.003D82A703

# Use swagger UI (https://api.ptagis.org/index.html; need "key") to figure out formatting
# key <- "7C769240-0CC7-4940-B065-E326880AD4C8"
site <- "WSH" # site code
year <- format(Sys.Date(), "%Y") # current year
url <- paste0("https://api.ptagis.org/data/events/observation/site/",site,"/year/",year,"?apiKey=7C769240-0CC7-4940-B065-E326880AD4C8")
a <- GET(url)
out <- fromJSON(rawToChar(a$content))
# names(a)
# a$header
dtm <- as.POSIXct(a$date) # pull "data grab" date/time
tz <- Sys.timezone()
attr(dtm, "tzone") <- tz # Change from Greenwich Mean Time (GMT) to Pacific
dtm <- format(dtm,"%D %T %Z") # format to layperson style

head(out)
tail(out)

# Retrieve PIT detection data from Brook's PTAGIS database query, automatically updated daily at 7:30 am
url <- "https://api.ptagis.org/reporting/reports/brook/file/Complex_Interrogation_Summary.csv"
path <- "data"
file <- paste (path, "/PTAGIS_Download_", current_date, ".csv", sep="")
curl_download(url, file) 



search()
library(curl)
ls(2)
ls()
