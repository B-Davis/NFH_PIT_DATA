# Notes
# I originally included all Bon arrays including juvenile anennas, e.g., corner collector. Although some returning
# adults were detected at said arrays - they were also detected at adult arrays Bon1-4. Ergo, juvenile arrays were
# discluded from this query to reduce download times.

# WS_Bon currently runs every night at 01:00 am
# A list of query parameters for WS_Bon.csv can be found "NFH_PIT_DATA\PTAGIS\WS_Bon_Attributes.html"
# WS_Bonn_Ladders currently runs every morning at 07:30 am
# A list of query parameters for WS_Bonn_Ladders.csv can be found "NFH_PIT_DATA\PTAGIS\WS_Bonn_Ladders_Attributes.html"

rm(list = ls())
###########
### Data ###
############

# For Warm Springs releases, all fish detected at Bonneville before June 1 the year they were released are considered juvenile detections. Fish detected after June 30 in adult ladders the year they were released are considered minijacks. Fish detected at Bonneville in the juvenile bypass or the corner collector between June 1 - June 30 should be considered juveniles.

# Read in data from PTAGIS
# tmp <- httr::GET("https://api.ptagis.org/reporting/reports/brook/file/WS_Bonn_Ladders.csv")
tmp <- httr::GET("https://api.ptagis.org/reporting/reports/brian_davis%40fws.gov/file/WS_Bon.csv")
bin <- httr::content(tmp, "raw")
writeBin(bin, "data/WarmSprings/Bonnydownload.csv") # pre massage

# Script to ensure file is downloaded beofore reading in and massaging
# Not sure this is necessary, maybe test later to see if needed
while(!file.exists("data/WarmSprings/Bonnydownload.csv")){
    Sys.sleep(.1)
}
file.rename("data/WarmSprings/Bonnydownload.csv","data/WarmSprings/Bonny.csv")


########################################################################

####################################
### PTAGIS Grab by Year and Site ###
####################################
# Example search by Code
# https://api.ptagis.org/data/events/observation?apiKey=7C769240-0CC7-4940-B065-E326880AD4C8&tagCode=3DD.003D82A703

# Use swagger UI (https://api.ptagis.org/index.html; need "key") to figure out formatting
# key <- "7C769240-0CC7-4940-B065-E326880AD4C8"

# *************************
# site <- "WSH" # site code
# year <- format(Sys.Date(), "%Y") # current year
# url <- paste0("https://api.ptagis.org/data/events/observation/site/",site,"/year/",year,"?apiKey=7C769240-0CC7-4940-B065-E326880AD4C8")
# a <- GET(url)
# out <- fromJSON(rawToChar(a$content))
# # names(a)
# # a$header
# dtm <- as.POSIXct(a$date) # pull "data grab" date/time
# tz <- Sys.timezone()
# attr(dtm, "tzone") <- tz # Change from Greenwich Mean Time (GMT) to Pacific
# dtm <- format(dtm,"%D %T %Z") # format to layperson style

# head(out)
# tail(out)

# # Retrieve PIT detection data from Brook's PTAGIS database query, automatically updated daily at 7:30 am
# url <- "https://api.ptagis.org/reporting/reports/brook/file/Complex_Interrogation_Summary.csv"
# path <- "data"
# file <- paste (path, "/PTAGIS_Download_", current_date, ".csv", sep="")
# curl_download(url, file) 
# *************************

