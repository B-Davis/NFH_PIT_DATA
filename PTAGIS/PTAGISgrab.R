# install.packages(c("httr", "jsonlite"), repos='http://cran.us.r-project.org', dependencies=TRUE)
# install.packages("lubridate", repos='http://cran.us.r-project.org', dependencies=TRUE)
# install.packages("radian", repos='http://cran.us.r-project.org', dependencies=TRUE)

library(httr)
library(jsonlite)

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
