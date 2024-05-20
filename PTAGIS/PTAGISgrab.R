# Notes
# I originally included all Bon arrays including juvenile anennas, e.g., corner collector. Although some returning
# adults were detected at said arrays - they were also detected at adult arrays Bon1-4. Ergo, juvenile arrays were
# discluded from this query to reduce download times.

# WS_Bon currently runs every night at 01:00 am
# A list of query parameters for WS_Bon.csv can be found "NFH_PIT_DATA\PTAGIS\WS_Bon_Attributes.html"
# Didn't use Brook's subscription because it had too much info (other hatcheries etc.)
# These parameters will likely need to be changed and massaged differently based on Brook's input.

# rm(list = ls())
###########
### Data ###
############

# For Warm Springs releases, all fish detected at Bonneville before June 1 the year they were released are considered juvenile detections. Fish detected after June 30 in adult ladders the year they were released are considered minijacks. Fish detected at Bonneville in the juvenile bypass or the corner collector between June 1 - June 30 should be considered juveniles.

# Read in data from PTAGIS
tmp <- httr::GET("https://api.ptagis.org/reporting/reports/brian_davis%40fws.gov/file/WS_Bon.csv")
bin <- httr::content(tmp, "raw")
writeBin(bin, "data/WarmSprings/Bonnydownload.csv") # pre massage

# Script to ensure file is downloaded beofore reading in and massaging
# Not sure this is necessary, maybe test later to see if needed
while(!file.exists("data/WarmSprings/Bonnydownload.csv")){
    Sys.sleep(.1)
}
file.rename("data/WarmSprings/Bonnydownload.csv","data/WarmSprings/Bonny.csv")

###############
### Massage ###
###############

d <- read.csv("data/WarmSprings/Bonny.csv",fileEncoding = "UTF16LE")
currYr <- as.integer(format(Sys.Date(),"%Y"))

# Massage dates and add year columns
d$Release.Date <- as.POSIXct(d$Release.Date, format = "%m/%d/%Y")
d$Last.Time <- as.POSIXct(d$Last.Time, format = "%m/%d/%Y %H:%M:%S ")
d$ObsYear <- as.integer(format(d$Last.Time,"%Y"))
d$RelYear <- as.integer(format(d$Release.Date,"%Y"))
unique(d$RelYear) # Ask Brook about 2069??? I knew once, but forgot
d <- d[-which(d$RelYear == 2069),] # remove records for now
# Remove duplicate tags (same tag, different antennas); keeping most recent Bonneville observation
d <- d[order(d$Last.Time, decreasing = TRUE),]
d <- d[-which(duplicated(d[,c("Tag","ObsYear")])),]
d <- d[order(d$Last.Time, decreasing = FALSE),] # reorder firt time observed -> last
# Age variable (observation year - release year)
d$Age <- with(d, ObsYear - RelYear)
d$DOY <- as.integer(format(d$Last.Time,"%j")) # Day of Year
# Another observation date variable, but all same year for plotting purposes (derived from day-of-year)
d$Dts_sameyear <- as.POSIXct(d$DOY*24*60**2,format = "%j",origin = as.POSIXct(paste0(currYr - 1,"-12-31")))
d$Cutoff <- as.POSIXct(paste0(d$ObsYear,"-06-30")) # Cutoff Date (juv or mini-jack detected in adult ladder)
i <- which(d$Age == 0 & d$Cutoff > d$Last.Time)
d <- d[-i,] # Remove Juvenile observations 


head(d)
##############
### Output ###
##############

### Histogram ###
# Create weekly bins for histograms
DtRng <- sprintf(paste0(currYr,"-0%s-01"),c(3,9)) |> as.POSIXct()
brks <- seq(DtRng[1],DtRng[2],by = "week")
tmp <- format(brks,"%m/%d")
brksNms <- paste(tmp[-length(tmp)],tmp[-1],sep = "-")

nmsAge <- as.character(0:4)
nmsYear <- as.character(unique(d$ObsYear))
A_hist <- array(0,dim = c(5,length(brks)-1,length(unique(d$ObsYear))),dimnames = list(nmsAge,brksNms,nmsYear))
for(j in nmsYear){
tmp <- d[d$ObsYear == j,c("Age","Dts_sameyear")]
tmp2 <- split(tmp[[2]],tmp[[1]])
for(i in nmsAge){
    tryCatch({
    A_hist[i,,j] <- table(cut(tmp2[i][[1]],breaks = brks))
},error = \(e){})
}
}
# A_hist

### Cumalitive ###

L_cum <- list()
for(i in seq_along(nmsYear)){
    tmp <- d[d$ObsYear == nmsYear[i],]
    L_cum[[nmsYear[i]]] <- cumsum(table(tmp$Dts_sameyear))
}



tapply(d$Tag, list(d$ObsYear,d$Age),length)

save("L_cum","A_hist","DtRng","currYr", file = "data/WarmSprings/WSdata.Rdata")
rm(list = ls())

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

