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
tmp <- httr::GET("https://api.ptagis.org/reporting/reports/brook/file/WS_Bonn_Ladders.csv")
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
d$yrsSeen <- with(d, ObsYear - RelYear)
d$Age <- with(d, ObsYear - Brood.Year)
d$DOY <- as.integer(format(d$Last.Time,"%j")) # Day of Year
# Another observation date variable, but all same year for plotting purposes (derived from day-of-year)
d$Dts_sameyear <- as.POSIXct(d$DOY*24*60**2,format = "%j",origin = as.POSIXct(paste0(currYr - 1,"-12-31")))
d$Cutoff <- as.POSIXct(paste0(d$ObsYear,"-06-30")) # Cutoff Dates are June 1  - June 30 (juveniles are detected in adult ladder before June 1, minijacks if detected after June 30)
i <- which(d$Age <= 2 & d$Cutoff > d$Last.Time) # Spring Chinook are released at Age-2 and are considered minijacks if they return upstream at Age-2. Jacks are Age-3.
d <- d[-i,] # Remove Juvenile observations 

head(d)

## Format Stock Names to join with Releases csv
library(dplyr)
d1 <- d %>% 
  mutate(Stock.Name = case_when(
    Stock == "WARM SPRINGS" ~ "WSPH",
    Stock == "ROUND BUTTE" ~ "ROBU",
    Stock == "PARKDALE" ~ "PARK",
    TRUE ~ Stock ))  %>%  # Keep the original value if none of the conditions match  
  rename(
    "Brood.Year.YYYY" = "Brood.Year",
    "Mark.Site.Name" = "Mark.Site",
    "Release.Site.Name" = "Release.Site")

## Import Releases file and join with d
d2 <- read.csv("data/Releases.csv") %>% 
  mutate(Expansion = Released/PIT.Tagged) 

df <- left_join(d1,d2, by = c("Mark.Site.Name", "Release.Site.Name", "Brood.Year.YYYY", "Run.Name", "Stock.Name"))

df <- df %>% 
  distinct(Tag, .keep_all = TRUE) %>% # Make sure there are no duplicates
  group_by(Tag) %>% 
  mutate(Unique.Tags = n(),
         Est.over.BONN = Unique.Tags * Expansion) %>% # Calculate expansion
  group_by(ObsYear, 
           Mark.Site.Name, 
           Release.Site.Name, 
           Run.Name, 
           Brood.Year.YYYY, 
           Species.Name, 
           Rear.Type.Name, 
           Age, 
           PIT.Tagged, 
           Released, 
           Expansion, 
           Stock.Name) %>%
  summarise(Unique.Tags.Detected = sum(Unique.Tags) , Est.Over.Bonneville= sum(Est.over.BONN)) %>% 
  ungroup() 

# Calculate 95% Confidence Intervals using the binomial distribution
ci <- as.data.frame(DescTools::BinomCI(df$Unique.Tags.Detected, df$Est.Over.Bonneville, conf.level = 0.95,  method = "clopper-pearson"))

df <- bind_cols(df,ci) 
df$LowerCI <- df$Unique.Tags.Detected/df$upr.ci
df$UpperCI <- df$Unique.Tags.Detected/df$lwr.ci
  
df <- df %>% ## Arrange by run and age, rename fields to aid reading table
  group_by(Release.Site.Name, Run.Name) %>% 
  rename("Last Observation Year" = ObsYear,
         "Hatchery" = Mark.Site.Name,
         "Release Site"= Release.Site.Name,
         "Run" = Run.Name,
         "Brood Year" = Brood.Year.YYYY,
         "Species" = Species.Name,
         "Rear" = Rear.Type.Name,
         "# PIT Tagged" = PIT.Tagged,
         "# Released" = Released,
         "Expansion (Rel/PIT)" = Expansion, 
         "Stock" = Stock.Name,
         "Unique Tags Detected" = Unique.Tags.Detected,
         "# Adults Estimated Over Bonneville" = Est.Over.Bonneville,
         "Lower CI" = LowerCI,
         "Upper CI" = UpperCI) %>% 
  arrange(Run, Hatchery, Age)  

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
tmp <- d[d$ObsYear == j,c("yrsSeen","Dts_sameyear")]
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
    tmp <- d[d$ObsYear == nmsYear[i],"Dts_sameyear"]
    L_cum[[nmsYear[i]]] <- cumsum(table(tmp))
}

# brksday <- seq(DtRng[1],DtRng[2],by = "day")

# a <- lines(as.POSIXct(names(L_cum[["2023"]])),L_cum[["2023"]])


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

