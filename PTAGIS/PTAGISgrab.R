# Notes
# I originally included all Bon arrays including juvenile anennas, e.g., corner collector. Although some returning
# adults were detected at said arrays - they were also detected at adult arrays Bon1-4. Ergo, juvenile arrays were
# discluded from this query to reduce download times.

# WS_Bon currently runs every night at 01:00 am
# A list of query parameters for WS_Bon.csv can be found "NFH_PIT_DATA\PTAGIS\WS_Bon_Attributes.html"
# Didn't use Brook's subscription because it had too much info (other hatcheries etc.)
# These parameters will likely need to be changed and massaged differently based on Brook's input.

rm(list = ls())
###########
### Data ###
############

# Before June 30 considered outmigrating juv 

# Read in data from PTAGIS
tmp <- httr::GET("https://api.ptagis.org/reporting/reports/brian_davis%40fws.gov/file/WS_Bon.csv")
bin <- httr::content(tmp, "raw")
writeBin(bin, "data/WarmSprings/Bonny.csv") # pre massage

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
d$DOY <- as.integer((format(d$Last.Time,"%j"))) # Day of Year
# Another observation date variable, but all same year for plotting purposes (derived from day-of-year)
d$Dts_sameyear <- as.POSIXct(d$DOY*24*60**2,format = "%j",origin = as.POSIXct(paste0(currYr - 1,"-12-31")))


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
A_hist

### Cumalitive ###

L_cum <- list()
for(i in seq_along(nmsYear)){
    tmp <- d[d$ObsYear == nmsYear[i],]
    L_cum[[nmsYear[i]]] <- cumsum(table(tmp$Dts_sameyear))
}


save("L_cum","A_hist","DtRng", file = "data/WarmSprings/WSdata.Rdata")
rm(list = ls())


########################################################################



# How long until adult ladder detection is a mini jack, e.g., 1 month from release is juvy, but 3 months is returner?
# a <- d[with(d, ObsYear > RelYear) & d$Site == "BCC - BON PH2 Corner Collector", "Tag"]
# Tags last seen at corner collector

col <- ifelse(d$Age == 0,"red","black")
plot(d$DOY,col = col)

# Are they all mini jacks if detected in adult ladder?
# CC <- c("3D9.1C2D76EC1A", "3D9.1C2DD63C95", "3D9.1C2DD657CB", "3D9.1C2DD667A4", 
# "3DD.003D6A3BA6", "3DD.0077492EA9", "3DD.0077818CCE")



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

# current_date <- as.Date(format(Sys.Date()))

# # Retrieve Release data from csv file (summarized from CRiS), UPDATE ANNUALLY!
# release_data <- read_csv("Data/Releases.csv") %>% 
#   mutate(Ratio = Released/PIT.Tagged)

# # Retrieve PIT detection data from Brook's PTAGIS database query, automatically updated daily at 7:30 am
# url <- "https://api.ptagis.org/reporting/reports/brook/file/Complex_Interrogation_Summary.csv"
# path <- "data"
# file <- paste (path, "/PTAGIS_Download_", current_date, ".csv", sep="")
# curl_download(url, file) 

# # Format date and standardize stock names to indicate program and origin
# ptagis_data <- read.csv(file, fileEncoding="UTF-16LE", stringsAsFactors = FALSE) %>%
#   mutate(Last.Date = as.Date(Last.Date, format = "%m/%d/%Y"),
#          Age = year(Last.Date) - Brood.Year,
#          Last.Year = year(Last.Date),
# # Add stock to blank fields (First four letters of Mark Site), beware of fish marked at other hatcheries or moved before their release like Warm Springs
#          Stock = ifelse(
#            Stock == "", toupper(substr(Mark.Site.Info.Name, 1, 4)), 
#            substr(Stock, 1, 4)),
#         Stock = recode(Stock, "WARM" = "WSPH"),# Warm Springs stock was entered incorrectly, replaced it with PTAGIS's stock name
#         Stock = recode(Stock, "SPRI" = "SPRC"),# Spring Creek stock was entered incorrectly, replaced it with PTAGIS's stock name
#         Stock = recode(Stock, "SPRING CREEK" = "SPRC"), 
#         Stock = recode(Stock, "TFCS" = "SPRC"), # Spring Creek Rikeem's Experimental stock
#         Stock = recode(Stock, "ROUN" = "ROBU"),# Stock was entered incorrectly, replaced it with PTAGIS's stock name
#         Stock = recode(Stock, "DES" = "ROBU"),
#         Stock = recode(Stock, "SMAL" = "ROBU"), # Stock was entered smalls batch of fish, replaced it with PTAGIS's stock name
#         Stock = recode(Stock, "BIGS" = "ROBU"), # Stock was entered bigs batch of fish, replaced it with PTAGIS's stock name
# # In 2022, Round Butte fish were marked in the Pelton Ladder        
#         Release.Site = recode(Release.Site, "PELTON - Pelton Ladder (Deschutes River) Acclimation Pond" = "ROBU - Round Butte Hatchery"), 
# # Willard program began with Carson stock
#         Stock = ifelse(Mark.Site.Info.Name == "Willard National Fish Hatchery", recode(Stock, "CARS" = "WILL"),  Stock)) 

# # Transform data to add expansion calculation
# expanded_adult_return <- left_join(ptagis_data, release_data, by = c("Release.Site", "Brood.Year", "Run.Name", "Stock"))  %>% 
#   ## Filter out fish that were released and detected in the same year 
#   filter(Run.Name == "Spring" & Age >2 | Run.Name == "Fall" & Age > 1) %>% 
#   distinct(Tag, .keep_all = TRUE) %>% # Make sure there are no duplicates
#   mutate(Est.Over.BONN = Count * Ratio) %>% # Calculate expansion
#   group_by(Last.Year, 
#            Mark.Site.Name, 
#            Release.Site, 
#            Run.Name, 
#            Brood.Year, 
#            Species.Name, 
#            Rear.Type.Name, 
#            Age, 
#            PIT.Tagged, 
#            Released, 
#            Ratio, 
#            Stock) %>%
#   summarise(Unique.Tags.Detected = sum(Count) , Est.Over.Bonneville= sum(Est.Over.BONN)) %>% 
#   ungroup() 
# ****************************************************************

# Calculate 95% Confidence Intervals using the binomial distribution
  ci <- as.data.frame(BinomCI(expanded_adult_return$Unique.Tags.Detected, expanded_adult_return$Est.Over.Bonneville, conf.level = 0.95,  method = "clopper-pearson"))

  expanded_adult_return <- bind_cols(expanded_adult_return,ci)
  expanded_adult_return$LowerCI <- expanded_adult_return$Unique.Tags.Detected/expanded_adult_return$upr.ci
  expanded_adult_return$UpperCI <- expanded_adult_return$Unique.Tags.Detected/expanded_adult_return$lwr.ci

## Arrange by run and age
  expanded_adult_return <- expanded_adult_return %>% 
  group_by(Release.Site, Run.Name) %>% 
  arrange(Run.Name, Mark.Site.Name, Age) %>% 
    ungroup()

  
  # Import PTAGIS File for cumulative returns
  url <- "https://api.ptagis.org/reporting/reports/brook/file/SCS_Adult_BONN_Gorge%20Complex%20and%20Round%20Butte.csv"
  path <- "data"
  file <- paste (path, "/PTAGIS_Returns_Download_", current_date, ".csv", sep="")
  curl_download(url, file)  
 
   adult_returns <- read.csv(file, header=TRUE, fileEncoding="UTF-16LE", stringsAsFactors = FALSE) %>% 
    mutate(Last.Obs.Date.Min = as.Date(Last.Obs.Date.Min, "%m/%d/%Y"),
           Year = format(Last.Obs.Date.Min, format="%Y"),
           Day = format(Last.Obs.Date.Min, format="%m/%d"),
           Interval = case_when(
             Day <= "03/15" ~ "01.March-15",
             Day <= "03/31" ~ "02.March-31",
             Day <= "04/15" ~ "03.April-15",
             Day <= "04/30" ~ "04.April-30",
             Day <= "05/15" ~ "05.May-15",
             Day <= "05/31" ~ "06.May-31",
             Day <= "06/15" ~ "07.June-15",
             Day <= "06/30" ~ "08.June-30",
             Day <= "07/15" ~ "09.July-15",
             Day <= "07/31" ~ "10.July-31",
             Day <= "08/15" ~ "11.August-15",
             Day >= "08/16" ~ "12.August-16+")) 
  
  ## Percent Cumulative Return - By Hatchery Release
  
  # Create a loop to summarize average cumulative return by hatchery to plot in one figure
  Hatchery <- adult_returns %>% # Create a data frame that holds the hatchery names
    distinct(Mark.Site.Name)
  
  # Create an empty data frame to hold loop data
  adult_returns2 <- data.frame()
  
  # For Troubleshooting loop, replaces i with Warm Springs Hatchery
  i=4 
  
  # The LOOP!
  for (i in 1:nrow(Hatchery)) {
    adult_returns1 <- adult_returns %>% 
      filter(Mark.Site.Name == Hatchery[i,]) %>% # Filter by Hatchery
      group_by(Year, Interval) %>% # Groups the annual return into two-week intervals
      mutate(Return = sum(Unique.Tags)) %>% # The total number of returns in the two-week interval each year
      select(1, 18, 20, 21) %>% # Clean up data frame, we don't need all these variables
      unique() %>% # We don't need duplicates
      group_by(Year)%>% # Calculate the cumulative return by two-week interval each year
      arrange(Interval) %>% # Make sure two-week intervals are in order for the cumulative return calculation
      mutate(Cumulative_Return = cumsum(Return), # Calculate the cumulative return for each interval, each year
             Annual_Total = sum(Return), # Calculate total return each year
             Perc_Cum_Return = Cumulative_Return/Annual_Total * 100) %>% # Calculate the percent of the annual return for each interval, each year
      group_by(Interval) %>% # Scale up to summarize across years
      na.omit() %>% # If there were no returns for an interval, drop the row of data
      summarise(Average_Perc_Cum_Ret = mean(Perc_Cum_Return),# Average the cumulative return by two-week interval across years
                sd = sd(Perc_Cum_Return), # Standard deviation - how widely scattered the averages are across years
                se =sd/sqrt(length(Perc_Cum_Return))) %>%  # Standard error - how far the average is to be from the true mean (if we are interested in evaluating 95% confidence intervals)
      mutate(Hatchery = Hatchery[i,])%>% # Add a column to indicate what hatchery this is summarizing
      separate_wider_delim(Interval, delim = ".", names = c("Interval", "End Date"))  # Tidy Interval Labels for plotting
    
    adult_returns2 <- bind_rows(adult_returns2, adult_returns1) # Put the output of each Hatchery summary into one data frame so we can plot it
    
  }

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

