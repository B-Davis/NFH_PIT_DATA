
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
# unique(d$RelYear) # Ask Brook about 2069??? I knew once, but forgot
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
# A_hist[A_hist == 0] <- NA
tmp <- Sys.Date() |> as.POSIXct() |> cut(,breaks = brks) |> table() |> as.vector() == 1
currday <- Sys.Date() |> as.POSIXct()
currWk <- which(tmp)



### Cumalitive ###
# Matrix
brksday <- seq(DtRng[1],DtRng[2],by = "day") ###
f <- \(x) d[d$ObsYear == x,"Dts_sameyear"] |> cut(breaks = brksday) |> table() |> cumsum()
M_cum <- sapply(nmsYear,f)
i_mx <- apply(M_cum,2,max)
for(i in seq_along(i_mx)) M_cum[which(M_cum[,i]==i_mx[i])[-1],i] <- NA # Remove totals after last detection
M_cum <- rbind(M_cum,NA)
rownames(M_cum) <- brksday
M_cum[M_cum == 0] <- NA

# Plotting
brksday <- seq(DtRng[1],DtRng[2],by = "day") ###
atlbl <- seq(DtRng[1],DtRng[2],by = "month") ###
lbl <- format(atlbl,"%m/%d") ###
tmp <- max(M_cum,na.rm = TRUE); tmp <- 220
y_lbls <- pretty(c(0,tmp),n = 10) ###
TenRg <- as.character((currYr - 10):(currYr - 1)) # previous ten years ###
# plot f()
f_cum <- \(yr,...) lines(brksday,M_cum[,yr],...) ###
# Ten Year Average
tmp <- as.vector(M_cum)
tmp2 <- rep(brksday,ncol(M_cum))
tmp <-matrix(c(tmp2,tmp),ncol = 2)
TenDta <- as.data.frame(tmp[complete.cases(tmp),])
colnames(TenDta) <- c("x","y")
FnL <- apply(M_cum,2,\(x) c(which.min(x),which.max(x))) |> apply(1,mean) |> round()# median first and last detection
Ten_i <- which(TenDta$x >= brksday[FnL[1]] & TenDta$x <= brksday[FnL[2]])
# with(TenDta,points(y~x, col = adjustcolor("grey80",.4),pch = 16,cex = 1))
x <- sort(unique(TenDta$x[Ten_i])) # x for running model
xx <- sort(unique(TenDta$x)) # x for plotting ###
fit <- nls(y ~ SSlogis(x, Asym, xmid, scal), data = TenDta[Ten_i,]) ###

L_Plot_cum <- list("brksday" = brksday,"atlbl" = atlbl,"lbl" = lbl,"y_lbls" = y_lbls,"TenRg" = TenRg,"xx" = xx,"TenRg"=TenRg)

# tapply(d$Tag, list(d$ObsYear,d$Age),length)

save("M_cum","A_hist","DtRng","currYr", "currWk","currday","df", "L_Plot_cum","fit","f_cum",file = "data/WarmSprings/WSdata.Rdata")
rm(list = ls())
# load("data/WarmSprings/WSdata.Rdata")
ls()
