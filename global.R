library(shiny)
# source("./data/WarmSprings/massage.R") # Remove this when app launches (or done with WS tab)
WS <- new.env()
load("data/WarmSprings/WSdata.Rdata",env = WS) # Warm Springs plotting data
# for(i in 1:length(L_Plot_cum)) assign(names(L_Plot_cum)[i], L_Plot_cum[[i]])


