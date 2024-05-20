load("data/WarmSprings/WSdata.Rdata") # Warm Springs plotting data
#################
### Histogram ###
#################

col <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")[-c(4,5)]
par(mar = c(7,4,4,2) + .01)
x <- barplot(A_hist[,,"2023"],las = 2,beside = FALSE,col = col, ylim = c(0,40))
legend("topright",sprintf("Age-%s",0:4),fill = col[1:5],bty = "n")

##################
### Cumalitive ###
##################
brks <- seq(DtRng[1],DtRng[2],by = "day")
atlbl <- seq(DtRng[1],DtRng[2],by = "month")
lbl <- format(atlbl,"%m/%d")
tmp <- max(sapply(L_cum,max))
y_lbls <- pretty(c(0,tmp),n = 10)


# plot f()
f_cum <- \(yr) lines(as.POSIXct(names(L_cum[[yr]])),L_cum[[yr]])

# plot
plot.new(); plot.window(range(brks),c(0,max(y_lbls)))
axis(1,atlbl,lbl); axis(2,y_lbls,las = 2)

f_cum("2023")

sapply(c("2014","2018","2022"),f_cum)
