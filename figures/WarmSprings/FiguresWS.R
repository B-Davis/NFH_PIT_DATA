load("data/WarmSprings/WSdata.Rdata") # Warm Springs plotting data
#################
### Histogram ###
#################


col <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")[-c(4,5)]
par(mar = c(7,4,4,2) + .01)
x <- barplot(A_hist[,,"2015"],las = 2,beside = FALSE,col = col, ylim = c(0,40),add = F)
legend("topright",sprintf("Age-%s",0:4),fill = col[1:5],bty = "n")

# brks <- seq(DtRng[1],DtRng[2],by = "week")


# tmp[(which(tmp == 1)+1):length(tmp)] <- 40; tmp[tmp == 1] <- 0
# barplot(tmp, col = "grey85",border = F,las = 2,beside = FALSE, ylim = c(0,40),xaxt = "n",yaxt = "n",space = 0)
  col <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")[-c(4,5)][1:4]
  par(mar = c(7,4,4,2) + .01, mfrow = c(2,1))
  dta <- A_hist[,,"2016"]
  coltxt <- rep(1,ncol(dta)); coltxt[currWk] <- 2
  yr <- as.integer("2016")
  x <- barplot(dta,las = 2,beside = FALSE,col = col, xaxt = "n", ylim = c(0,40))
  text(cex=1, x=x-.8, y=-5.25, colnames(dta), xpd=TRUE, srt=45, col = coltxt)
  legend("topright",sprintf("Brood Year-%s",yr -c(0,1,2,3)),fill = col,bty = "n")
  title(xlab = "Week", line = 4.5); title(ylab = "Count", main = paste0("Bonneville Dam Detections\n",yr))


##################
### Cumalitive ###
##################
# plot
for(i in 1:length(L_Plot_cum)) assign(names(L_Plot_cum)[i], L_Plot_cum[[i]])

plot.new(); plot.window(range(brksday),c(0,max(y_lbls)))
axis(1,atlbl,lbl); axis(2,y_lbls,las = 2)
sapply(TenRg, \(x) lines(brksday,M_cum[,x],col = adjustcolor("grey80",.5),lwd = 4))
lines(xx, predict(fit, newdata = data.frame(x = xx)),col = adjustcolor("red",.2),lwd = 4)
f_cum("2016", lwd = 3, lty = 2)
axis(1,currday,"", col = adjustcolor("red",.3),line = 0,lwd = 1.5,tck = -.15,lty = 3)
text(currday,-60,"Today",col = adjustcolor("red",.3),xpd = NA)
# legend()


M_cum
A_hist
