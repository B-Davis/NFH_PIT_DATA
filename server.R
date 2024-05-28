#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# library(shiny)
library(dplyr)

function (input,output){

output$histWS <- renderPlot({
  col <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")[-c(4,5)][1:4]
  par(mar = c(4,6,3,1) + .01, oma = c(4,2,1,1), mfrow = c(2,1))
  dta <- A_hist[,,input$histyear]
  yr <- as.integer(input$histyear)

  # Cumulative
plot.new(); plot.window(range(brksday),c(0,max(y_lbls)))
axis(1,atlbl,lbl); axis(2,y_lbls,las = 2)
sapply(TenRg, \(x) lines(brksday,M_cum[,x],col = adjustcolor("grey80",.5),lwd = 4))
lines(xx, predict(fit, newdata = data.frame(x = xx)),col = adjustcolor("red",.2),lwd = 4)
f_cum(input$histyear, lwd = 3, lty = 2)
axis(1,currday,"", col = adjustcolor("red",.3),line = 0,lwd = 1.5,tck = -.15,lty = 3)
text(currday,-60,"Today",col = adjustcolor("red",.3),xpd = NA)
title(main = paste0("Bonneville Dam Detections\n",yr),ylab = "Cumalitive Count")
legend("topright",c("Ten year data","Ten year model average",yr),col = c(adjustcolor(c("grey80","red"),.5),1),lty = c(1,1,3),lwd = c(4,4,3),bty = "n",box.lty = 2)

# Histogram  
  # par(mar = c(7,4,4,2) + .01)
  coltxt <- rep(1,ncol(dta)); coltxt[currWk] <- 2
  x <- barplot(dta,las = 2,beside = FALSE,col = col, xaxt = "n", ylim = c(0,40))
  text(cex=1, x=x-.8, y=-7, colnames(dta), xpd=TRUE, srt=45, col = coltxt)
  legend("topright",sprintf("Brood Year-%s",yr -c(0,1,2,3)),fill = col,bty = "n")
  title(xlab = "Week", line = 4.5); title(ylab = "Count")
  mtext("Week",1,line = 1,outer = T)
})



# Expansion Table
output$expansionWS <- renderTable({
  L_ws_expansion[[input$histyear]]
})



}

