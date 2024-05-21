#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# library(shiny)
source("./global.R")

function (input,output){

output$histWS <- renderPlot({
  col <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")[-c(4,5)][1:4]
  par(mar = c(7,4,4,2) + .01)
  dta <- A_hist[,,input$histyear]
  yr <- as.integer(input$histyear)
  x <- barplot(dta,las = 2,beside = FALSE,col = col, xaxt = "n", ylim = c(0,40))
  text(cex=1, x=x-.8, y=-5.25, colnames(dta), xpd=TRUE, srt=45)
  legend("topright",sprintf("Brood Year-%s",yr -c(0,1,2,3)),fill = col,bty = "n")
  title(xlab = "Week", line = 4.5); title(ylab = "Count", main = paste0("Bonneville Dam Detections\n",yr))
})

}

