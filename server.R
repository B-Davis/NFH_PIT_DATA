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
library(ggplot2)

function (input,output){

output$histWS <- renderPlot({
col <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")[-c(4,5)][1:4]
par(mar = c(4,6,3,1) + .01, oma = c(4,2,1,1), mfrow = c(2,1))
dta <- WS$A_hist[,,input$histyear]
yr <- as.integer(input$histyear)

# Cumulative
plot.new(); plot.window(range(WS$brksday),c(0,max(WS$y_lbls)))
axis(1,WS$atlbl,WS$lbl); axis(2,WS$y_lbls,las = 2)
sapply(WS$TenRg, \(x) lines(WS$brksday,WS$M_cum[,x],col = adjustcolor("grey80",.5),lwd = 4))
lines(WS$xx, predict(WS$fit, newdata = data.frame(x = WS$xx)),col = adjustcolor("red",.2),lwd = 4)
WS$f_cum(input$histyear, lwd = 3, lty = 2)
axis(1,WS$currday,"", col = adjustcolor("red",.3),line = 0,lwd = 1.5,tck = -.15,lty = 3)
text(WS$currday,-60,"Today",col = adjustcolor("red",.3),xpd = NA)
title(main = paste0("Bonneville Dam Detections\n",yr),ylab = "Cumalitive Count")
legend("topright",c("Ten year data","Ten year model average",yr),col = c(adjustcolor(c("grey80","red"),.5),1),lty = c(1,1,2),lwd = c(4,4,3),bty = "n",box.lty = 2)


# Histogram  
  # par(mar = c(7,4,4,2) + .01)
  coltxt <- rep(1,ncol(dta)); coltxt[WS$currWk] <- 2
  x <- barplot(dta,las = 2,beside = FALSE,col = col, xaxt = "n", ylim = c(0,40))
  text(cex=1, x=x-.8, y=-7, colnames(dta), xpd=TRUE, srt=45, col = coltxt)
  legend("topright",sprintf("Brood Year-%s",yr -c(0,1,2,3)),fill = col,bty = "n")
  title(xlab = "Week", line = 4.5); title(ylab = "Count")
  mtext("Week",1,line = 1,outer = T)
})


# Expansion Table
output$T_expansion <- renderTable({
  df_expansion_table <- WS$L_T_expansion[[input$histyear]] 
  # Determine the index of rows that have more than 5 detections
  ci_row_index <- as.numeric(df_expansion_table[[7]]) < 5
  # Determine the index of the last row
  last_row_index <- nrow(df_expansion_table)
  
  #Create formatted table
  df_expansion_table  %>%
    mutate_at(c(2, 4, 5, 7, 8, 9, 10), as.integer) %>%
    mutate(across(c(4, 5, 7:10), ~format(., big.mark = ","))) 
  
  # Create a function to make the last row bold
  df_expansion_table[last_row_index, ] <- lapply(df_expansion_table[last_row_index, ], function(x) {
    x[is.na(x)] <- ""  # Replace NA with empty string
    paste0("<span style='font-weight:bold; color:black;'>", x, "</span>")})
  
  # Create a function to make CI generated form less than 5 unique detections tan
  df_expansion_table[ci_row_index, c(9, 10)] <- lapply(df_expansion_table[ci_row_index, c(9, 10)], function(x) {
    paste0("<span style='color:tan;'>", x, "</span>") })
  
   # Return the processed data frame to be rendered as a table
  df_expansion_table
  
}, sanitize.text.function = function(x) x)

# Weekly Expansion Plot ## 
output$P_expansion <- renderPlot({
  par(mar = c(4,6,3,1) + .01, oma = c(4,2,1,1), mfrow = c(2,1))
  
  # Subset L_P_expansion based on the selected histyear
  df_expansion_plot <- WS$L_P_expansion[[input$histyear]]
  
  # Create the plot
  P_expansion <- ggplot(df_expansion_plot, aes(x = week_start, y = cum_expansion)) +
    geom_point() +
    theme_minimal()
  
  print(P_expansion)
})







}

