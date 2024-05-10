#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

  output$bonnTable <- renderTable({
    expanded_adult_return %>% 
      mutate_at(vars(c(1, 5, 8, 9, 10)), as.integer)%>% 
      mutate(Est.Over.Bonneville = as.integer(round(Est.Over.Bonneville, 0))) %>% 
      filter(Last.Year == format(Sys.Date(), "%Y")) %>% # Current year only
      select(Mark.Site.Name, 
             Run.Name, 
             Brood.Year, 
             Age, 
             PIT.Tagged, 
             Released, 
             Ratio, 
             Stock, 
             Unique.Tags.Detected, 
             Est.Over.Bonneville, 
             LowerCI, 
             UpperCI) 
          })
  
  output$cummulativePlot <- renderPlot({
  ggplot(adult_returns2, aes(x=Interval, y=Average_Perc_Cum_Ret, color = Hatchery, group = Hatchery)) +
                geom_point() +
                geom_line()+
                #geom_errorbar(aes(ymin=Average_Perc_Cum_Ret-se, ymax=Average_Perc_Cum_Ret+se))+
                ggtitle("Spring Chinook - Average Cumulative Detections at Bonneville ") +
                xlab("") + ylab("Average Cumulative Return (%)")+
                scale_x_discrete(labels=BONN_Returns2$`End Date`)+
                theme_bw()+
                theme(legend.position = 'bottom', legend.direction = "vertical")+
                theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), text=element_text(face="bold", size=8))
  })
  
  
}

#runApp()
