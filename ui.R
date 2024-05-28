# National Fish Hatchery Tool for Columbia River Gorge complex
library(shinythemes)

fluidPage(theme = shinytheme("cerulean"),
    navbarPage(
        "Title",
        tabPanel("Warm Springs",
            fluidRow(
                column(2,
                    wellPanel(h3(""),
                        radioButtons("histyear",label  = "Year",
                            choices = rev(dimnames(A_hist)[[3]]),
                            selected = currYr
                        )
                    )
                ),
                column(10,
                    tabsetPanel(
                        tabPanel(title = "Detection Data",
                                mainPanel(plotOutput("histWS",height = "600px",width = "800px"))
                        ),
                        tabPanel(title = "Expansion Data",
                                tableOutput("expansionWS")
                        )
                    )
                )    
            )
        ),
        tabPanel("Next Hatchery"),
        tabPanel("Next Hatchery"),
        tabPanel("Next Hatchery"),
        tabPanel("Next Hatchery")
    )
)

# ui <- page_sidebar(
#   # App title ----
#   title = "Hello Shiny!",
#   # Sidebar panel for inputs ----
#   sidebar = sidebar(
#     # Input: Slider for the number of bins ----
#     sliderInput(
#       inputId = "bins",
#       label = "Number of bins:",
#       min = 1,
#       max = 50,
#       value = 30
#     )
#   ),
#   # Output: Histogram ----
#   plotOutput(outputId = "distPlot")
# )

# library(shiny)

# # Define UI for application that draws a histogram
# fluidPage(

#     # Application title
#     titlePanel("2024 Expanded Adult Returns to Bonneville"),

#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),

#         # Show a plot of the generated distribution
#         mainPanel(
#             tableOutput("bonnTable") ,
#             plotOutput("cummulativePlot") 
#         )
#     )
# )
