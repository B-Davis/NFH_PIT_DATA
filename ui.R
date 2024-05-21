# National Fish Hatchery Tool for Columbia River Gorge complex
library(shinythemes)

fluidPage(theme = shinytheme("cerulean"),
    navbarPage(
        "Title",
        tabPanel("Warm Springs",
            sidebarPanel(
                tags$h3("Some Title Perhaps"),
                radioButtons("histyear",label  = "Year",
                choices = rev(dimnames(A_hist)[[3]]), selected = currYr,
                width = 2
                )
            ),
            mainPanel(plotOutput("histWS"),
                      tableOutput("expansionWS"))
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
