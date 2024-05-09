# National Fish Hatchery Tool for Columbia River Gorge complex
library(shinythemes)

# fluidPage(theme = shinytheme("cerulean"),
#      sidebarPanel(
#       textInput("txt", "Text input:", "text here"),
#       sliderInput("slider", "Slider input:", 1, 100, 30),
#       actionButton("action", "Button"),
#       actionButton("action2", "Button2", class = "btn-primary")
#     ),
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Tab 1"),
#         tabPanel("Tab 2")
#       )
#     )
# )



library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)
