library(shiny)

shinyUI(fluidPage(

    titlePanel("Next word prediction"),

    sidebarLayout(
        sidebarPanel(
            textInput("userPhrase", "Enter any phrase"),
            h5("This page makes prediction of the next word based on entered words.
               The result is word alternatives and their probabilities.")
        ),

        mainPanel(
            dataTableOutput("result")
        )
    )
))
