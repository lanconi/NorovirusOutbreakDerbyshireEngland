#
#
#

library(shiny)

# Define UI for application that draws a histogram
shiny::shinyUI(shiny::fluidPage(

    # Application title
    shiny::titlePanel("Norovirus Outbreak in Derbyshire England"),

    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
        position = "left",
        
        shiny::sidebarPanel(
            shiny::h3("Select Rooms"),
            shiny::checkboxInput("room01","Room 01 [children=86]", value=TRUE),
            shiny::checkboxInput("room02","Room 02 [children=36]", value=TRUE),
            shiny::checkboxInput("room03","Room 03 [children=25]", value=TRUE),
            shiny::checkboxInput("room04","Room 04 [children=29]", value=TRUE),
            shiny::checkboxInput("room05","Room 05 [children=32]", value=TRUE),
            shiny::checkboxInput("room06","Room 06 [children=27]", value=TRUE),
            shiny::checkboxInput("room07","Room 07 [children=28]", value=TRUE),
            shiny::checkboxInput("room08","Room 08 [children=33]", value=TRUE),
            shiny::checkboxInput("room09","Room 09 [children=36]", value=TRUE),
            shiny::checkboxInput("room10","Room 10 [children=24]", value=TRUE),
            shiny::checkboxInput("room11","Room 11 [children=28]", value=TRUE),
            shiny::checkboxInput("room12","Room 12 [children=29]", value=TRUE),
            shiny::checkboxInput("room13","Room 13 [children=30]", value=TRUE),
            shiny::checkboxInput("room14","Room 14 [children=29]", value=TRUE),
            shiny::checkboxInput("room15","Room 15 [children=20]", value=TRUE),
            shiny::submitButton("Update Plot"),
            width=3
        ),

        # Show a plot of the generated distribution
        shiny::mainPanel(
            shiny::p(paste0("Background: In 2001, there was a Norovirus outbreak in a primary school in Derbyshire, England.",
                            " There were a total of 492 students in 15 separate classes, or rooms.",
            " Over a 23 day period, the Norovirus spread throughout the school and children reported absent due to symptoms and sickness.",
            "The plot below shows how many total students were absent as the days progressed onwards.")),
            shiny::p(paste0( "As you can see, the plot shows the maximum 186 Absent Children.",
                             " Follow the Direction below to see different results ...")),
            shiny::p(paste0("Directons: Select any or all of the rooms on the left panel, ",
                            " in order to see how many students were accumulatively ",
                            "absent as the days increased in number. ",
                            " Press the 'Update Plot' button to refresh the plot below. ",
                            "Only children from the selected rooms on the left panel will be used in the plot below")),
            shiny::plotOutput("childrenAbsences"),
            shiny::textOutput("roomsSelectedTxt"),
            shiny::p(""),
            shiny::p("For more analysis, please go to following link on Rubs ... https://rpubs.com/lanconi/NorovirusOutbreaksProximityVsCleanliness")
        )
    )
))
