#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(mgcv)
library(outbreaks)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # default setting for rooms_selected to be used when the 
    # shiny app loads and we have not selected the Submit button yet
    # rooms_selected <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
    
    # create a vector of integers, respresenting the
    # rooms selected from the left panel in the ui.
    # rooms_selected <- shiny::reactive( {
    #     temp <- c()
    #     if( input$room01 ) {  temp <- append(temp, 1) }
    #     if( input$room02 ) {  temp <- append(temp, 2) }
    #     if( input$room03 ) {  temp <- append(temp, 3) }
    #     if( input$room04 ) {  temp <- append(temp, 4) }
    #     if( input$room05 ) {  temp <- append(temp, 5) }
    #     if( input$room06 ) {  temp <- append(temp, 6) }
    #     if( input$room07 ) {  temp <- append(temp, 7) }
    #     if( input$room08 ) {  temp <- append(temp, 8) }
    #     if( input$room09 ) {  temp <- append(temp, 9) }
    #     if( input$room10 ) {  temp <- append(temp, 10) }
    #     if( input$room11 ) {  temp <- append(temp, 11) }
    #     if( input$room12 ) {  temp <- append(temp, 12) }
    #     if( input$room13 ) {  temp <- append(temp, 13) }
    #     if( input$room14 ) {  temp <- append(temp, 14) }
    #     if( input$room15 ) {  temp <- append(temp, 15) }
    #     
    #     # if no checkboxes were selected, then we use all rooms
    #     if( length(temp) == 0 ) {
    #         temp = c(1, 2,3, 4,5,6,7,8,9,10,11, 12,13,14,15)
    #     }
    #     
    #     # print for debugging ..
    #     cat(temp)
    #     
    #     # return the vector of selected rooms
    #     temp
    # })

    output$childrenAbsences <- renderPlot({
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # 
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        # Norovirus in a primary school in Derbyshire, England, 2001
        # These data describe an outbreak of norovirus in the summer of 2001 in a 
        # primary school and nursery in Derbyshire, England.
        # A data frame with 492 rows and 5 columns
        # 
        # The data on norovirus cases were analysed by O'Neill and Marks (2005). 
        # As described in the paper, out of a total of 492 children in the school, 
        # 186 were absent from school with gastrointestinal symptoms. 
        # The school was cleaned on days 13 and 14, and on days 20 and 21, 
        # both of which were weekends, and the school was shut on days 18 and 19. 
        # Following the second cleaning, there were no further absences, 
        # although three children reported symptoms on day 22, the last day of the outbreak.
        data(package="outbreaks", norovirus_derbyshire_2001_school )
        # ?outbreaks::norovirus_derbyshire_2001_school 
        
        # make a copy
        nv <- norovirus_derbyshire_2001_school
        
        # set the names
        names(nv) <- c("room", "day_absent", "start_illness", "end_illness", "day_vomiting")
        
        # set room as integer
        nv$room <- as.integer(as.character(nv$room)) 
        
        # filter the data by only allowing the chosen rooms, this will subset the data ...
        # rooms <- c(1,
        #            2,3,
        #            4,5,6,7,8,
        #            9,10,11,
        #            12,
        #            13,
        #            14,15)
        # rooms_ch <- paste(rooms,collapse = ", ")
        # rooms_selected <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
        
        rooms_selected <- c()
        if( input$room01 ) {  rooms_selected <- append(rooms_selected, 1) }
        if( input$room02 ) {  rooms_selected <- append(rooms_selected, 2) }
        if( input$room03 ) {  rooms_selected <- append(rooms_selected, 3) }
        if( input$room04 ) {  rooms_selected <- append(rooms_selected, 4) }
        if( input$room05 ) {  rooms_selected <- append(rooms_selected, 5) }
        if( input$room06 ) {  rooms_selected <- append(rooms_selected, 6) }
        if( input$room07 ) {  rooms_selected <- append(rooms_selected, 7) }
        if( input$room08 ) {  rooms_selected <- append(rooms_selected, 8) }
        if( input$room09 ) {  rooms_selected <- append(rooms_selected, 9) }
        if( input$room10 ) {  rooms_selected <- append(rooms_selected, 10) }
        if( input$room11 ) {  rooms_selected <- append(rooms_selected, 11) }
        if( input$room12 ) {  rooms_selected <- append(rooms_selected, 12) }
        if( input$room13 ) {  rooms_selected <- append(rooms_selected, 13) }
        if( input$room14 ) {  rooms_selected <- append(rooms_selected, 14) }
        if( input$room15 ) {  rooms_selected <- append(rooms_selected, 15) }
        
        # if no checkboxes were selected, then we use all rooms
        if( length(rooms_selected) == 0 ) {
            rooms_selected = c(1, 2,3, 4,5,6,7,8,9,10,11, 12,13,14,15)
        }
        
        rooms_selected_ch <- paste(rooms_selected, collapse = ", ")
        # nv <- nv %>%  dplyr::filter(room %in% rooms )
        nv <- nv %>%  dplyr::filter(room %in% rooms_selected )
        
        # create a table showing how many students become absent on a given day.
        day_absent_table <- table(nv$day_absent)
        
        day_absent_df <- as.data.frame(day_absent_table)
        names(day_absent_df) <- c("Day","Infected")
        day_absent_df$Day    <- as.integer(as.character(day_absent_df$Day))
        
        # complete the sequence of days so it goes from 0 to 23
        day_absent_df <- day_absent_df %>% tidyr::complete(Day = seq(0, 23, by=1) )
        
        # if any NA, then fill in with 0
        day_absent_df <- day_absent_df %>%
            dplyr::mutate(Infected = ifelse(is.na(Infected), 0, Infected))
        
        # remove the first row because there is no Day 0
        day_absent_df <- day_absent_df[-1,]
        
        # create a InfectedAccum column
        day_absent_df <- day_absent_df %>%
            dplyr::mutate(InfectedAccum  = Infected)
        
        # now add each value in Infected column to the one after it, to
        # accumulate the values
        for( i in 2:nrow(day_absent_df)) {
            day_absent_df$InfectedAccum[i] =  day_absent_df$InfectedAccum[i] +
                day_absent_df$InfectedAccum[i-1]
        }
        
        # force data types to be of the specified class.
        day_absent_df <- day_absent_df %>%
            dplyr::mutate(Day           = as.integer(Day),
                          Infected      = as.integer(Infected),
                          InfectedAccum = as.integer(InfectedAccum))
        
        # ==========================================================
        # Fit a Generalized Additive Model for the data
        # ?mgcv::gam
        # Generalized additive models with integrated smoothness estimation
        # 
        # ?mgcv::s
        # Defining smooths in GAM formulae
        # ==========================================================
        gmodel <- mgcv::gam(InfectedAccum ~ s(Day), data = day_absent_df)
        
        # ==========================================================
        # Usge ggplot2 to visualize the data and the model
        # ==========================================================
        ggplot2::ggplot(data = day_absent_df,
                        aes(x=Day, y=InfectedAccum)) +
            ggplot2::geom_bar(stat = "identity", fill="red", color="#993333", alpha=0.2) +
            ggplot2::stat_smooth(method=gam, formula = y ~ s(x)) +
            ggplot2::labs(title = "Norovirus Outbreak in a primary school - Derbyshire, England, 2001",
                          subtitle = paste0("492 children in 15 rooms; data below shows cumulative children absences for rooms: ", 
                                            rooms_selected_ch ),
                          x = "Days", y = "Total Children Sick & Absent") +
            geom_text(aes(label=format(InfectedAccum)),  vjust=-.5) + #  numbers above each bar
            scale_x_continuous(breaks = seq(0, 23, by= 1)) +
            ggplot2::theme(
                plot.title = element_text(color="black", size=14, face="bold.italic"),
                axis.title.x = element_text(color="black", size=14, face="bold"),
                axis.title.y = element_text(color="black", size=14, face="bold"),
                panel.background = element_blank(),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_line( size=.5, color="grey" ),
                panel.border = element_rect(colour = "black", fill=NA, size=1))

    })
    
    output$roomsSelectedTxt <- shiny::renderText({
        temp <- c()
        if( input$room01 ) {  temp <- append(temp, 1) }
        if( input$room02 ) {  temp <- append(temp, 2) }
        if( input$room03 ) {  temp <- append(temp, 3) }
        if( input$room04 ) {  temp <- append(temp, 4) }
        if( input$room05 ) {  temp <- append(temp, 5) }
        if( input$room06 ) {  temp <- append(temp, 6) }
        if( input$room07 ) {  temp <- append(temp, 7) }
        if( input$room08 ) {  temp <- append(temp, 8) }
        if( input$room09 ) {  temp <- append(temp, 9) }
        if( input$room10 ) {  temp <- append(temp, 10) }
        if( input$room11 ) {  temp <- append(temp, 11) }
        if( input$room12 ) {  temp <- append(temp, 12) }
        if( input$room13 ) {  temp <- append(temp, 13) }
        if( input$room14 ) {  temp <- append(temp, 14) }
        if( input$room15 ) {  temp <- append(temp, 15) }
        
        # if no checkboxes were selected, then we use all rooms
        if( length(temp) == 0 ) {
            "No Rooms were selected, therefore defaulting to ALL Rooms selected in plot above."
        } else {
            temp_ch <- paste(temp, collapse = ", ")
            paste0("The following rooms were selected: ", temp_ch)
            # "testing works"
        }
    })

})
