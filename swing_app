# --------------------------------------------------------------------------
# Libraries
# --------------------------------------------------------------------------
library(civis)
library(shiny)
library(plotly)
library(maps)
library(tidyverse)

# --------------------------------------------------------------------------
# Build data
# --------------------------------------------------------------------------
# County-level
df <- str_c("
            select distinct
            state,
            state_full,
            county_name,
            dem_twoway,
            dem_twoway16,
            total_votes,
            turnout_2016,
            projected_support,
            projected_turnout,
            dem_twoway::real - dem_twoway16::real as support_swing,
            total_votes::real - turnout_2016::real as turnout_swing,
            total_votes::real/turnout_2016::real as pct_2016_turnout
            from data.election_data 
            where county_name is not null 
              and state is not null;") %>%
  sql() %>%
  read_civis() 

min(df$dem_twoway, na.rm = T)

# Change to lower case
df$county_name <- tolower(df$county_name)
df$state_full <-  tolower(df$state_full)


# Map data
county_map <- map_data('county')


# Merge base data
county_map_data <- merge(df, county_map, by.x = c("state_full","county_name"), by.y = c("region","subregion"))
county_map_data <- county_map_data[order(county_map_data$order),]


# set up labels
labels <- c(dem_twoway = "Two-Way Biden Vote",
            support_swing = "Support Swing (2016-2020)",
            pct_2016_turnout = "% of 2016 Turnout")

# set up low gradient 
low <- c(dem_twoway = "#FF4137",
         support_swing = "#FF4137",
         pct_2016_turnout = "#A6611A")

# set up high gradient 
high <- c(dem_twoway = "#4C95D7",
          support_swing = "#4C95D7",
          pct_2016_turnout = "#018571")

# set up midpoint
midpoint <- c(dem_twoway = .5,
              support_swing = 0,
              pct_2016_turnout = 1)

# set up break sequence
by <- c(dem_twoway = .1,
        support_swing = .05,
        pct_2016_turnout = .1)
#------------------------------------------------------------------------------
# Build Shiny App
#------------------------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel(title = "Support and Turnout Swing (2016 - 2020) in Swing States"),
  sidebarLayout(
    
    sidebarPanel(selectInput(inputId = "fill_state", label = h3("Select a variable: "),
                                   choices = list("Two-Way Biden Vote" = "dem_twoway",
                                                  "Support Swing (2016-2020)" = "support_swing",
                                                  "% of 2016 Turnout" = "pct_2016_turnout"),
                                   selected = "dem_twoway"),
                       selectInput(inputId = "state", label = h3("Select a state:"), 
                                   choices = list("Arizona" = "AZ",
                                                  "Florida" = "FL",
                                                  "Georgia" = "GA",
                                                  "Michigan" = "MI",
                                                  "Nevada" = "NV",
                                                  "North Carolina" = "NC",
                                                  "Pennsylvania" = "PA",
                                                  "Wisconsin" = "WI"), 
                                   selected = "AZ"),
      
      
      helpText("Hover over plot for more information."),
      

    ),
    
    mainPanel(plotlyOutput("state_map"))
    )
    
  )


server <- function(input, output) {
  
  # Plot state map
  output$state_map <- renderPlotly({
    
    county_plot_data <- county_map_data %>%
      filter(state == input$state)
    
    # set up min limits
    min <- c(dem_twoway = min(county_plot_data$dem_twoway, na.rm = T),
             support_swing = min(county_plot_data$support_swing, na.rm = T),
             pct_2016_turnout = min(county_plot_data$pct_2016_turnout, na.rm = T))
    
     # set up max limits
    max <- c(dem_twoway = max(county_plot_data$dem_twoway, na.rm = T),
             support_swing = max(county_plot_data$support_swing, na.rm = T),
             pct_2016_turnout = max(county_plot_data$pct_2016_turnout, na.rm = T))
    
    support_swing_map <- 
      ggplot(data = county_plot_data, aes_string(fill = input$fill_state)) + 
      coord_fixed(1.3) + 
      geom_polygon(data = county_plot_data, aes(x = long, y = lat, group = group,
                       text=paste0(stringr::str_to_title(county_name), ", ", state, 
                                   "<br>Two-Way Biden Vote: ", paste0(round(dem_twoway,2)*100,"%"), 
                                   "<br>Two-Way Clinton Vote: ", paste0(round(dem_twoway16,2)*100,"%"),                                  
                                   "<br>Support Swing (2016-2020): ", ifelse(support_swing > 0, 
                                                                            paste0("+",round(support_swing,2)*100,"%"), 
                                                                            paste0(round(support_swing,2)*100,"%")),
                                   "<br>% of 2016 Turnout: ", paste0(round(pct_2016_turnout,2)*100,"%"))), 
                   color = "black") +
      scale_fill_gradient2(	low = low[input$fill_state], mid = "white", high = high[input$fill_state],
                            midpoint = midpoint[input$fill_state],
                            guide = guide_colorbar(	barwidth = 12, barheight = 0.5, nbin = 10, 
                                                    title = labels[input$fill_state], title.position = "top"),
                            labels = scales::percent_format(accuracy = 1),
                            limits = c(min[input$fill_state], max[input$fill_state]),
                            breaks = seq(min[input$fill_state], max[input$fill_state], by = by[input$fill_state])) +
      labs(title = paste0(state.name[which(state.abb == input$state)], " - ", labels[input$fill_state])) +
      theme_minimal() +
      theme( plot.title = element_text(hjust = .5),
             axis.line = element_blank(),
             axis.title = element_blank(),
             axis.text = element_blank(),
             legend.position = "bottom",
             panel.background = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             text = element_text(family = "Roboto"))
    
    ggplotly(support_swing_map, tooltip = "text")
    
  })
  
}

shinyApp(ui=ui, server=server)
