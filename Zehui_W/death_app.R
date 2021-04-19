library(shiny)
library(maps)
library(mapproj)
library(tidyverse)
library(leaflet)
library(dplyr)
library(ggplot2)
library(rsconnect)
library(selectr)
library(reshape2)
library(RColorBrewer)
library(plotly)

# Import data & rename data & clean data
share_death <- read_csv("data/share-deaths-aids.csv")
colnames(share_death)[4] <- "share_death"

death_rates_age <- read_csv("data/hiv-death-rates-by-age.csv")
colnames(death_rates_age)[4] <- "<=5"
colnames(death_rates_age)[5] <- ">=70"
colnames(death_rates_age)[6] <- "5-14"
colnames(death_rates_age)[7] <- "15-49"
colnames(death_rates_age)[8] <- "50-69"
colnames(death_rates_age)[9] <- "all"

share_death <- share_death %>% drop_na(Code)
share_death <- share_death %>% filter(Entity != "World")

death_rates_age <- death_rates_age %>% drop_na(Code)
death_rates_age <- death_rates_age %>% filter(Entity != "World")

# get world map
world_map <- map_data("world")
world_map <- fortify(world_map)

region <- unique(share_death$Entity)

#Shiny UI
ui <- navbarPage("HIV Share of Death",
                 #share_death
                 tabPanel("Share of Population Death because of HIV", 
                          titlePanel("Death Rate of HIV"),
                          
                          sliderInput(inputId = "year",
                                      label = "Choose a year",
                                      value = 2000, 
                                      min = 1990, 
                                      max = 2017),
                          
                          selectInput(inputId = "age_group",
                                      label = "Choose a age group",
                                      choices = c("<=5" = "1990",
                                                  ">=70" = "1995",
                                                  "5-14" = "2000",
                                                  "15-49" = "2005",
                                                  "50-69" = "2010",
                                                  "all" = "2017")),
                          
                          plotlyOutput(outputId = "death_share_map",
                                       height = "800px",
                                       width = "1200px")
                          
                          
                 )
)

server <- function(input, output){
  #output for share map
  output$death_share_map<- renderPlotly({
    sharemap_year <- share_death %>% filter(Year == input$year)
    
    # specify boundary & map options
    l <- list(color = toRGB("grey"), width = 0.3)
    
    g <- list(
      showframe = TRUE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    fig <- plot_geo(sharemap_year)
    fig <- fig %>% add_trace(
      z = ~share_death, color = ~share_death, colors = 'Reds',
      text = ~Entity, locations = ~Code, marker = list(line = l)
    )
    fig <- fig %>% colorbar(title = 'Share of infected population', ticksuffix = '%', thickness = 15)
    fig <- fig %>% layout(
      title = 'Share of infected population HIV',
      geo = g
    )
    fig
  }) 
  
}

shinyApp(ui = ui,server = server)
