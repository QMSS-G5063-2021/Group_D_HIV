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

pop_infection <- read_csv("share-of-population-infected-with-hiv-ihme.csv")
colnames(pop_infection)[4] <- "Prevalence"

new_cases <- read_csv("new-cases-of-hiv-infection.csv")
colnames(new_cases)[4] <- "Incidence"

new_cases <- new_cases %>% drop_na(Code)
new_cases <- new_cases %>% filter(Entity != "World")


pop_infection <- pop_infection %>% drop_na(Code)
pop_infection <- pop_infection %>% filter(Entity != "World")



world_map <- map_data("world")
world_map <- fortify(world_map)


region <- unique(new_cases$Entity)



#Create shiny interface/server
ui <- navbarPage("HIV",
  #create panel for shares of infected population
  tabPanel("Share of Population Infected with HIV", 
            titlePanel("Share of Population Infected with HIV"),
            
            #create a slider with year in data range (1990-2017)
            sliderInput(inputId = "year1",
                        label = "Choose a year",
                        value = 2017, min = 1990, max = 2017, width = "500px"),
            
            #plot map
            plotlyOutput(outputId = "share_map",
                       height = "800px",
                       width = "800px")
  ),
  
  #create panel for Number of New Infections Each Year
  tabPanel("Number of New Infections Each Year",
             titlePanel("Number of New Infections Each Year"),
             
             #create a slider with year in data range (1990-2017)
             sliderInput(inputId = "year2",
                         label = "Choose a year",
                         value = 2017, min = 1990, max = 2017, width = "500px"),
             
             #plot map
             plotlyOutput(outputId = "newcase_map",
                        height = "800px",
                        width = "800px")
  )
)

server <- function(input, output){
  #output for share map
  output$share_map<- renderPlotly({
    sharemap_year <- pop_infection %>% filter(Year == input$year1)
    
    # specify boundary & map options
    l <- list(color = toRGB("grey"), width = 0.3)
    
    g <- list(
      showframe = TRUE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    fig <- plot_geo(sharemap_year)
    fig <- fig %>% add_trace(
      z = ~Prevalence, color = ~Prevalence, colors = 'Reds',
      text = ~Entity, locations = ~Code, marker = list(line = l)
    )
    fig <- fig %>% colorbar(title = 'Share of infected population', ticksuffix = '%', thickness = 15)
    fig <- fig %>% layout(
      title = 'Share of infected population HIV',
      geo = g
    )
    fig
  }) 
  
  #output for newcase map  
  output$newcase_map<- renderPlotly({
    newcasemap_year <- new_cases %>% filter(Year == input$year2)

    # specify boundary & map options
    l <- list(color = toRGB("grey"), width = 0.3)
    
    g <- list(
      showframe = TRUE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    fig <- plot_geo(newcasemap_year)
    fig <- fig %>% add_trace(
      z = ~Incidence, color = ~Incidence, colors = 'Greens',
      text = ~Entity, locations = ~Code, marker = list(line = l)
    )
    fig <- fig %>% colorbar(title = 'Number of New Infections Each Year', thickness = 15)
    fig <- fig %>% layout(
      title = 'Number of New Infections Each Year',
      geo = g
    )
    fig
  })
}

shinyApp(ui = ui,server = server)
