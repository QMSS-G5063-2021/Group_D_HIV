# import all the libraries

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

# Import all the data & Rename data 

pop_infection <- read_csv("data/share-of-population-infected-with-hiv-ihme.csv")
colnames(pop_infection)[4] <- "Prevalence"

new_cases <- read_csv("data/new-cases-of-hiv-infection.csv")
colnames(new_cases)[4] <- "Incidence"

share_death <- read_csv("data/share-deaths-aids.csv")
colnames(share_death)[4] <- "share_death"

death_rates_age <- read_csv("data/hiv-death-rates-by-age.csv")
colnames(death_rates_age)[4] <- "<=5"
colnames(death_rates_age)[5] <- ">=70"
colnames(death_rates_age)[6] <- "5-14"
colnames(death_rates_age)[7] <- "15-49"
colnames(death_rates_age)[8] <- "50-69"
colnames(death_rates_age)[9] <- "all"

art <- read_csv("data/art_hiv.csv")
colnames(art)[4] <- "Coverage"

educ <- read_csv("data/educ_hiv.csv")
colnames(educ)[4] <- "Knowledge"

# Clean Data

new_cases <- new_cases %>% drop_na(Code)
new_cases <- new_cases %>% filter(Entity != "World")

pop_infection <- pop_infection %>% drop_na(Code)
pop_infection <- pop_infection %>% filter(Entity != "World")

share_death <- share_death %>% drop_na(Code)
share_death <- share_death %>% filter(Entity != "World")

death_rates_age <- death_rates_age %>% drop_na(Code)
death_rates_age <- death_rates_age %>% filter(Entity != "World")

educ <- educ %>% arrange(desc(Knowledge)) %>%
  top_n(10)    
educ$Entity <- factor(educ$Entity, levels = c("Antigua and Barbuda", "Malawi", "Belize", "Peru",
                                              "Lithuania", "Cameroon", "Kenya", "Namibia", "Mauritania", "Eswatini"))


# Get World Map

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
  ),
  
  # Share of Death
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
  ),
  
  # ART
  tabPanel("Antiretroviral Therapy Coverage of Infected Population", 
           titlePanel("Antiretroviral Therapy Coverage of Infected Population"),
           
           sliderInput(inputId = "year_choose",
                       label = "Choose a year",
                       value = 2017, min = 2000, max = 2017, width = "500px"),
           
           plotlyOutput(outputId = "art_map",
                        height = "900px",
                        width = "1200px")
  ),
  
  # Education
  tabPanel("Education on AIDS prevetion among young people", 
           titlePanel(" Top 10 Countries of Education on AIDS prevetion among young people in 2016"),
           
           plotOutput(outputId = "educ_graph",
                      height = "900px",
                      width = "1200px")
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
    fig <- fig %>% colorbar(title = 'Share of Death population', ticksuffix = '%', thickness = 15)
    fig <- fig %>% layout(
      title = 'Share of Death population - HIV',
      geo = g
    )
    fig
  }) 
  
  
  output$art_map<- renderPlotly({
    artmap_year <- art %>% filter(Year == input$year_choose)
    
    line <- list(color = toRGB("grey"), width = 0.3)
    
    geo <- list(
      showframe = TRUE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercators')
    )
    
    p <- plot_geo(artmap_year)
    p <- p %>% add_trace(
      z = ~Coverage, color = ~Coverage, colors = 'Greens',
      text = ~Entity, locations = ~Code, marker = list(line = line)
    )
    p <- p %>% colorbar(title = 'ART Coverage', ticksuffix = '%', thickness = 50)
    p <- p %>% layout(
      title = 'Antiretroviral Therapy Coverage of Infected Population',
      geo = geo
    )
    p
  }) 
  
  
  #output for educ graph  
  output$educ_graph<- renderPlot({
    
    fig <- ggplot(educ, aes(Entity, Knowledge)) + 
      geom_col(aes(fill=Knowledge))
    fig
    
  })
  
}

shinyApp(ui = ui,server = server)
