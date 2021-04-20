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

art <- read_csv("art_hiv.csv")
art <- rename(df, "Coverage" = 4)

educ <- read_csv("educ_hiv.csv")
educ <- rename(educ, "Knowledge" = 4)

educ <- educ %>% arrange(desc(Knowledge)) %>%
            top_n(10)    
educ$Entity <- factor(educ$Entity, levels = c("Antigua and Barbuda", "Malawi", "Belize", "Peru",
                                              "Lithuania", "Cameroon", "Kenya", "Namibia", "Mauritania", "Eswatini"))

world_map <- map_data("world")
world_map <- fortify(world_map)

ui <- navbarPage("HIV",
                 tabPanel("Antiretroviral Therapy Coverage of Infected Population", 
                          titlePanel("Antiretroviral Therapy Coverage of Infected Population"),
                          
                          sliderInput(inputId = "year_choose",
                                      label = "Choose a year",
                                      value = 2017, min = 2000, max = 2017, width = "500px"),
                          
                          plotlyOutput(outputId = "art_map",
                                       height = "900px",
                                       width = "1200px")
                 ),
                 
                 tabPanel("Education on AIDS prevetion among young people", 
                          titlePanel(" Top 10 Countries of Education on AIDS prevetion among young people in 2016"),
                          
                          plotOutput(outputId = "educ_graph",
                                       height = "900px",
                                       width = "1200px")
                 )
)

server <- function(input, output){
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
