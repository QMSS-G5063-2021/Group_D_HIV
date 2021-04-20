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

pop_infection <- read_csv("share-of-population-infected-with-hiv-ihme.csv")
names(pop_infection)[names(pop_infection) == 
                       "Prevalence - HIV/AIDS - Sex: Both - Age: 15-49 years (Percent)"] <- "Prevalence"

new_cases <- read_csv("new-cases-of-hiv-infection.csv")
names(new_cases)[names(new_cases) == 
                       "Incidence - HIV/AIDS - Sex: Both - Age: All Ages (Number)"] <- "Incidence"

new_cases <- new_cases %>% drop_na(Code)
new_cases <- new_cases %>% filter(Entity != "World")
new_cases <- new_cases %>% mutate_at(vars(Incidence), funs(round(., 0)))

pop_infection <- pop_infection %>% drop_na(Code)
pop_infection <- pop_infection %>% filter(Entity != "World")
pop_infection <- pop_infection %>% mutate_at(vars(Prevalence), funs(round(., 5)))


world_map <- map_data("world")
world_map <- fortify(world_map)


region <- unique(new_cases$Entity)



#Create shiny interface/server
ui <- fluidPage(
  #create panel for shares of infected population
  tabPanel("Share of Population Infected with HIV", 
            titlePanel("Share of Population Infected with HIV"),
            
            #create a slider with year in data range (1990-2017)
            sliderInput(inputId = "year1",
                        label = "Choose a year",
                        value = 2017, min = 1990, max = 2017, width = "500px"),
            
            #plot map
            plotOutput(outputId = "share_map",
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
             plotOutput(outputId = "newcase_map",
                        height = "800px",
                        width = "800px")
  )
)

server <- function(input, output){
  #output for share map
  output$share_map<- renderPlot({
    sharemap_year <- pop_infection %>% filter(Year == input$year1)
    sharemap_year$percentage <- cut(sharemap_year$Prevalence,
                          breaks=c(min(sharemap_year$Prevalence, na.rm = T),0.01,0.1,1,10, max(sharemap_year$Prevalence, na.rm = T)), 
                          labels=c("<=0.01%","0.01%-0.1%","0.1%-1%","1%-10%", ">10%"),include.lowest=TRUE,order=TRUE)
    ggplot() +
      geom_map(data = world_map, map = world_map, aes(x = long, y = lat, map_id = region),
               fill = "white", color = "#7f7f7f", size = 0.5) +
      geom_map(data = sharemap_year, map = world_map, aes(fill = percentage, map_id = region)) +
      scale_fill_brewer(palette = "Greys") + 
      theme_minimal() + 
      theme(legend.position = "top") + 
      labs(x = "", y = "", caption = "Source: OurWorldInData")
  }) 
  
  #output for newcase map  
  output$newcase_map<- renderPlot({
    newcasemap_year <- new_cases %>% filter(Year == input$year2)
    newcasemap_year$cases <- cut(newcasemap_year$Incidence,
                                  breaks=c(min(newcasemap_year$Incidence, na.rm = T),100,1000,10000,100000, max(newcasemap_year$Incidence, na.rm = T)), 
                                  labels=c("<=100","100-1000","1000-10000","10000-100000", ">100000"),include.lowest=TRUE,order=TRUE)
    ggplot() +
    geom_map(data = world_map, map = world_map, aes(x = long, y = lat, map_id = region),
                        fill = "white", color = "#7f7f7f", size = 0.5) +
    geom_map(data = newcasemap_year, map = world_map, aes(fill = cases, map_id = region)) +
    scale_fill_brewer(palette = "Oranges") + 
    theme_minimal() + 
    theme(legend.position = "top") + 
    labs(x = "", y = "", caption = "Source: OurWorldInData")
  })
}

shinyApp(ui = ui,server = server)
