# import all the libraries
library(bslib)
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
library(shinythemes)

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
educ <- read_csv("data/educ_hiv.csv")

colnames(art)[4] <- "Coverage"
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

death_rates_age_world <- read_csv("data/hiv-death-rates-by-age.csv")
colnames(death_rates_age_world)[4] <- "<=5"
colnames(death_rates_age_world)[5] <- ">=70"
colnames(death_rates_age_world)[6] <- "5-14"
colnames(death_rates_age_world)[7] <- "15-49"
colnames(death_rates_age_world)[8] <- "50-69"
colnames(death_rates_age_world)[9] <- "all"
death_rates_age_world <- death_rates_age_world %>% drop_na(Code)
death_rates_age_world <- death_rates_age_world %>% filter(Entity == "World")

# Get World Map

world_map <- map_data("world")
world_map <- fortify(world_map)

region <- unique(new_cases$Entity)

#Create shiny interface/server

ui <- fluidPage(
  theme = bs_theme(version = 4,bootswatch = "minty"),   
  h1("Global Distribution of HIV & Severity & Prevention"),
  
#-------------------------------------------------------------------------  
  h2("Introduction"), 
    p(a(href = "https://ourworldindata.org/hiv-aids#the-global-distribution-of-deaths-from-hiv-aids", 
        "Data Source (ourworldindata.org)")),
    p("An infection with HIV (human immunodeficiency virus) can lead to AIDS (acquired immunodeficiency syndrome). AIDS results in a gradual and persistent decline and failure of the immune system, resulting in heightened risk of life-threatening infection and cancers. In the majority of cases, HIV is a sexually-transmitted infection. However, HIV can also be transmitted from a mother to her child, during pregnancy or childbirth, or through breastfeeding. Non-sexual transmission can also occur through the sharing of injection equipment such as needles."),
  
    img(src = "WHO-HIV-Data.jpg", height = 450, width = 900),
    br(), br(),

  h3("This is an R Shiny interface to provide worldwide HIV related insights"),
    p("HIV/AIDS is one of the world’s most fatal infectious diseases – particularly across Sub-Saharan Africa, where the disease has had a massive impact on health outcomes and life expectancy in recent decades.
    
    The Global Burden of Disease is a major global study on the causes of death and disease published in the medical journal The Lancet.1 These estimates of the annual number of deaths by cause are shown here. This chart is shown for the global total, but can be explored for any country or region using the “change country” toggle.
    
    In the chart we see that, globally, it is the second most fatal infectious disease.
    
    According to the Global Burden of Disease study, almost one million (954,000) people died from HIV/AIDS in 2017. To put this into context: this was just over 50% higher than the number of deaths from malaria in 2017.
    
    It’s one of the largest killers globally; but for some countries – particularly across Sub-Saharan Africa, it’s the leading cause of death. If we look at the breakdown for South Africa, Botswana or Mozambique – which you can do on the interactive chart – we see that HIV/AIDS tops the list. For countries in Southern Sub-Saharan Africa, deaths from HIV/AIDS are more than 50% higher than deaths from heart disease, and more than twice that of cancer deaths."),

#------------------------------------------------------------------------- 
  h2("HIV Worldwide Distribution"),
  h4("Share of Population Infected with HIV"),    
    p("Between 1996 and 2001 more than 3 million people were infected with HIV ever year. Since then the number of new infections began to decline and in 2017 it was reduced to below 2 million. The lowest number of new infections since 1990."),
    #create a slider with year in data range (1990-2017)
    sliderInput(inputId = "year1",
                label = "Choose a year",
                value = 2017, min = 1990, max = 2017, width = "500px"),
    
    #plot map
    plotlyOutput(outputId = "share_map",
                 height = "500px",
                 width = "900px"),   

#-------------------------------------------------------------------------   
  h4("Number of New Infections Each Year"),
    #create a slider with year in data range (1990-2017)
    sliderInput(inputId = "year2",
                label = "Choose a year",
                value = 2017, min = 1990, max = 2017, width = "500px"),
    
    #plot map
    plotlyOutput(outputId = "newcase_map",
                 height = "500px",
                 width = "900px"), 

#-------------------------------------------------------------------------    
  h2("HIV is fatal & Serious"), 
  h4("Share of Population Death because of HIV"),
    p("Globally, 1.7% of deaths were caused by HIV/AIDS in 2017.This share is high, but masks the wide variations in the toll of HIV/AIDS across the world. In some countries, this share was much higher."),
    p("In the interactive map we see the share of deaths which resulted from HIV/AIDS across the world. 
        Across most regions the share was low: across Europe, for example, it accounted for less than 0.1% of deaths.
        But across some countries – focused primarily in Southern Sub-Saharan Africa – the share is very high. 
        More than 1-in-4 of deaths (28%) in South Africa and Botswana were caused by HIV/AIDS in 2017. 
        The share was also very high across Mozambique (24%); Namibia (23%); Zambia (18%); Kenya (17%); and Congo (15%)."),
    
    sliderInput(inputId = "year",
                label = "Choose a year",
                value = 2000, 
                min = 1990, 
                max = 2017),
    
    plotlyOutput(outputId = "death_share_map",
                 height = "500px",
                 width = "900px"),

#-------------------------------------------------------------------------
  h4("Death Rate among the Top 8 HIV severe Countries"),
    p("If we take a furthur look at the Death Rate of HIV, we could sadly see that for the Top 8 countries, 
      with highest HIV death rate, the death rate in these countries is increasing gradually.
      The large health burden of HIV/AIDS across Sub-Saharan Africa is also reflected in death rates. 
      Death rates measure the number of deaths from HIV/AIDS per 100,000 individuals in a country or region."),
      plotlyOutput(outputId = "death_rate_line",
                   height = "500px",
                   width = "900px"),

  h4("HIV Death Rate Worldwide for Different Age Groups"),
    p("In the chart we show death rates by age group. 
      We can see that baby <5 years old and yound adults 15-49 are most risky groups.
      Since HIV is primarily a sexually-transmitted infection, where unsafe sex is a primary risk factor, this is what we would expect for age 15-49 group.
      But we also see that death rates are higher for young children <5 years old. 
      This is because HIV can be transmitted from mother-to-child if the mother is infected."),
      plotlyOutput(outputId = "death_rate_age",
                   height = "500px",
                   width = "900px"),
#------------------------------------------------------------------------- 
  h2("HIV Can Be Prevented "), 
  h4("Antiretroviral Therapy Coverage of Infected Population"), 
    p("A couple of decades ago, the chances of surviving more than ten years with HIV were slim. Today, thanks to antiretroviral therapy (ART), people with HIV/AIDS can expect to live long lives. 
      ART is a mixture of antiviral drugs that are used to treat people infected with human immunodeficiency virus (HIV). ART is an essential player in making progress against HIV/AIDS because it saves lives, allows people with HIV to live longer, and prevents new HIV infections. "),
    sliderInput(inputId = "year_choose",
                label = "Choose a year",
                value = 2017, min = 2000, max = 2017, width = "500px"),
    
    plotlyOutput(outputId = "art_map",
                 height = "500px",
                 width = "900px"),

#-------------------------------------------------------------------------  
  h4("Education on AIDS prevetion among young people"),
    p("The number of people who receive ART has increased significantly in recent years, especially in African countries where the prevalence of HIV/AIDS is the highest. You can see this in the map. In 2005 only 2 million people received ART; by 2018 this figure has increased more than ten-fold to 23 million.11"),
    plotOutput(outputId = "educ_graph",
               height = "400px",
               width = "700px")
)

server <- function(input, output){
  #output for share map
  output$share_map<- renderPlotly({
    align = c
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
  
  
  top_8_death <- death_rates_age %>%
    filter(Entity == c("Albania","Bosnia and Herzegovina","Slovakia",
                       "North Macedonia", "Syria",
                       "Comoros","Turkey","Maldives"))
  
  
  output$death_rate_line <- renderPlotly({
    ggplot(data = top_8_death, 
           aes(x = Year, y = all, colour = Entity)) + 
      geom_line() +
      geom_point() +
      labs(title="Top 8 Death Rate Countries",
           x="Year",
           y="Death Rate of the country",
           color = "Country",
           caption = "Source: https://ourworldindata.org/hiv-aids#the-global-distribution-of-deaths-from-hiv-aids") +
      theme(legend.position = 'top', 
            plot.title = element_text(colour = alpha('black')))
  })
  
  death_rates_age_world_long <- death_rates_age_world %>%
    gather(c("<=5",">=70","5-14","15-49","50-69","all"),key="age_group",value="death_rate")

  output$death_rate_age <- renderPlotly({
    ggplot(data = death_rates_age_world_long, 
           aes(x = Year, y = death_rate, colour = age_group)) + 
      geom_line() +
      geom_point() +
      labs(title="HIV Death Rate Worldwide Among Different Age Groups",
           x="Year",
           y="HIV Death Rate Worldwide",
           color = "Age Group",
           caption = "Source: https://ourworldindata.org/hiv-aids#the-global-distribution-of-deaths-from-hiv-aids") +
      theme(legend.position = 'top', 
            plot.title = element_text(colour = alpha('black')))
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
