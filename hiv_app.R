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
library(tm)
library(wordcloud)

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

death_rate_all <- select(death_rates_age,Entity,Code,Year,all)

art <- read_csv("data/art_hiv.csv")
art2 <- read_csv("data/art_hiv.csv")
colnames(art2)[4] <- "ART"
art2 <- art2 %>%
  mutate(ART = ART/100)

educ <- read_csv("data/educ_hiv.csv")


colnames(art)[4] <- "Coverage"
colnames(educ)[4] <- "HIV Prevention Education Coverage"

death_rate_art <- right_join(x=death_rate_all,
                             y=art2,
                             by = c("Entity" = "Entity",
                                    "Code" = "Code", 
                                    "Year" = "Year"))


# Clean Data

new_cases <- new_cases %>% drop_na(Code)
new_cases <- new_cases %>% filter(Entity != "World")

pop_infection <- pop_infection %>% drop_na(Code)
pop_infection <- pop_infection %>% filter(Entity != "World")

share_death <- share_death %>% drop_na(Code)
share_death <- share_death %>% filter(Entity != "World")

death_rates_age <- death_rates_age %>% drop_na(Code)
death_rates_age <- death_rates_age %>% filter(Entity != "World")

educ <- educ %>% arrange(desc(`HIV Prevention Education Coverage`)) %>%
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

hiv_tweet <- read_csv("data/hiv_tweet.csv")

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
    p("An infection with HIV (human immunodeficiency virus) can lead to AIDS (acquired immunodeficiency syndrome). 
      AIDS results in a gradual and persistent decline and failure of the immune system, resulting in heightened risk of life-threatening infection and cancers. 
      In the majority of cases, HIV is a sexually-transmitted infection. However, HIV can also be transmitted from a mother to her child, during pregnancy or childbirth, or through breastfeeding. 
      Non-sexual transmission can also occur through the sharing of injection equipment such as needles."),
      
    p("HIV/AIDS is one of the world’s most fatal infectious diseases – 
      particularly across Sub-Saharan Africa, where the disease has had a massive impact on health outcomes and life expectancy in recent decades."),

    p("A couple of decades ago, there is no chance of surviving more than ten years with HIV. Today, thanks to antiretroviral therapy (ART), people with HIV/AIDS can expect to live long lives. 
      ART is a mixture of antiviral drugs that are used to treat people infected with human immunodeficiency virus (HIV). 
      ART is an essential player in making progress against HIV/AIDS because it saves lives, allows people with HIV to live longer, and prevents new HIV infections."),

    p("Below is the snapshot of the HIV/AIDS Epidemic in 2019, we can see that HIV/ADIS is still a serious virus/disease in our society. 
       We hope this project would introduce a general history of HIV/AIDS, and how people are fighting against it."),

    img(src = "WHO-HIV-Data.jpg", height = 450, width = 900),
    br(), br(),

  h3("This is an R Shiny interface to provide worldwide HIV related insights, and there are four sections:"),
  h5("1) worldwide distribution of HIV,") ,
  h5("2) worldwide death caused by HIV, ") ,
  h5("3) prevention of HIV and ") ,
  h5("4) HIV related tweet text analysis."),
     

#------------------------------------------------------------------------- 
  h2("HIV Worldwide Distribution"),
  h4("Share of Population Infected with HIV"),    
    p("From the interactive plot of Share of population infected with HIV, we can clearly identify the trends of spreading of HIV. 
      In the 90s, the share of the infected population was comparably lower, with only 2 countries exceeding 10% (Uganda 14.21% and Zimbabwe 14.12%) The distribution of high density of HIV is generally located in the middle-south part of Africa."),
    p("Around 2000, the southern part suffered most from HIV population proportion: multiple countries exceeded 10% share of the infected population with the highest being Botswana, 26.67% The HIV share in Africa reached its peak in 2004. Then HIV began to spread to other continents at a quicker pace. For example, the share of population in Russia was 0.2% in 2004, and went all the way up to 1.36% in 2017. 
      Many other regions such as South-eastern Asia and South America also appeared to have an increased share of infected population, proving that the spread of HIV is speeding up."),
    #create a slider with year in data range (1990-2017)
    sliderInput(inputId = "year1",
                label = "Choose a year",
                value = 2017, 
                min = 1990, 
                max = 2017, 
                width = "700px",
                sep=""),
    
    #plot map
    plotlyOutput(outputId = "share_map",
                 height = "500px",
                 width = "900px"),   

#-------------------------------------------------------------------------   
  h4("Number of New Infections Each Year"),
    p("Combined with previous graphs, it is not hard for us to infer that the newly diagnosed HIV cases have been increasing, too. 
      However, since this plot focuses on the magnitude of new infections, it shows some interesting patterns not obvious in other graphs: For example, in terms of the absolute number of infection cases, countries with a higher population such as United States, China, India and Brazil played a significant role on the world stage. "),
       p("Also, through the choropleth map we can see that the new infection cases have not always been increasing. One possible explanation is that some effective treatment and preventions may help keep the new infection numbers under control.
        "),
    #create a slider with year in data range (1990-2017)
    sliderInput(inputId = "year2",
                label = "Choose a year",
                value = 2017, 
                min = 1990, 
                max = 2017, 
                width = "700px",
                sep=""),
    
    #plot map
    plotlyOutput(outputId = "newcase_map",
                 height = "500px",
                 width = "900px"), 

#-------------------------------------------------------------------------    
  h2("HIV is fatal & Serious"), 
  h4("Share of Population Death because of HIV"),
    p("Globally, 1.7% of deaths were caused by HIV/AIDS in 2017. 
      This share is high already, but in some countries, this share was much higher."),
    p("In the interactive map we can see the share of deaths which resulted from HIV/AIDS across the world. 
        Across most regions the share was low, for example, Europe accounted for less than 0.1% of deaths.
        But across some countries – focused primarily in Southern Sub-Saharan Africa – the share is very high. 
        More than 1-in-4 of deaths (28%) in South Africa and Botswana were caused by HIV/AIDS in 2017. 
        The share was also very high across Mozambique (24%); Namibia (23%); Zambia (18%); Kenya (17%); and Congo (15%)."),
    p("If we use the slider and choose a different year as the input, we can see that the share of death increased significantly since 1994, 
      in some countries the share of death even reached 60%. The upward trend did not stop until the Year 2006, and after Year 2006, the death share finally began to decrease."),
    
    sliderInput(inputId = "year",
                label = "Choose a year",
                value = 2000, 
                min = 1990, 
                max = 2017,
                width = "700px",
                sep=""),
    
    plotlyOutput(outputId = "death_share_map",
                 height = "500px",
                 width = "900px"),

#-------------------------------------------------------------------------
  h4("Death Rate among the most Severely HIV Infected Countries"),
    p("The large health burden of HIV/AIDS across Sub-Saharan Africa is also reflected in death rates. 
      Death rates measure the number of deaths from HIV/AIDS per 100,000 individuals in a country or region.
      If we take a further look at the Death Rate of HIV, 
      we could sadly see that for Turkey, Maldives and Comoros, the death rate of HIV increases with time; 
      while for many other countries, the death rate of HIV is under controlled gradually.
      "),
      plotlyOutput(outputId = "death_rate_line",
                   height = "500px",
                   width = "900px"),

  h4("HIV Death Rate Worldwide for Different Age Groups"),
    p("In the chart, we show death rates by age group. 
    We can see that baby <5 years old and young adults 15-49 are the riskiest groups. 
    Since HIV is primarily a sexually transmitted infection, where unsafe sex is a primary risk factor, this is what we would expect for the age 15-49 group. 
    But we also see that death rates are higher for young children <5 years old. This is because HIV can be transmitted from mother-to-child if the mother is infected.In the chart, we show death rates by age group. We can see that baby <5 years old and young adults 15-49 are the riskiest groups. Since HIV is primarily a sexually transmitted infection, where unsafe sex is a primary risk factor, this is what we would expect for the age 15-49 group. But we also see that death rates are higher for young children <5 years old. 
      This is because HIV can be transmitted from mother-to-child if the mother is infected."),
      plotlyOutput(outputId = "death_rate_age",
                   height = "500px",
                   width = "900px"),
#------------------------------------------------------------------------- 
  h2("HIV Can Be Prevented "), 
  h4("Antiretroviral Therapy Coverage of Infected Population"), 
    p("Antiretroviral therapy (ART) is a mixture of antiviral drugs that are used to treat people infected with HIV. 
    It allows people with HIV to live longer and prevents new HIV infections. Thanks to antiretroviral therapy (ART), people with HIV/AIDS can expect to live long lives. 
    This interactive map of ART coverage of the world shows what percentage of HIV-infected people in each country receive antiretroviral therapy, with darker green representing a higher percentage of people receiving ART. The slide bar above also allows filtering by years from 2000 to 2017. It shows that ART coverage has significantly increased in almost all countries within this time span. In 2000, only Australia and a few west European countries have over 30% ART coverage. In 2017 most countries reached this level, and especially African counties have shown significant growth in ART coverage. 
      For example, from 0% in Zimbabwe in 2000 to 84% in 2017."),

    sliderInput(inputId = "year_choose",
                label = "Choose a year",
                value = 2017, min = 2000, max = 2017, width = "700px",
                sep=""),
    
    plotlyOutput(outputId = "art_map",
                 height = "500px",
                 width = "900px"),

#-------------------------------------------------------------------------  
  h4("Education on AIDS Prevention among young people"),
    p("Education on HIV prevention to the young population is also an effective and crucial way to reduce HIV infection rate in a country. This bar chart contains the top 10 counties with the highest share of young people with knowledge on HIV prevention. The result shows that Antigua and Barbuda have the highest percentage and followed by Malawi, Belize, Peru and Lithuania."),
    plotOutput(outputId = "educ_graph",
               height = "400px",
               width = "700px"),

#------------------------------------------------------------------------- 
  h2("HIV related Text Analysis"),
    p("By using the Twitter API, we got 1000 'HIV' related tweets, 
      we cleaned it by removing non-English characters, remove stopwords... 
      and finally got a DocumentTermMatrix of the HIV-related tweets for plotting the word cloud.
      From the word cloud, we are able to see that 'People','amp','liberng' and 'testing' are the most frequently mentioned words with HIV content."),

       
     sliderInput("max",
                        "Maximum Number of Words:",
                        min = 1,  
                        max = 100,  
                        value = 30,
                        width = "300px"),
      
    plotOutput(outputId = "plot",
               height = "500px",
               width = "900px"),


#------------------------------------------------------------------------- 
h2("Does ART really decrease Death Rate?"),
p("Though the line's slope is not that significant, by selecting different years, it is clear that ART helps a lot to decrease the Death Rate of HIV. Especially, if we choose years around 2000 to 2010, the ART is negatively correlated with HIV Death Rate for ART coverage > 0.1 (10%). 
  Another piece of information we can get from this plot is that, only if the ART coverage is higher than 10%, it would save people's life."),

    sliderInput(inputId = "year_choose_2",
                label = "Choose a year",
                value = 2008, 
                min = 2000, 
                max = 2017, 
                width = "700px",
                sep=""),
    
    plotlyOutput(outputId = "death_art_line",
                 height = "500px",
                 width = "900px"),
#------------------------------------------------------------------------- 
  h2("Conclusion"),
     p("The world has made significant progress against HIV/AIDS. 
     Global deaths from AIDS have halved over the past decade. HIV/AIDS once accounted for more than 1-in-3 deaths in some countries, but rates are now falling.
     "),
     p("Global progress on HIV/AIDS has been driven by large improvements in countries which were most affected by the HIV epidemic. We saw that over the past decade the death rate has fallen as antiretroviral treatment has become more widely available, and therefore, we optimistically expect that the death rate of HIV would continue to decrease in the next decades. 
       More and more people are going to live without worry about being infected."),

 h2("Reference"),
 p(a(href = "https://ourworldindata.org/hiv-aids#the-global-distribution-of-deaths-from-hiv-aids", 
     "HIV Data Source we used(ourworldindata.org)"))

)
#------------------------------------------------------------------------- 
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
      z = ~Prevalence, 
      color = ~Prevalence, 
      colors = 'Blues',
      text = ~paste('</br> Country: ', Entity,
                    '</br> Share of Pop: ', round(Prevalence, digits = 2), "%",
                    '</br> Code: ', Code), 
      locations = ~Code, 
      marker = list(line = l),
      hoverinfo = "text"
    )
    fig <- fig %>% colorbar(title = 'Share of infected population', 
                            ticksuffix = '%', 
                            thickness = 15)
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
      z = ~Incidence, 
      color = ~Incidence, 
      colors = 'Blues',
      text = ~paste('</br> Country: ', Entity,
                    '</br> # of New Case: ', round(Incidence, digits = 2),
                    '</br> Code: ', Code), 
      locations = ~Code, 
      marker = list(line = l),
      hoverinfo = "text"
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
      z = ~share_death, 
      color = ~share_death, 
      colors = 'Reds',
      text = ~paste('</br> Country: ', Entity,
                    '</br> Share of Pop: ', round(share_death, digits = 2), "%",
                    '</br> Code: ', Code), 
      locations = ~Code, 
      marker = list(line = l),
      hoverinfo = "text"
    )
    fig <- fig %>% colorbar(title = 'Share of Death population', 
                            ticksuffix = '%', 
                            thickness = 15)
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
      labs(title="Top 8 Counties with Most Severly HIV Death Rates",
           x="Year",
           y="Death Rate of the Country, For All Age Groups (%)",
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
           y="HIV Death Rate Worldwide (%)",
           color = "Age Group",
           caption = "Source: https://ourworldindata.org/hiv-aids#the-global-distribution-of-deaths-from-hiv-aids") +
      theme(legend.position = 'top', 
            plot.title = element_text(colour = alpha('black')))
  })

# Word Cloud  
  hiv_tweet$text <- str_trim(gsub("[A-Z]{2,}","",hiv_tweet$text))
  hiv_tweet_corpus = Corpus(VectorSource(hiv_tweet$text))
  hiv_tweet_corpus = tm_map(hiv_tweet_corpus, content_transformer(tolower))
  hiv_tweet_corpus = tm_map(hiv_tweet_corpus, removeNumbers)
  hiv_tweet_corpus = tm_map(hiv_tweet_corpus, removePunctuation)
  hiv_tweet_corpus = tm_map(hiv_tweet_corpus, removeWords, c("the", "and", stopwords("english")))
  hiv_tweet_corpus =  tm_map(hiv_tweet_corpus, stripWhitespace)
  #Create a DTM successful
  hiv_dtm <- DocumentTermMatrix(hiv_tweet_corpus)
  
  freq_s = data.frame(sort(colSums(as.matrix(hiv_dtm)), 
                           decreasing=TRUE))
  
  output$plot <- renderPlot({
    wordcloud(rownames(freq_s),
              freq_s[,1],
              max.words=input$max,
              colors=brewer.pal(8, "Dark2"),
              scale=c(3.5,0.25))
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
      z = ~Coverage, 
      color = ~Coverage, 
      colors = 'Greens',
      text = ~paste('</br> Country: ', Entity,
                    '</br> ART Coverage: ', Coverage, "%",
                    '</br> Code: ', Code), 
      locations = ~Code, 
      marker = list(line = line),
      hoverinfo = "text"
    )
    p <- p %>% colorbar(title = 'ART Coverage', ticksuffix = '%', thickness = 15)
    p <- p %>% layout(
      title = 'Antiretroviral Therapy Coverage of Infected Population',
      geo = geo
    )
    p
  }) 
  
  
  #output for educ graph  
  output$educ_graph<- renderPlot({
    
    fig <- ggplot(educ, aes(Entity, `HIV Prevention Education Coverage`)) + 
      geom_col(aes(fill=`HIV Prevention Education Coverage`)) +
      scale_fill_distiller(palette = "Blues", guide = FALSE, direction = 1) +
      labs(title="HIV Education",
           x="country",
           y="% of young population with AIDS Prevention Knowledge",
           caption = "Source: https://ourworldindata.org/hiv-aids#the-global-distribution-of-deaths-from-hiv-aids") +
      theme(legend.position = 'top', 
            plot.title = element_text(colour = alpha('black')))
    fig
    
  })
  
  output$death_art_line <- renderPlotly({
    
    dra_year <- death_rate_art %>% filter(Year == input$year_choose_2)
    
    ggplot(data = dra_year,
           aes(x = ART, 
               y = all)) + 
      geom_jitter() +
      geom_smooth() +
      labs(title="HIV Death Rate (per 100,000) with Respect to ART Coverage",
           x="ART Coverage Rate (%)",
           y="HIV Death Rate in A Country",
           caption = "Source: https://ourworldindata.org/hiv-aids#the-global-distribution-of-deaths-from-hiv-aids") +
      theme(legend.position = 'top', 
            plot.title = element_text(colour = alpha('black')))
    
  })
  
}

shinyApp(ui = ui,server = server)
