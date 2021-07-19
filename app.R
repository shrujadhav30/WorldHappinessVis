# Data Visualization Project
# Student Name: Shruti J


# Installing and Loading Libraries
Install_And_Load <- function(Required_Packages)
{
  Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[,"Package"])];
  
  if(length(Remaining_Packages)) 
  {
    install.packages(Remaining_Packages);
  }
  for(package_name in Required_Packages)
  {
    library(package_name,character.only=TRUE,quietly=TRUE);
  }
}

Required_Packages = c("shiny", "ggplot2", "dplyr", "leaflet", "stringr", "maps", "readr", "shinydashboard", "tidyr", "shinythemes", "plotly", "RColorBrewer", "corrplot", "countrycode", "DT", "ggrepel", "SwimmeR", "shinyWidgets", "shinyjs")
Install_And_Load(Required_Packages);

#library(shiny)
#library(ggplot2)
#library(dplyr)
#library(leaflet)
#library(stringr)
#library(maps)
#library(readr)
#library(shinydashboard)
#library(tidyr)
#library(shinythemes)
#library(plotly)
#library(RColorBrewer)
#library(corrplot)
#library(countrycode)
#library(DT)
#library(ggrepel)
#library(SwimmeR)
#library(shinyWidgets)
#library(shinyjs)


#Importing data
happiness2019 <- read.csv("./HappinessReport.csv",strip.white = TRUE, stringsAsFactors = FALSE)
countries <- read.csv("./Countries.csv", strip.white = TRUE, stringsAsFactors = FALSE)

#Cleaning and preprocessing countries data
countries <- subset(countries, select = -c(Climate, Other....))
countries$Country <- trimws(countries$Country)
countries$Region <- trimws(countries$Region)
countries <- countries %>% rename(Area_persqm = Area..sq..mi..,
                                  Population_Density_persqm = Pop..Density..per.sq..mi..,
                                  Coast_Area_Ratio = Coastline..coast.area.ratio.,
                                  Infant_mortality= Infant.mortality..per.1000.births.,
                                  GDP_percapita = GDP....per.capita.,
                                  Literacy = Literacy....,
                                  Phones_per1000 = Phones..per.1000.,
                                  Arable = Arable....,
                                  Crops = Crops....)

countries$Region <- factor(countries$Region, levels = c("ASIA (EX. NEAR EAST)", "BALTICS", "C.W. OF IND. STATES", "EASTERN EUROPE", "LATIN AMER. & CARIB", "NEAR EAST", "NORTHERN AFRICA", "NORTHERN AMERICA", "OCEANIA", "SUB-SAHARAN AFRICA", "WESTERN EUROPE"))
countries$Population_Density_persqm <- str_replace(countries$Population_Density_persqm,",",".") %>% as.double(countries$Population_Density_persqm)
countries$Coast_Area_Ratio <- str_replace(countries$Coast_Area_Ratio,",",".") %>% as.double(countries$Coast_Area_Ratio)
countries$Net.migration <- str_replace(countries$Net.migration,",",".") %>% as.double(countries$Net.migration)
countries$Infant_mortality <- str_replace(countries$Infant_mortality,",",".") %>% as.double(countries$Infant_mortality)
countries$Literacy <- str_replace(countries$Literacy,",",".") %>% as.double(countries$Literacy)
countries$Phones_per1000 <- str_replace(countries$Phones_per1000,",",".") %>% as.double(countries$Phones_per1000)
countries$Arable <- str_replace(countries$Arable,",",".") %>% as.double(countries$Arable)
countries$Crops <- str_replace(countries$Crops,",",".") %>% as.double(countries$Crops)
countries$Birthrate <- str_replace(countries$Birthrate,",",".") %>% as.double(countries$Birthrate)
countries$Deathrate <- str_replace(countries$Deathrate,",",".") %>% as.double(countries$Deathrate)
countries$Agriculture <- str_replace(countries$Agriculture,",",".") %>% as.double(countries$Agriculture)
countries$Industry <- str_replace(countries$Industry,",",".") %>% as.double(countries$Industry)
countries$Service <- str_replace(countries$Service,",",".") %>% as.double(countries$Service)


# Joining both the datasets 
join2019 <- happiness2019 %>% left_join(countries, by = c('Country.or.region' = 'Country'))
Countrys <- join2019$Country.or.region
# Some more preprocessing on joined data
join2019$code <- countrycode(join2019$Country.or.region,'country.name','iso3c')
join2019$Literacy[is.na(join2019$Literacy)] <- median(join2019$Literacy, na.rm = TRUE)
join2019$Birthrate[is.na(join2019$Birthrate)] <- median(join2019$Birthrate, na.rm = TRUE)
join2019$Deathrate[is.na(join2019$Deathrate)] <- median(join2019$Deathrate, na.rm = TRUE)
join2019$Phones_per1000[is.na(join2019$Phones_per1000)] <- median(join2019$Phones_per1000, na.rm = TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(
      #Navbar Structure for UI                 
    navbarPage("World Happiness Report",
        tabPanel("Rank Finder", fluid = TRUE, icon = icon("globe-americas"),
                 dashboardPage(
                  dashboardHeader(disable = TRUE),
                  dashboardSidebar(disable = TRUE),
                  dashboardBody(
                    fluidRow(
                      
                      p("
The 3D world map displays an overview of the happiness scores of all the countries from the", a("World Happiness Report.", href = "https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv"), " By selecting a country from the map, you can see a radar chart that shows values of Generosity, Absesnce of Corruption, Literacy, Freedom and Happiness score of that country. 
"),
                    hr(), 
                     
                    ),
                    
                    fluidRow(
                      box(
                        title = "World Happiness Map: Year 2019", solidHeader = TRUE, status = "primary",
                        helpText("Please hover over the country to see details."),
                        plotlyOutput("Mapoutput"),
                        width = 6
                      ),
                      box(
                        title = "Radar Chart of a selected country", solidHeader = TRUE, status = "primary",
                        helpText("Please Select a country from the map to display radar chart."),
                        plotlyOutput("Radaroutput"),
                        width = 6
                      )
                    ),
                    
                    fluidRow(
                      
                      p("By selecting two countries from the menu below, you can compare different factors that contribute to the happiness scores of the selected countries."),
                    
                      
                    ),
                    
                    fluidRow(
                      box(
                        title = "Comparison between the Countries",solidHeader = TRUE, status = "primary",
                        helpText("Select two Countries for comparison."),
                        hr(),
                        selectInput("Cnt1", "First Country: ", choices = Countrys, selected = "India"),
                        selectInput("Cnt2", "Second Country: ", choices = Countrys, selected = "Australia"),
                        width = 4
                      ),
                      box(
                        title = "Country comparison Bar Graph", solidHeader = TRUE, status = "primary",
                        plotlyOutput("Barplot"),
                        width = 8
                      )  
                    ))
                 )),
        tabPanel("Rank Comparator", fluid = TRUE, icon = icon("atlas"),
                 fluidRow(
                   
                   p("The scatter plot displays the countries with respect to their happiness score and the selected factor. Depending on the selected factor the list of top 10 and bottom 10 countries will be updated.  "), 
                   
                   hr(),
                   
                 ),
                 fluidRow(
                  box(
                    title = "Comaprison between different factors affecting Happiness scores", solidHeader = TRUE, status = "primary",
                    helpText("Please Select the factor from below to check if it is affecting the happiness of countries:"),
                    hr(),
                    radioButtons(inputId = "FCT", "Factors Affecting Happiness Rank: ",
                                 c("GDP Per Capita Score" = "GDP",
                                   "Social Support Score" = "SS",
                                   "Life Expectancy Score" = "LFE",
                                   "Freedom Score" = "FDM",
                                   "Generosity Score" = "GEN",
                                   "Absence of Corruption Score" = "COR",
                                   "Literacy Rate" = "LIT",
                                   "Population Density" = "POP"
                                   )),
                    width = 3
                  ),
                  box(
                    title = "Scatter Plot of effect of selected factor on Happiness Scores of Countries", solidHeader = TRUE, status = "primary",
                    plotlyOutput("SCplot"),
                    width = 9
                  )
                 ),
                 fluidRow(
                   box(
                     title = "Top 10 Countries with respect to the selected factor", solidHeader = TRUE, status = "primary",
                     plotlyOutput("T10hbar"),
                     width = 6
                   ),
                   box(
                     title = "Bottom 10 Countries with respect to the selected factor", solidHeader = TRUE, status = "primary",
                     plotlyOutput("B10hbar"),
                     width = 6
                   )
                 )
                 ),
        tabPanel("About", fluid = TRUE, icon = icon("info-circle"),
                 fluidRow(
                   hr(),
                   p("The world happiness resolution was started by the UN general assembly in 2011, to guide the public policy they invited the member countries to measure the happiness of their people. The first World Happiness Report was released on April 1, 2012 which drew international attention. The report highlighted the state of world happiness through several case studies and explained the reasons for happiness and misery. In 2013 the second happiness report was released and from 2015 it became a annual thing. The report basically uses the Gallup World poll data, and the reports can be viewed on the", a("World Happiness Report.", href = "https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv"), 
"The progress of nations is assessed by the experts in fields of psychology, economics, survey analysis and national statistics."), 
p("The World Happiness report of 2019 ranks 156 countries by their happiness levels. The report specifies six key variables that support well being of the nations. Those six variables are income, social support, freedom, trust, healthy life expectancy and generosity.  Each variable measured reveals a populated-weighted average score on a scale running from 0 to 10 that is tracked over time and compared against other countries. These variables currently include: real GDP per capita, social support, healthy life expectancy, freedom to make life choices, generosity, and perceptions of corruption for the 2019 report. 
For the purpose of analysis apart from the six variables we have added extra measures of countries like population, population density and literacy to check if we can derive a relation between these factors and the happiness rank of each nation. 
"),
                  
                   hr(),
                   
                 ),
                 
                 )
        
        
        
        )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
##----------------------------------------------------------Tab 1 Graphs------------------------------------------------------------------
  
  
  output$Barplot<-renderPlotly({
    abc<- join2019 %>% filter(Country.or.region %in% c(input$Cnt1, input$Cnt2)) %>% gather(key = "factor", value = "score", 4:9)%>% select(Country.or.region,factor, score)
    plot_ly(data = abc,
            x = ~ factor,
            y= ~ score,
            type = "bar",
            color = ~ Country.or.region,
            colors = c("#fec44f", "#7fcdbb")
    ) %>%
      layout(
        yaxis = list(zeroline = FALSE, title = "Score"),
        xaxis = list(zeroline = FALSE, title = "Factors", categoryorder = "array", categoryarray = ~factor)
      )
  })
  
  #------------------------------------------------------------------------------------- MAP--------------------------------------------------
  
  # light grey boundaries
  l <- list(color = toRGB("#d1d1d1"), width = 0.5)
  
  # specify map projection/options
  g <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'orthographic'),
    #projection = list(type = 'Mercator'),
    resolution = '100',
    showcountries = TRUE,
    countrycolor = '#d1d1d1',
    showocean = TRUE,
    oceancolor = '#c9d2e0',
    showlakes = TRUE,
    lakecolor = '#99c0db',
    showrivers = TRUE,
    rivercolor = '#99c0db'
  )
  
  output$Mapoutput <- renderPlotly({
    
    plot_geo(join2019, source = 'Mapout') %>%
      add_trace(
          z = ~Score, color = ~Score, colors = 'Greens',
          # Hover text:
          text = ~with(data=join2019, paste("<b>Country:</b> ", Country.or.region,
                                              "<br><b>Region:</b> ", Region,
                                              "<br><b>Happiness Rank: </b>", Overall.rank,
                                              "<br><b>Happiness Score: </b>", Score)),
          locations = ~code, marker = list(line = l)) %>%
        colorbar(title = 'Happiness Score') %>%
        layout( geo = g )
  })
  
  
  
  output$Radaroutput <- renderPlotly({
      s <- event_data("plotly_click", source = 'Mapout', priority = "event")
      
      if (!is.null(s)){
      updatedCountry <- join2019%>%filter(Score == s$z)
      #updateSelectInput(session, 'Country1', selected = updatedCountry)
      
      #SelectedCountry <- join2019%>%filter(Country.or.region == input$Country1)
      plot_ly(data = updatedCountry, type = "scatterpolar",
              r = c(~Score*10,~Generosity*100, ~Perceptions.of.corruption*100, ~Literacy, ~Freedom.to.make.life.choices*100, ~Score*10),
              theta = c('Happiness Score', 'Generosity', 'Absence of Corruption', 'Literacy', 'Freedom', 'Happiness Score'),
              fill = 'toself')  %>% layout(polar = list(
                radialaxis = list(
                  visible = T,
                  range = c(0,100)
                )
              ),
              showlegend = F
              )
      }
    })
  
  
## ----------------------------------------------------------------Tab 2 Grpahs--------------------------------------------------------------------
     
     
     output$SCplot <- renderPlotly(
       suppressWarnings(
       switch (input$FCT,
         GDP = plot_ly(data=join2019,x=~Score,
                   y=~GDP.per.capita,
                   color=~Region,
                   type="scatter", mode = "markers",
                   marker = list(size = 15),
                   # Hover text:
                   text = ~paste("<b>Region:</b> ", Region, 
                                 "<br><b>Country:</b> ", Country.or.region,
                                 "<br><b>Happiness Score:</b>", Score,
                                 "<br><b>GDP per capita:</b>", GDP.per.capita)) %>%
             layout(xaxis=list(title="Happiness Score"),
                    yaxis=list(title="GDP per Capita Score") ),
         
         SS = plot_ly(data=join2019,x=~Score,
                      y=~Social.support,
                      color=~Region,
                      type="scatter", mode = "markers",
                      marker = list(size = 15),
                      # Hover text:
                      text = ~paste("<b>Region:</b> ", Region, 
                                    "<br><b>Country:</b> ", Country.or.region,
                                    "<br><b>Happiness Score:</b>", Score,
                                    "<br><b>Social Support Score:</b>", Social.support)) %>%
           layout(xaxis=list(title="Happiness Score"),
                  yaxis=list(title="Social Support Score") ),
         
         LFE = plot_ly(data=join2019,x=~Score,
                      y=~Healthy.life.expectancy,
                      color=~Region,
                      type="scatter", mode = "markers",
                      marker = list(size = 15),
                      # Hover text:
                      text = ~paste("<b>Region:</b> ", Region, 
                                    "<br><b>Country:</b> ", Country.or.region,
                                    "<br><b>Happiness Score:</b>", Score,
                                    "<br><b>Life Expectancy Score:</b>", Healthy.life.expectancy)) %>%
           layout(xaxis=list(title="Happiness Score"),
                  yaxis=list(title="Healthy Life Expectancy Score") ),
         
         FDM = plot_ly(data=join2019,x=~Score,
                      y=~Freedom.to.make.life.choices,
                      color=~Region,
                      type="scatter", mode = "markers",
                      marker = list(size = 15),
                      # Hover text:
                      text = ~paste("<b>Region:</b> ", Region, 
                                    "<br><b>Country:</b> ", Country.or.region,
                                    "<br><b>Happiness Score:</b>", Score,
                                    "<br><b>Freedom Score:</b>", Freedom.to.make.life.choices)) %>%
           layout(xaxis=list(title="Happiness Score"),
                  yaxis=list(title="Freedom to Make Life Choices Score") ),
         
         GEN = plot_ly(data=join2019,x=~Score,
                      y=~Generosity,
                      color=~Region,
                      type="scatter", mode = "markers",
                      marker = list(size = 15),
                      # Hover text:
                      text = ~paste("<b>Region:</b> ", Region, 
                                    "<br><b>Country:</b> ", Country.or.region,
                                    "<br><b>Happiness Score:</b>", Score,
                                    "<br><b>Generosity Score:</b>", Generosity)) %>%
           layout(xaxis=list(title="Happiness Score"),
                  yaxis=list(title="Generosity Score") ),
         
         COR = plot_ly(data=join2019,x=~Score,
                      y=~Perceptions.of.corruption,
                      color=~Region,
                      type="scatter", mode = "markers",
                      marker = list(size = 15),
                      # Hover text:
                      text = ~paste("<b>Region:</b> ", Region, 
                                    "<br><b>Country:</b> ", Country.or.region,
                                    "<br><b>Happiness Score:</b>", Score,
                                    "<br><b>Perception of Corruption:</b>", Perceptions.of.corruption)) %>%
           layout(xaxis=list(title="Happiness Score"),
                  yaxis=list(title="Perception of Corruption Score") ),
         
         LIT = plot_ly(data=join2019,x=~Score,
                      y=~Literacy,
                      color=~Region,
                      type="scatter", mode = "markers",
                      marker = list(size = 15),
                      # Hover text:
                      text = ~paste("<b>Region:</b> ", Region, 
                                    "<br><b>Country:</b> ", Country.or.region,
                                    "<br><b>Happiness Score:</b>", Score,
                                    "<br><b>Literacy Score:</b>", Literacy)) %>%
           layout(xaxis=list(title="Happiness Score"),
                  yaxis=list(title="Literacy Score") ),
         
         POP = plot_ly(data=join2019,x=~Score,
                      y=~Population_Density_persqm,
                      color=~Region,
                      type="scatter", mode = "markers",
                      marker = list(size = 15),
                      # Hover text:
                      text = ~paste("<b>Region:</b> ", Region, 
                                    "<br><b>Country:</b> ", Country.or.region,
                                    "<br><b>Happiness Score:</b>", Score,
                                    "<br><b>Population Density(per sqm):</b>", Population_Density_persqm)) %>%
           layout(xaxis=list(title="Happiness Score"),
                  yaxis=list(title="Population_Density(per sqm)") ),
         
       )) )
    

     
     # Top 10 Countries according to the factor selected
      plot_hbar<-function(data, value1){
        plot_ly( data = data,
          x= data[ ,value1],
          y= ~Country.or.region,
          type = "bar",
          color = I("#fdae6b"),
          text= ~ paste("<b>Region:</b> ", Region, 
                        "<br><b>Happiness Score:</b>", Score
                        )
        ) %>%
          layout(
            yaxis = list(zeroline = FALSE,title = "Countries", categoryorder = "array", categoryarray = ~data[value1]),
            xaxis = list(zeroline = FALSE, title = colnames(data[value1]))
          )
      }
     
      output$T10hbar <- renderPlotly({
        switch(input$FCT,
               GDP = {
                 filterdata <- join2019[order(-join2019$GDP.per.capita),]
                 filterdata <- filterdata[1:10,]
                 filterdata <- filterdata[order(filterdata$GDP.per.capita),]
                 plot_hbar(filterdata, 4)     
               },
               SS = {
                 filterdata <- join2019[order(-join2019$Social.support),]
                 filterdata <- filterdata[1:10,]
                 filterdata <- filterdata[order(filterdata$Social.support),]
                 plot_hbar(filterdata, 5)
               },
               LFE = {
                 filterdata <- join2019[order(-join2019$Healthy.life.expectancy),]
                 filterdata <- filterdata[1:10,]
                 filterdata <- filterdata[order(filterdata$Healthy.life.expectancy),]
                 plot_hbar(filterdata, 6)
               },
               FDM = {
                 filterdata <- join2019[order(-join2019$Freedom.to.make.life.choices),]
                 filterdata <- filterdata[1:10,]
                 filterdata <- filterdata[order(filterdata$Freedom.to.make.life.choices),]
                 plot_hbar(filterdata, 7)
               },
               GEN = {
                 filterdata <- join2019[order(-join2019$Generosity),]
                 filterdata <- filterdata[1:10,]
                 filterdata <- filterdata[order(filterdata$Generosity),]
                 plot_hbar(filterdata, 8)
               },
               COR = {
                 filterdata <- join2019[order(-join2019$Perceptions.of.corruption),]
                 filterdata <- filterdata[1:10,]
                 filterdata <- filterdata[order(filterdata$Perceptions.of.corruption),]
                 plot_hbar(filterdata, 9)
               },
               LIT = {
                 filterdata <- join2019[order(-join2019$Literacy),]
                 filterdata <- filterdata[1:10,]
                 filterdata <- filterdata[order(filterdata$Literacy),]
                 plot_hbar(filterdata, 18)
               },
               POP = {
                 filterdata <- join2019[order(-join2019$Population_Density_persqm),]
                 filterdata <- filterdata[1:10,]
                 filterdata <- filterdata[order(filterdata$Population_Density_persqm),]
                 plot_hbar(filterdata, 13)
               }
                       
        )  
      })
      
      
      
      # Bottom 10 countries for selected factor
      output$B10hbar <- renderPlotly({
        switch(input$FCT,
               GDP = {
                 filterdata <- join2019[order(join2019$GDP.per.capita),]
                 filterdata <- filterdata[1:10,]
                 filterdata <- filterdata[order(-filterdata$GDP.per.capita),]
                 plot_hbar(filterdata, 4)     
               },
               SS = {
                 filterdata <- join2019[order(join2019$Social.support),]
                 filterdata <- filterdata[1:10,]
                 filterdata <- filterdata[order(-filterdata$Social.support),]
                 plot_hbar(filterdata, 5)
               },
               LFE = {
                 filterdata <- join2019[order(join2019$Healthy.life.expectancy),]
                 filterdata <- filterdata[1:10,]
                 filterdata <- filterdata[order(-filterdata$Healthy.life.expectancy),]
                 plot_hbar(filterdata, 6)
               },
               FDM = {
                 filterdata <- join2019[order(join2019$Freedom.to.make.life.choices),]
                 filterdata <- filterdata[1:10,]
                 filterdata <- filterdata[order(-filterdata$Freedom.to.make.life.choices),]
                 plot_hbar(filterdata, 7)
               },
               GEN = {
                 filterdata <- join2019[order(join2019$Generosity),]
                 filterdata <- filterdata[1:10,]
                 filterdata <- filterdata[order(-filterdata$Generosity),]
                 plot_hbar(filterdata, 8)
               },
               COR = {
                 filterdata <- join2019[order(join2019$Perceptions.of.corruption),]
                 filterdata <- filterdata[1:10,]
                 filterdata <- filterdata[order(-filterdata$Perceptions.of.corruption),]
                 plot_hbar(filterdata, 9)
               },
               LIT = {
                 filterdata <- join2019[order(join2019$Literacy),]
                 filterdata <- filterdata[1:10,]
                 filterdata <- filterdata[order(-filterdata$Literacy),]
                 plot_hbar(filterdata, 18)
               },
               POP = {
                 filterdata <- join2019[order(join2019$Population_Density_persqm),]
                 filterdata <- filterdata[1:10,]
                 filterdata <- filterdata[order(-filterdata$Population_Density_persqm),]
                 plot_hbar(filterdata, 13)
               }
               
        )  
      })
      
     
}

# Run the application 
shinyApp(ui = ui, server = server)
