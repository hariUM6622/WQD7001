library(shiny)
library(shinythemes)
library(shinycustomloader)
library(leaflet)
library(DT)
library(ggplot2)
library(dplyr)
library(sf)
library(plotly)

#this si the theme for the app
ui <- fluidPage(
    theme = shinytheme('cyborg'),
    
    #-- Header layout --#
    titlePanel('A Comparative Analysis of Chinese and Western responses to COVID-19'),
    p('How are the USA, Canada and China responses towards Covid-19'),
    hr(), #________
    
    #-- Dropdowns layout --#
    fluidRow(
        column(
            width = 3,
            uiOutput('regions')
        ),
        column(
            width = 9,
            uiOutput('variables')
        )
    ),
    
    #-- Tabs layout --#
    tabsetPanel(
        id = 'tab',
        
        tabPanel(
            'Cases',
            
            h3('Total Covid-19 Cases From December 2019 up to December, 2021'),
            p('Select a country and a variable from above. Hover over individual countries for info.'),
            
            withLoader(
                leafletOutput('map_cases', width = '100%', height = 500),
                loader = 'pacman'
            )
            
        ),
        tabPanel(
          'Response',
          
          h3('Government responses to COVID-19 outbreak on December, 2021'),
          p('Select a country and a variable from above. Hover over individual countries for info.'),
          
          withLoader(
            leafletOutput('map_response', width = '100%', height = 500),
            loader = 'pacman'
          ),
          
          hr(), #________
          
          h3('Correlation Analysis'),
          p('This is how the selected variable is correlated with the total number of confirmed cases. Mouse over individual points for country info.'),
          
          withLoader(
            plotlyOutput('correlation_response', width = '100%', height = 500),
            loader = 'pacman'
          )
          
        ),
        tabPanel(
            'Population',
            
            h3('Population Map'),
            p('Select a region and a variable from above. Hover over individual countries for info.'),
            
            withLoader(
                leafletOutput('map_population', width = '100%', height = 500),
                loader = 'pacman'
            ),
            
            hr(), #________
            
            h3('Correlation Analysis'),
            p('This is how the selected variable is correlated with the total number of confirmed cases. Mouse over individual points for country info.'),
            
            withLoader(
                plotlyOutput('correlation_population', width = '100%', height = 500),
                loader = 'pacman'
            )
        ),
        tabPanel(
            'Health',
            
            h3('Health Map'),
            p('Select a region and a variable from above. Mouse over individual countries for info.'),
            
            withLoader(
                leafletOutput('map_health', width = '100%', height = 500),
                loader = 'pacman'
            ),
            
            hr(), #________
            
            h3('Correlation Analysis'),
            p('This is how the selected variable is correlated with the total number of confirmed cases. Mouse over individual points for country info.'),
            
            withLoader(
                plotlyOutput('correlation_health', width = '100%', height = 500),
                loader = 'pacman'
            )
        ),
        tabPanel(
            'Education',
            
            h3('Education Map'),
            
            p('Select a region and a variable from above. Mouse over individual countries for info.'),
            withLoader(
                leafletOutput('map_education', width = '100%', height = 500),
                loader = 'pacman'
            ),
            
            hr(), #________
            
            h3('Correlation Analysis'),
            p('This is how the selected variable is correlated with the total number of confirmed cases. Mouse over individual points for country info.'),
            
            withLoader(
                plotlyOutput('correlation_education', width = '100%', height = 500),
                loader = 'pacman'
            )
        ),
        tabPanel(
            'Economy',
            
            h3('Economy Map'),
            p('Select a region and a variable from above. Mouse over individual countries for info.'),
            
            withLoader(
                leafletOutput('map_economy', width = '100%', height = 500),
                loader = 'pacman'
            ),
            
            hr(), #________
            
            h3('Correlation Analysis'),
            p('This is how the selected variable is correlated with the total number of confirmed cases. Mouse over individual points for country info.'),
            
            withLoader(
                plotlyOutput('correlation_economy',width = '100%', height = 500),
                loader = 'pacman'
            )
        ),
        tabPanel(
            'Happiness',
            
            h3('Happiness Map'),
            p('Select a region and a variable from above. Mouse over individual countries for info.'),
            
            withLoader(
                leafletOutput('map_happiness', width = '100%', height = 500),
                loader = 'pacman'
            ),
            
            hr(), #________
            
            h3('Correlation Analysis'),
            p('This is how the selected variable is correlated with the total number of confirmed cases. Mouse over individual points for country info.'),
            
            withLoader(
                plotlyOutput('correlation_happiness', width = '100%'),
                loader = 'pacman'
            )
        ),
        tabPanel(
            'Data',
            
            h3('COVID-19 Cases and All Variables'),
            p('Below is the data used for this project. You may select a region from above. You may also browse, sort and search in the table below.'),
            
            withLoader(
                DTOutput('data'),
                loader = 'pacman'
            ),
            
            h3('Data Sources'),
            tags$ul(
              tags$li('Best Healthcare ranking: ', a('https://www.who.int/healthinfo/paper30.pdf', target = '_blank')),
              tags$li('Current health expenditure (% of GDP): ', a('https://data.worldbank.org/indicator/SH.XPD.CHEX.GD.ZS', target = '_blank')),
              tags$li('Hospital beds: ', a('https://www.kaggle.com/ikiulian/global-hospital-beds-capacity-for-covid19?select=hospital_beds_global_v1.csv', target = '_blank')),
              tags$li('Covid test: ', a('https://ourworldindata.org/coronavirus-testing#how-many-tests-are-performed-each-day', target = '_blank')),
              tags$li('Doctors and Nurses: ', a('https://data.oecd.org/healthres/doctors.htm#indicator-chart', target = '_blank')),
              tags$li('Novel Coronavirus (COVID-19) Cases Data: ', a('https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases', target = '_blank')),
              tags$li('The World Bank: ', a('https://data.worldbank.org/indicator', target = '_blank')),
              tags$li('World Happiness Report: ', a('https://www.kaggle.com/unsdsn/world-happiness', target = '_blank')),
              tags$li('Government Expenditure on Education: ', a('https://data.worldbank.org/indicator/SE.XPD.TOTL.GD.ZS?view=chart', target = '_blank')),
              tags$li('Adult female literacy rate: ', a('https://data.worldbank.org/indicator/SE.ADT.LITR.FE.ZS?view=chart', target = '_blank')),
              tags$li('Adult male literacy rate: ', a('https://data.worldbank.org/indicator/SE.ADT.LITR.MA.ZS?view=chart', target = '_blank')),
              tags$li('Youth female literacy rate: ', a('https://data.worldbank.org/indicator/SE.ADT.1524.LT.FE.ZS?view=chart', target = '_blank')),
              tags$li('Youth male literacy rate: ', a('https://data.worldbank.org/indicator/SE.ADT.1524.LT.MA.ZS?view=chart', target = '_blank')),
              tags$li('Adult total literacy rate: ', a('https://data.worldbank.org/indicator/SE.ADT.LITR.ZS?view=chart', target = '_blank')),
              tags$li('Youth total literacy rate: ', a('https://data.worldbank.org/indicator/SE.ADT.1524.LT.ZS?view=chart ', target = '_blank')),
              tags$li('Cause of death, by communicable diseases and maternal, prenatal and nutrition conditions (% of total): ', a('https://data.worldbank.org/indicator/SH.DTH.COMM.ZS', target = '_blank')),
              tags$li('Cause of death, by injury (% of total): ', a('https://data.worldbank.org/indicator/SH.DTH.INJR.ZS?view=map', target = '_blank')),
              tags$li('Cause of death, by non-communicable diseases (% of total): ', a('https://data.worldbank.org/indicator/SH.DTH.NCOM.ZS', target = '_blank')),
              tags$li('Death rate, crude (per 1,000 people): ', a('https://data.worldbank.org/indicator/SP.DYN.CDRT.IN', target = '_blank')),
              tags$li('Diabetes prevalence (% of population ages 20 to 79): ', a('https://data.worldbank.org/indicator/SH.STA.DIAB.ZS', target = '_blank')),
              tags$li('Immunization, DPT (% of children ages 12-23 months): ', a('https://data.worldbank.org/indicator/SH.IMM.IDPT', target = '_blank')),
              tags$li('Immunization, HepB3 (% of one-year-old children): ', a('https://data.worldbank.org/indicator/SH.IMM.HEPB', target = '_blank')),
              tags$li('Immunization, measles (% of children ages 12-23 months): ', a('https://data.worldbank.org/indicator/SH.IMM.MEAS', target = '_blank')),
              tags$li('Life expectancy at birth, total (years): ', a('https://data.worldbank.org/indicator/SP.DYN.LE00.IN', target = '_blank')),
              tags$li('Prevalence of anemia among children (% of children under 5): ', a('https://data.worldbank.org/indicator/SH.ANM.CHLD.ZS', target = '_blank')),
              tags$li('Prevalence of undernourishment (% of population): ', a('https://data.worldbank.org/indicator/SN.ITK.DEFC.ZS', target = '_blank')),
              tags$li('Risk of impoverishing expenditure for surgical care (% of people at risk): ', a('https://data.worldbank.org/indicator/SH.SGR.IRSK.ZS', target = '_blank')),
              tags$li('COVID-19 Government Response Tracker: ', a('https://www.bsg.ox.ac.uk/research/research-projects/covid-19-government-response-tracker', target = '_blank'))
            )              
        ),
        
        tabPanel(
            'User Guide',
            
            h3('How to Use The App?'),
            
            tags$ol(
                
                tags$li('The landing page is a world map indicate the number of total Covid-19 cases as of December 19, 2020 for all countries.'),
                
                tags$li('Selection of confirmed cases, recovered cases and death cases is available at top right.'),
      
                
                tags$li('Select the region from drop down to zoom in the world map by region.'),
               
                
                tags$li('There are 5 different categories of social determinants of health (SDH) namely Population, Health, Education, Economy and Happiness. Click on the tab, to observe the correlation between Covid-19 cases and SDH.'),
              
                
                tags$li('Correlation analysis scatter plot will be displayed below the map.'),
                
                
                tags$li('Click on region to select or deselect the region.'),
               
                
                tags$li('Click on Toggle Spike Lines to toggle the lines to axis X & Y with details.'),
            
                
                tags$li('Click show closest data on hover to show the details of the data point.'),
               
                
                tags$li('Click compare data on hover to compare data point sitting near the same vertical line.'),
                
                
                tags$li('There are multiple datasets available for each category.'),

                h5('Population'),
          
                h5('Health'),
                
                h5('Education'),
             
                h5('Economy'),
              
                h5('Happiness'),
              
                
                tags$li('The dataset for each country is available under Data tab.'),
                
                
                tags$li('Enter the country name/region to search the data by country/region.'),
                
                
                tags$li('Click on expand icon the view data of each variables.'),

            ),
        ),
        tabPanel(
            'Codebook',
            
            h3('Attributes of Dataset'),
            p('This is the list of attributes (columns) available in the datasets'),
            
            withLoader(
                tableOutput('notes'),
                loader = 'pacman'
            )
        )
    ),
    
    hr(), #________
    
    #-- Footer layout --#
    p(
        'Created by Haritharan(S2157199),Zhang Ziteng (S2149768),Zhang Jingrong (s2111224) ,JIE YINGGANG (s2113868) , Siti Khadijah Mohamed Afni (S2016903)| '
  )
)
