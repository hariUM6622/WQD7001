library(shiny)
library(sf)
library(plotly)
library(bslib)

#install.packages("shinythemes")
# state the dataframe source file.
source('data.R')

#the this function act as receiver input and shows output
function(input, output, session) {
    values <- reactiveValues(
        region = 'All Countries',
        category = 'Cases'
    )   
    
    #-- Load data --#
    load_data <- reactive({
        print('Loading data')
        #read the dataset csv file
        read.csv('latest_cleansed_data.csv', check.names = FALSE) %>%
            rename(
                'Cumulative confirmed cases' = 'Confirmed',
                'Cumulative recovered cases' = 'Recovered',
                'Cumulative death cases' = 'Deaths'
            ) %>%
            select(
                Country = Country_Name,
                Region,
                
                ### must have all categories, order not important
                GetColumns('Cases'),
                GetColumns('Response'),
                GetColumns('Population'),
                GetColumns('Health'),
                GetColumns('Education'),
                GetColumns('Economy'),
                GetColumns('Happiness')
            ) %>%
            arrange(
                Country
            )
    })   
    
    #-- Returns all data for selected region --#
    load_selected_data <- reactive({
        print('Loading selected data')
        
        load_data() %>%
            { if (values$region == 'All Countries' | values$region == '') filter(., TRUE) else filter(., Region == values$region) }
    })
    
    #-- map data --#
    load_map_data <- reactive({
        print('Loading map data')
        world_shapefiles <- read_sf(dsn = 'world-shape-files/ne_50m_admin_0_countries.shp')
        
        world_shapefiles[world_shapefiles$NAME == 'Antigua and Barb.', 'NAME'] <- 'Antigua and Barbuda'
        world_shapefiles[world_shapefiles$NAME == 'Central African Rep.', 'NAME'] <- 'Central African Republic'
        world_shapefiles[world_shapefiles$NAME == 'Dominican Rep.', 'NAME'] <- 'Dominican Republic'
        world_shapefiles[world_shapefiles$NAME == 'Eq. Guinea', 'NAME'] <- 'Equatorial Guinea'
        world_shapefiles[world_shapefiles$NAME == 'eSwatini', 'NAME'] <- 'Eswatini'
        world_shapefiles[world_shapefiles$NAME == 'Marshall Is.', 'NAME'] <- 'Marshall Islands'
        world_shapefiles[world_shapefiles$NAME == 'St. Kitts and Nevis', 'NAME'] <- 'Saint Kitts and Nevis'
        world_shapefiles[world_shapefiles$NAME == 'St. Vin. and Gren.', 'NAME'] <- 'Saint Vincent and the Grenadines'
        world_shapefiles[world_shapefiles$NAME == 'São Tomé and Principe', 'NAME'] <- 'Sao Tome and Principe'
        world_shapefiles[world_shapefiles$NAME == 'Solomon Is.', 'NAME'] <- 'Solomon Islands'
        world_shapefiles[world_shapefiles$NAME == 'S. Sudan', 'NAME'] <- 'South Sudan'
        world_shapefiles[world_shapefiles$NAME == 'United States of America', 'NAME'] <- 'United States'
        
        world_shapefiles %>%
            right_join(load_selected_data(), by = c('NAME' = 'Country'))
    })
    
    #-- region dropdown --#
    output$regions <- renderUI({
        items <- sort(unique(load_data()[['Region']]))
        
        selectInput(
            inputId = 'region',
            label = NULL,
            choices = c('Select Region' = '', 'All Countries' = 'All Countries', items),
            selected = 'All Countries'
        )
    })
    
    #-- variable dropdown --#
    output$variables <- renderUI({
        selectInput(
            inputId = 'variable',
            label = NULL,
            
            # use VariableNames on dropdown
            choices = c('Select Variable' = '', setNames(GetColumnVariableLabels(values$category), GetColumnVariableNames(values$category)))
            
            # use VariableLabels on dropdown
            #choices = c('Select Variable' = '', GetColumnVariableLabels(values$category))
        )
    })
    
    #-- When region is changed update to values dataframe --#
    observeEvent(
        input$region,
        ignoreNULL = TRUE,
        
        {
            if (input$region == '') {
                return
            }
            
            values$region <- input$region
        }    
    )
    
    #-- When variable is changed prints variable to console --#
    observeEvent(
        input$variable,
        ignoreNULL = TRUE,
        
        {
            if (input$variable == '') {
                return
            }
            
            print(paste0('Variable - ', input$variable))
        }
    )    
    
    #-- When a tab is changed update for variable dropdown --#
    observeEvent(
        input$tab,
        {
            if (input$tab %in% GetCategories()) {
                values$category <- input$tab
                updateSelectInput(session, 'variable', selected = GetColumnVariableLabels(input$tab)[1])
            }
        }
    )
    
    #-- Function for map --#
    PlotMap <- reactive({
        if (is.null(input$region)) {
            return (NULL)
        }
        
        # If tab is not the right category  for variable map won't load, no error displayed
        if (input$tab != GetCategory(input$variable)) {
            return (NULL)
        }
        
        print(paste0('Loading map / Tab = ', input$tab))
        print(paste0('Loading map / Var = ', input$variable))
        
        map_data <- load_map_data()
        variable <- input$variable
        
        # Set color gradient for mood
        colors <- switch(
            GetMood(variable),
            'Good' = c('#addd8e', 'green4'),
            'Neutral' = c('#fec44f', '#d95f0e'),
            'Bad' = c('#ffeda0', 'red4')
        )
        
        # Set color for NA
        qpal <- colorNumeric(colors, map_data[[variable]], na.color = '#bdbdbd')
        
        region <- input$region 
        
        # Set default zooms for each region. Since we are focusing only on Amrica and Asia map only ( USa, Canada,China)
        if (region == 'All Countries') {
            lng = 0
            lat = 50
            zoom = 3
        }
        else if (region == 'Africa') {
            lng = 20
            lat = 0
            zoom = 3
        }
        else if (region == 'Americas') {
            lng = -85
            lat = 12
            zoom = 2
        }
        else if (region == 'Asia') {
            lng = 85
            lat = 25
            zoom = 3
        }
        else if (region == 'Europe') {
            lng = 20
            lat = 57
            zoom = 3
        }
        else if (region == 'Oceania') {
            lng = 145
            lat = -21
            zoom = 3
        }
        
        # map settings
        map_data %>%
            leaflet() %>%
            addTiles() %>%
            setView(lng = lng, lat = lat, zoom = zoom) %>%
            addPolygons(
                stroke = TRUE,
                color = 'black',
                weight = 1,
                opacity = 1,
                smoothFactor = 1,
                fillOpacity = 0.5,
                fillColor = qpal(map_data[[variable]]),
                label = ~paste0(NAME, ' - ', map_data[[variable]])
            ) %>%
            addLegend(
                pal = qpal, 
                values = map_data[[variable]], 
                opacity = 0.8,
                title = variable,
                position = 'bottomleft',
                na.label = 'NA'
            )
    })    
    
    #-- Render for Data tab --#
    output$data <- renderDT(
        datatable(load_selected_data(),
        rownames = FALSE,
        class = 'cell-border compact stripe',
        extensions = c('Responsive'),
        escape = FALSE
        ) %>%
          
          # set background color for all rows to grey
          formatStyle("Country",
                      target = 'row',
                      backgroundColor = "grey")
    )
    
    #-- Render for Codebook tab --#
    output$notes <- renderTable(
        {
            columns %>%
            select(-Mood) %>%
            # renaming for display
            rename(Name = VariableLabel, Description = VariableName)
        },
        
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    #-- Function for correlation --#
    PlotCorrelation <- reactive({
        
        # If no variable selected
        if (is.null(input$variable)) {
            return (NULL)
        }
        
        # If tab is not the right category  for variable graph won't load, no error displayed
        if (input$tab != GetCategory(input$variable)) {
            return (NULL)
        }
        
        print(paste0('Loading plot / Tab = ', input$tab))
        print(paste0('Loading plot / Var = ', input$variable))
        
        data = load_selected_data()
        
        Region <- input$region
        VariableName <- GetVariableName(input$variable)
        #this is the correlation graph which will show by comparing among 3 countries-
        CorrTest <- cor.test(data[[input$variable]], (data$Confirmed))
        Plabel <- ifelse(CorrTest$p.value < 0.001, 'p < 0.001', paste0('p = ', round(CorrTest$p.value, 3)))
        x_max <- max(data[[input$variable]], na.rm = TRUE)
        x_min <- min(data[[input$variable]], na.rm = TRUE)
        y_max <- max((data$Confirmed), na.rm = TRUE)
        y_min <- min((data$Confirmed), na.rm = TRUE)
        x_label <- x_min#(x_max - x_min) / 5 + x_min
        y_label <- y_max#y_max - (y_max - y_min) / 10

        viz <- ggplot(data = data, aes(x = .data[[input$variable]], y = (Confirmed))) +
            geom_point(aes(text = paste0('Country: ', Country), color = Country),size=5)+
            geom_smooth(method = 'lm', formula = y ~ x,linetype="dashed") +
            annotate(
                'text', 
                label = paste0('R = ', round(CorrTest$estimate, 3), ', ', Plabel), 
                x = x_label,
                y = y_label,
                color = 'steelblue',
                size = 3
            ) +
            labs(
                # title = paste0(Region, ' - ', VariableName, ' and Confirmed Cases'),
                x = VariableName,
                y = 'log(Confirmed Cases)'
            ) +
            ggplot_defaults
        
        ggplotly(viz) %>%
            layout(
                title = paste0(Region, ' - ', VariableName, ' and Confirmed Cases'),
                margin = 100
            )
    })    
    
    
    #-- Renders for map --#
    output$map_cases <- renderLeaflet({
        PlotMap()
    })
      
    output$map_response <- renderLeaflet({
      PlotMap()
    })
    
    output$map_population <- renderLeaflet({
        PlotMap()
    })
    
    output$map_health <- renderLeaflet({
        PlotMap()
    })
    
    output$map_education <- renderLeaflet({
        PlotMap()
    })
    
    output$map_economy <- renderLeaflet({
        PlotMap()
    })
    
    output$map_happiness <- renderLeaflet({
        PlotMap()
    })
    
    
    #-- Renders for correlation --#
    output$correlation_response <- renderPlotly({
      PlotCorrelation()
    })
    
    output$correlation_population <- renderPlotly({
        PlotCorrelation()
    })
    
    output$correlation_health <- renderPlotly({
        PlotCorrelation()
    })
    
    output$correlation_education <- renderPlotly({
        PlotCorrelation()
    })
    
    output$correlation_economy <- renderPlotly({
        PlotCorrelation()
    })
    
    output$correlation_happiness <- renderPlotly({
        PlotCorrelation()
    })
    
}

