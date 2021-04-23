#loading the libraries
#library(shiny)
library(shinydashboard)
library(plotly)
library(fresh)
library(fontawesome)
library(htmlwidgets)
library(shinyWidgets)
library(data.table)
library(tidyverse) 
library(dplyr)
library(tidyr)
library(leaflet)
library(DT)

#Creating a custom theme
mytheme <- create_theme(
    adminlte_color(
        light_blue = "#aaaaaa"
    ),
    adminlte_sidebar(
        width = "400px",
        dark_bg = "#c9c9c9",
        dark_hover_bg = "#D3D3D3",
        dark_color = "#525252"
    ),
    adminlte_global(
        content_bg = "#FFF",
        box_bg = "#c9c9c9", 
        info_box_bg = "#c9c9c9"
    )
)
########## loading the 3 datasets ##########
#data where the NAs are kept, as this includs all missions
load("df_NA.RData")

#data where the NAs are removed (the dataset had a lot of them)
load("df_NONA.RData")

#data reconstructed for the sunburst chart
load("sunburn.RData")


#defning the UI   
ui <- dashboardPage(
    header = dashboardHeader(title = strong('"TAKING YOU TO SPACE"', icon("user-astronaut")),
                             tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/hedvigbrekke/" ,icon("linkedin"), "LinkedIn", target="_blank")),
                             tags$li(class="dropdown",tags$a(href="https://github.com/hedvigbre" ,icon("github"), "Github", target="_blank")),
                             tags$li(class="dropdown",tags$a(href="https://www.kaggle.com/hedvig", icon("kaggle"), "Kaggle", target="_blank"))),
    #creating the sidebar
    sidebar = dashboardSidebar(
        div(style= 'position:absolute; left:32px; top:67px;',h4(strong('A "DEEP DIVE" INTO THE SPACE RACE'))),
        div(style= 'position:absolute; left:32px; right:40px; top:100px;',h5(em('"Hi! This app aims to take a deep dive into the global history of the Space Race that originally started in 1957 when the Soviet Union launched Sputnik 2. By using the filters in the sidebar, and the different tabs, you will be able to examin the countries and companies participating, the cost of missions, the success rates and so on. The code and data are posted on the listed sources."- Hedvig'))),
        div(style= 'position:absolute; left:32px; top:233px;',h5(strong('Use the filters below to sort the data.'))),    
        div(style= 'position:absolute; left:32px; top:700px',h5(strong('Reload your browser to reset the filters'), icon('internet-explorer'))),
        br(), 
        
        #fluidpage with columns for the app
        fluidPage(
            column(1, style= 'position:absolute; left:0px; top:245px;width:400px; height:100px',
                   checkboxGroupInput(inputId = "mii", h4(strong("Status Mission")),               
                                      choices = list("Success" = "S", 
                                                     "Failure" = "F", 
                                                     "Partial Failure" = "PF"),
                                      selected = c("S", "F", "PF"))),
            column(2, style= 'position:absolute; left:160px; top:245px; width:400px; height:100px',
                   checkboxGroupInput(inputId = "roo", h4(strong("Status Rocket")),               
                                      choices = list("Active" = "Active", 
                                                     "Retired" = "Retired"),
                                      selected = c("Active", "Retired"))),
            br(), 
            br(), 
            br(), 
            br(), 
            br(), 
            br(), 
            br(), 
            br(), 
            br(), 
            br(), 
            br(), 
            br(), 
            br(), 
            br(), 
            br(),
            br(),
            
            #creating an input to select countries (the dataset without NAs is used as more options are not needed)
            selectizeInput(
                inputId = 'foo', label = 'Selected Countries', choices = unique(df_NA$country),
                selected = c('Russia', 'USA', 'China', 'Japan', 'Kazakhstan', 'France', 'New Zealand', 'Israel', 'Iran'), multiple = TRUE,
                options = list(create = TRUE)
            ),
            setSliderColor(c('grey', 'grey'), c(1, 2)),
            sliderInput("slid1", "The Year of  Mission:",
                        min = 1957, max = 2020,
                        #add year as value/df_year
                        value = c(1957,2020), step = 1,
                        pre = "Year = ", sep = ""),
            sliderInput("slid2", "The Cost of a Mission:",
                        min = 5.3, max = 5000,
                        #add cost as value/df_cost
                        value = c(5.3,5000), 
                        step = 1,
                        pre = "Millions$ = ", sep = ""),
        )
    ),
    body = dashboardBody(
        use_theme(mytheme), 
        
        fluidPage(
            #changing the color and width of the tabsetpanel
            tags$style(HTML("
        .tabbable > .nav > li > a {background-color: lightgrey;  color:white; width: 230PX;}
        ")),
        #creating tabs
        tabsetPanel(type = "tabs",
                    tabPanel(icon("home"),
                             br(),
                             column(9, style= 'position:absolute; right:0px; left:730px; top:125px',
                                    infoBoxOutput("info_1")),
                             column(9, style= 'position:absolute; right:200px; left:400px; top:125px',
                                    infoBoxOutput("info_2")),
                             column(9, style= 'position:absolute; right:150px; left:1070px; top:125px',
                                    valueBoxOutput("vbox_1")),
                             #trying to add a copyright footer
                             tags$footer(strong("Copyright of Hedvig Brekke", icon("copyright")), style = "position:absolute;left:832px;top:720px;bottom:0;width:100%;
                              height:30px;  
                              color: lightgrey;
                              padding: 10px;
                              background-color: white;
                              z-index: 1000;"),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             plotlyOutput(outputId = "sunburst",  width = "80%"),
                             tags$text(em("Hover over the sunburst chart or click on the different elements to interact with it."), style = 'position:absolute;right:41px;top:250px;bottom:0px;
                              color: #808080;
                              height:40px; 
                              width:500px;
                              padding: 10px;
                              background-color: white'
                             ),
                             tags$text(icon("flag"),em("The Overall Success Rate of Russia is Calculated to 93%"), style = "position:absolute;right:41px;top:325px;'bottom:0;width:100%;
                              height:40px;  
                              color: black;
                              padding: 10px;
                              background-color: #FFF2A7;
                              z-index: 1000;"),
                             tags$text(icon("flag"),em("The Overall Success Rate of The USA is Calculated to 88%"), style = "position:absolute;right:33px;top:500px;'bottom:0;width:100%;
                              height:40px;  
                              color: black;
                              padding: 10px;
                              background-color: #31DCBD;
                              z-index: 1000;"),
                             column(9, style= 'position:absolute; right:0px; left:1020px; top:545px',
                                    valueBoxOutput("vbox_usa")),
                             column(9, style= 'position:absolute; right:0px; left:1020px; top:370px',
                                    valueBoxOutput("vbox_rus")),
                             
                    ),
                    tabPanel(icon("chart-bar"),
                             tags$footer(strong("Copyright of Hedvig Brekke", icon("copyright")), style = "position:absolute;left:825px;top:720px;'bottom:0;width:100%;
                              height:30px;  
                              color: lightgrey;
                              padding: 10px;
                              background-color: white;
                              z-index: 1000;"),
                             mainPanel(
                                 column(9, style= 'position:absolute; left:0px; top:20px', plotOutput('bar1', width = 460, height = 550)),
                                 column(9, style= 'position:absolute; left:490px; top:20px', plotOutput('bar2', width = 440, height = 300)),
                                 column(9, style= 'position:absolute; left:500px; top:300px', plotOutput('bar3', width = 440, height = 300)))
                    ),
                    tabPanel(icon("map"),
                             br(),
                             div(style= 'position:sticky; position: -webkit-sticky; left:430px; top:110px;', h4(strong('"Map of Launch Sites"', icon("rocket")))), 
                             div(style= 'position:sticky; position: -webkit-sticky; left:430px; top:135px;', h6(em('Click on the different launch sites for more information.'))),
                             leafletOutput("mymap", width = "100%", height = "800"),
                    ),
                    tabPanel(icon("database"), 
                             br(),
                             br(),
                             column(12,
                                    dataTableOutput('table')))),
        ),
    ),
)
#end tagList
#the server
server <- function(input, output, session) {
    
    ########## loading the 3 datasets ##########
    #data where the NAs are kept, as this includs all missions
    load("df_NA.RData")
    
    #data where the NAs are removed (the dataset had a lot of them)
    load("df_NONA.RData")
    
    #data reconstructed for the sunburst chart
    load("sunburn.RData")
    
    #creating the dynamic data for df_NONA
    data <- reactive({                
        df_NONA %>%                  
            filter(year >= input$slid1[1], 
                   year <= input$slid1[2], 
                   cost_mill_usd >= input$slid2[1], 
                   cost_mill_usd <= input$slid2[2], 
                   country %in% input$foo,
                   status_mission %in% input$mii,
                   status_rocket %in% input$roo
            )})
    #creating the dynamic data for df_NONA
    data_2 <- reactive({                
        df_NA %>%                  
            filter(year >= input$slid1[1], 
                   year <= input$slid1[2], 
                   cost_mill_usd >= input$slid2[1], 
                   cost_mill_usd <= input$slid2[2], 
                   country %in% input$foo,
                   status_mission %in% input$mii,
                   status_rocket %in% input$roo
            )})
    
    #creating the dynamic data for the success rate 
    data_1 <- reactive({                
        df_NA %>%                  
            filter(year >= input$slid1[1], 
                   year <= input$slid1[2]
            )})
    
    output$info_1 <- renderInfoBox({
        df_NONA <- data_2()
        infoBox(value = sum(df_NONA$status_mission=="S"),
                strong('Succeeded Missions'), 
                icon = shiny::icon("rocket"), 
                color = "light-blue", width = 4,
                href = NULL, fill = FALSE)
    })
    
    output$info_2 <- renderInfoBox({
        df_NONA <- data_2()
        infoBox(value = sum(df_NONA$status_mission=="F"), 
                strong('Failed Missions'),
                icon = shiny::icon("rocket"), 
                color = "light-blue", width = 4,
                href = NULL, fill = FALSE)
        
    })
    
    output$vbox_1 <- renderValueBox({
        df_NONA <- data_2()
        valueBox(value = sum(df_NONA$cost_mill_usd),
                 subtitle = strong('Sum of Launches Displayed in Million USD$'),
                 icon = shiny::icon("money"), 
                 color = "light-blue", 
                 width = 4)
        
    })
    
    output$vbox_usa <- renderValueBox({
        #calculating mission success rate for USA 
        a <- data_1() %>%
            group_by(country) %>%
            filter(country == 'USA') %>%
            summarise(n_distinct(index)) %>%
            select(2)
        
        a <- data_1() %>%
            group_by(country) %>%
            group_by(status_mission) %>%
            filter(country == 'USA') %>%
            filter(status_mission == 'S') %>%
            summarise(n_distinct(index)/a) %>%
            select(2)*100
        
        valueBox(
            "Success Rate USA (%)",
            color = "black",
            value = round(a, 2),
            icon = icon("flag")
        )
    })
    
    output$vbox_rus <- renderValueBox({
        #calculating mission success rate for Russia
        b <- data_1() %>%
            group_by(country) %>%
            filter(country == 'Russia') %>%
            summarise(n_distinct(index)) %>%
            select(2)
        
        b <- data_1() %>%
            group_by(country) %>%
            group_by(status_mission) %>%
            filter(country == 'Russia') %>%
            filter(status_mission == 'S') %>%
            summarise(n_distinct(index)/b) %>%
            select(2)*100
        
        valueBox(
            "Success Rate Russia (%)",
            color = "black",
            value = round(b, 2),
            icon = icon("flag")
        )
    })
    
    output$bar1 <- renderPlot({ 
        df_NA <- data_2()
        plot(df_NA %>%
                 group_by(country) %>%
                 summarize(cntcountry = n_distinct(index)) %>%
                 ggplot(aes(x = cntcountry, y = reorder(country, cntcountry))) +
                 geom_col() +
                 labs(title = "Number of Starts Per Country",
                      subtitle = "With The US accounting for the most",
                      x = "Mission Count",
                      y = "Countries"))
    })
    
    output$bar2 <- renderPlot({ 
        df_NONA <- data_2()
        plot(df_NONA %>%
                 group_by(country) %>%
                 summarize(countspending = sum(cost_mill_usd))%>%
                 ggplot(aes(x = reorder(country, desc(countspending)), y = countspending)) +
                 geom_col() +
                 scale_x_discrete(guide = guide_axis(angle = -50)) +
                 labs(title = "Total Spending Per Country", 
                      subtitle = "With The US accounting for the highest spending", 
                      x = "Country ", y = "Cost"))
    })
    
    output$bar3 <- renderPlot({ 
        df_NA <- data_2()
        plot(df_NA %>%   
                 group_by(year) %>%
                 summarize(countyear = n_distinct(index))%>%
                 ggplot(aes(x = year, y = countyear)) +
                 geom_line() +
                 labs(title = "Number of Missions Per Year", 
                      subtitle = "The data displays from 1957 to 2020", 
                      x = "Year ", 
                      y = "Count Missions"))
    })
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(
                "CartoDB.Positron",
                group = "CartoDB.Positron"
            ) %>%
            addMarkers(data = df_NA, 
                       label = ~site,
                       popup = ~df_NA$popup_info,
                       lng = ~long, 
                       lat = ~lat,
                       icon = list(
                           iconUrl = 'http://simpleicon.com/wp-content/uploads/rocket.png',
                           iconSize = c(50, 50)),
            )
    })  
    output$sunburst <- renderPlotly({
        p <- plot_ly(labels = sunburn$labels,
                     parents = sunburn$parents,
                     values = sunburn$values,
                     ids = sunburn$ids,
                     type = 'sunburst',
                     branchvalues = "total",
                     name = "  ",
                     hovertemplate = paste('<b>%{label}</b><br>','Count=%{value}</b><br>', 
                                           'ID=%{id}</b><br>', 'Parent=%{parent}</b><br>'),
        )
        fig <- layout(p,
                      grid = list(columns =0, rows = 2),
                      margin = list(l = 0, r = 0, b = 0, t = 0),
                      colorway = c("#FFF2A7", "#4CE1C3", "#FFC0CB", "#A7B4FF", "#FFDECB", 
                                   "#FFF2A7", "#CBFFF8", "#FFC0CB", "#A7B4FF", "#4CE1C3", 
                                   "#FFF2A7", "#CBFFF8", "#FFC0CB", "#A7B4FF", "#4CE1C3", 
                                   "#FFF2A7", "#CBFFF8", "#FFC0CB", "#A7B4FF", "#4CE1C3",
                                   "#FFF2A7", "#CBFFF8", "#FFC0CB", "#A7B4FF", "#4CE1C3"),
                      height = 500, width = 500)
    })
    
    #displaying the dataset in the last tab
    output$table <- DT::renderDataTable({data() %>% select(1:8)})
}

shinyApp(ui, server)

