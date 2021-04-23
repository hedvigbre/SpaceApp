# SpaceApp
Code to an interactive app created in R Shiny showing space launches from 1957 to 2020. 
Link to my app: https://hedvigbrekke.shinyapps.io/SpaceApp/  
Data Source: https://www.kaggle.com/agirlcoding/all-space-missions-from-1957 

In the app, the data is split into two dataframes, one with and one without NAs. Most of the analysis is made off of the data with no NA, but in some cases they are kept (the other dataset is used) as the actual total amount of missions is interesting in some calculations. 

With a huge interest and fascination for space, I was very happy when I found a dataset containing different space launches over time. The dataset contains information about different space launches from 1957 to 2020 with data from different sites, companies, countries and outcome of mission. The app that I created aims to take a deep dive into the alleged ‘Space Race’. Most prominent and known in the context of the Russia/USA competition. The app tries to display and analyse this data with the use of valueboxes, a sunburst chart and some graphs. 

The app has a sidebar and is divided into 4 tabs; the frontpage, a page with charts, a map and a tab with the datasource. All the elements of the app are interactive except for the map. I filled in the latitude and longitude values manually based on space stations which makes the output very static. Ideally it would have been interesting to have the exact information and coordinates of each launch.
