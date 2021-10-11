library(tidyverse)
library(scales)
library(ggspatial)
library(lubridate)
library(sf)
library(viridis)
library(animation)
library(tidytext)
library(proto)
library(gsubfn)
library(RSQLite)
library(sqldf)
library(shiny)
library(RColorBrewer)
library(wordcloud)
library(shinythemes)
library(tidytext)
library(shinyWidgets)
library(quanteda)
library(tidyr)
library(tibble)
library(maps)
library(tmap)
library(leaflet)
library(tidylo)
library(urbnmapr)
library(ggmap)

#ladoing everything from otehr script

source(file = 'map and other functions.R')


#### app startrs here ####

ui <- fluidPage(theme = shinytheme("cyborg"),
                titlePanel("Beer Awards analysis"),
                navbarPage(" ",
                           tabPanel(
                             "Beer Awards general analysis",
                             sidebarLayout(
                               
                               sidebarPanel(
                                 #h2('General settings'),
                                 selectInput('state_select', 'Choose a state or all states:', state_list),
                                 
                                 
                                 
                                 checkboxGroupInput("medal_group",choices = medal_list,selected = c("Gold","Silver",'Bronze'),
                                                    label = 'Choose medal' ),
                                 
                                 sliderInput("dateRange", "Award Year",
                                             min = 1980, max = 2020,
                                             value = c(1980,2020)),
                                 pickerInput(
                                   inputId = "catpicker", 
                                   label = "Select beer category", 
                                   choices = cat_list, 
                                   options = list(
                                     `actions-box` = TRUE, 
                                     size = 10,
                                     `selected-text-format` = "count > 3"
                                   ),selected = cat_list, 
                                   multiple = TRUE
                                 ),
                                 hr(),
                                 #h3('Barplot Number of top breweries'),
                                 sliderInput("top_n", "Select how many top breweries you like to see",
                                             min = 1, max = 10,
                                             value = 5,step = 1),
                                 
                                 hr(),
                                 h5('Beer name text analysis settings'),
                                 
                                 selectInput('n_gram_id', 'Choose how many letters in beer name:', token_list),
                                 
                                 
                                 checkboxInput("stopwords_id", 'Stop words', value = TRUE),
                                 
                                 sliderInput("mincount", "Minimum words for beer names:",
                                             min = 5, max = 50,
                                             value = 15),
                                 actionButton("do", "Submit")
                               ),
                               mainPanel(
                                 tabsetPanel(
                                   tabPanel("Top Breweries",
                                            plotOutput("plot1",height = "800px",width = "800px")),
                                   tabPanel("Top Cities",
                                            plotOutput("plot2",height = "800px",width = "800px")),
                                   tabPanel("Beer Types",
                                            plotOutput("plot3",height = "800px",width = "1400px")),
                                   tabPanel("Medals won by selected categories",
                                            plotOutput("plot4",height = "800px",width = "800px")),
                                   tabPanel("Total number of medals in state by category",
                                            plotOutput("plot5",height = "800px",width = "800px")),
                                   tabPanel("Beer name analysis",
                                            plotOutput("plot6",height = "600px",width = "800px"))
                                   
                                 )))),
                           tabPanel(
                             "Interactive map",
                             sidebarLayout(
                               sidebarPanel(
                                 h2('Interactive map options'),  
                                 h3("Most Popular Beers"),
                                 selectInput("Selection","Beers List",top),
                                 actionButton("RunButton","Run"),
                                 hr(),
                                 h3("Total Number of Medals"),
                                 selectInput("Selection2","Total Medals",medal),
                                 actionButton("Run2","Run")
                               ),
                               mainPanel(#plotOutput("plot1"))))
                                 tabsetPanel(
                                   tabPanel("Interactive map: Top Beers",leafletOutput("map","100%",1000)),
                                   tabPanel("Interactive map: Medal Count",leafletOutput("map2","100%",1000))
                                   
                                 ))))
                ))
                
                  


server <- function(input, output) {
  
  df_ready <- eventReactive(input$do,{withProgress({
    setProgress(message = "Processing corpus...")
    df_filter(data_df = beer_awards,state_df =input$state_select,medal_df = input$medal_group,
              date_df = date_read(),cat_select = cat_ready())
  })})
  
  freq<-eventReactive(input$RunButton,withProgress({
    setProgress(message = "Processing Corpus...")
    getdata(input$Selection) # ... = replace with the two inputs from Task 2
  }))
  
  cat_ready <- eventReactive(input$do,{withProgress({
    setProgress(message = "Processing corpus...")
    input$catpicker
  })})
  
  date_read <- eventReactive(input$do,{withProgress({
    setProgress(message = "Processing corpus...")
    input$dateRange
  })})
  
  state_ready <- eventReactive(input$do,{withProgress({
    setProgress(message = "Processing corpus...")
    input$state_select
  })})
  
  top_n_click <- eventReactive(input$do,{withProgress({
    setProgress(message = "Processing corpus...")
    input$top_n
  })})
  g5_read <- eventReactive(input$do,{withProgress({
    setProgress(message = "Processing corpus...")
    g5_func(data_df = df_ready(),beer_cat = cat_ready())
  })})
  
  g7_read <- eventReactive(input$do,{withProgress({
    setProgress(message = "Processing corpus...")
    map_fun_g7(a = df_ready())
  })})
  
  
  word_dfm <- eventReactive(input$do,{withProgress({
    setProgress(message = "Processing corpus...")
    get_corpus(data_df = df_ready(),n_gram = input$n_gram_id,stop_click = input$stopwords_id)
  })})
  
  count_slider <- eventReactive(input$do,{withProgress({
    setProgress(message = "Processing corpus...")
    input$mincount
  })})
  
  output$plot1 <- renderPlot({
    d <- df_ready()
    g1(n_df = top_n_click(),data_df = d,title_name = state_ready(),year_d = date_read())})
  
  output$plot2 <- renderPlot({
    d <- df_ready()
    g2(n_df = top_n_click(),data_df = d,title_name = state_ready(),year_d = date_read())})
  
  output$plot3 <- renderPlot({
    d <- df_ready()
    g3(n_df = top_n_click(),data_df = d,title_name = state_ready(),year_d = date_read())})
  
  output$plot4 <- renderPlot({
    d <- df_ready()
    g4(data_df = d,title_name = state_ready(),year_d = date_read())})
  
  
  output$plot5 <- renderPlot({
    d <- df_ready()
    g8(data_df = d,title_name = state_ready(),year_d = date_read())})
  
  output$plot6 <- renderPlot({
    d <- word_dfm()
    get_wordcloud(data_dfm = d,min_word = count_slider())
  })
  

  #output$plot7 <- renderPlot({
  #  if(state_ready()=='USA'){g6(data_df = g7_read(),title_name = state_ready(),year_d = date_read())}
  #  else{g6_one(data_df =df_ready(),title_name = state_ready(),year_d =date_read())}
  #  })
  
  output$map<-renderLeaflet({
    f<-freq()
    
    bins<-c(0,10,20,50,100,500,1000)    #Create a color palette with Bins
    pal<-colorBin("BuPu" ,domain=f,bins =bins)
    
    
    labels <- paste(                   #Create a label with state name and their number one famous beer
      f@data$NAME,"<br/>", 
      f@data$beer 
    ) %>%
      lapply(htmltools::HTML)
    
    
    
    m<-leaflet(f)%>%
      setView(-96, 37.8, 4)%>%
      addProviderTiles(providers$Stamen.Toner)%>%
      addPolygons( data=f,
                   weight=1,
                   color="#660000",
                   smoothFactor=1,
                   dashArray=3,
                   fillOpacity=0.7)
    
    
    
    m <- m %>% addPolygons(
      fillColor = ~pal(n),
      weight = 2,
      opacity = 1,
      color = "black",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#660000",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
    
    m %>% addLegend(pal = pal, values = ~n, opacity = 0.7, title = "Total medal and their famous beer",
                    position = "bottomright")
    m
  })
  
  
  
  
  freq2<-eventReactive(input$Run2,withProgress({
    setProgress(message = "Processing Corpus...")
    getdata(1) # ... = replace with the two inputs from Task 2
  }))
  output$map2<-renderLeaflet({
    f<-freq2()
    value<-input$Selection2
    bins<-c(0,10,20,50,100,500,1000)    #Create a color palette with Bins
    pal<-colorBin("BuPu" ,domain=f,bins =bins)
    
    
    if(input$Selection2=="GOLD") {labels <- paste(                   #Create a label with state name and their number one famous beer
      f@data$NAME,"<br/>", 
      f@data$GOLD
    ) %>% lapply(htmltools::HTML)}
    else if (input$Selection2=="SILVER") 
    {labels <- paste(                   #Create a label with state name and their number one famous beer
      f@data$NAME,"<br/>", 
      f@data$SILVER
    ) %>%lapply(htmltools::HTML)}
    else{labels <- paste(                   #Create a label with state name and their number one famous beer
      f@data$NAME,"<br/>", 
      f@data$BRONZE
    ) %>%
      lapply(htmltools::HTML)}
    
    
    
    m<-leaflet(f)%>%
      setView(-96, 37.8, 4)%>%
      addProviderTiles(providers$Stamen.Toner)%>%
      addPolygons( data=f,
                   weight=1,
                   color="#660000",
                   smoothFactor=1,
                   dashArray=3,
                   fillOpacity=0.7)
    
    
    
    m <- m %>% addPolygons(
      fillColor = ~pal(n),
      weight = 2,
      opacity = 1,
      color = "black",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#660000",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
    
    m %>% addLegend(pal = pal, values = ~n, opacity = 0.7, title = "Total medal and their famous beer",
                    position = "bottomright")
    m
  })
}
# Run the application 
shinyApp(ui = ui, server = server)