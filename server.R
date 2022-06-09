#Server side. 
server <- function(input, output, session) {
  
  #Popup when the app is first loaded
  showModal(modalDialog(
    title =  htmltools::HTML("<h2>Welcome to the River Watch Data Tool!</h2>"), #html used below to format text. paragraphs wrapped in tags$div()
    htmltools::HTML("<a href='https://coloradoriverwatch.org/'> River Watch</a> is a statewide volunteer water quality-monitoring program operated in partnership between
                    <a href='https://www.river.science/'> River Science</a> and <a href='https://cpw.state.co.us/'> Colorado Parks and Wildlife (CPW)</a>.
                    Our mission is to work with voluntary stewards to monitor water quality and utilize this data to educate communities and help inform decision makers 
                    about the condition of Colorado’s waters.<br><br>
                    This tool is designed to interactively visualize River Watch data across Colorado. Use the main window to explore the map and generate graphs. Use the left panel 
                    to navigate tabs and access the user guide. <br><br>
                    Some aquatic life standards are presented in this tool. Standards are not included for all parameters and are provided for reference only. To obtain the most 
                    current standards, visit the the Water Quality Control Commission’s <a href='https://cdphe.colorado.gov/water-quality-control-commission-regulations'> website</a>. 
                    <b>River Watch does not assign attainment or impairment of aquatic life standards</b>. To learn more about River Watch methods and data collection,
                    <a href='https://coloradoriverwatch.org/our-data/'> go here</a>."),
    footer = modalButton(label = "Dismiss"),
    easyClose = TRUE,
    size = "m"
  ))

####Station Tab----
  slide <-reactive({ #Filter data for date slider based on what graph is selected. Time series and boxplot are the same. Multi station includes all stations selected
    switch(input$type, 
           "Time Series"  = dplyr::filter(data, StationNumber == input$station), 
           "Time Series (Multiple Stations)" =dplyr::filter(data, StationNumber %in% c(input$station, input$station2)), 
           "Box Plot"=dplyr::filter(data, StationNumber == input$station))
  })
  #Slider Dates
  output$dateslider <- renderUI({
    req(input$station!="Select a station here or on the map") #does not display anything if no station selected
    date_min = min(slide()$SampleDate)
    date_max = max(slide()$SampleDate)
    sliderInput("dateslider", "Date Range:", min=date_min, max=date_max, value=c(date_min,date_max))
  })
  #Show standards? - appears on ui side. req(specifying when checkbox should appear) and checkboxInput(defining the checkbox)
  output$showstd<- renderUI({
    req(input$type=="Time Series"& input$param %in% c("Ph","Al.T","As.D","Cd.D","Cu.D","Fe.T","Pb.D","Se.D","Zn.D"))
    checkboxInput("showstd", label = "Calculate Aquatic Life Standards", value = FALSE)
  })
  #multiple stations select
  output$station2 <- renderUI({
    req(input$type == "Time Series (Multiple Stations)")
    selectInput("station2", label = "Select More Stations", choices = stations, selected = NULL, multiple = T)
  })
  
  #filtering data - data used for graphs and table
  filt <- reactive({ #select data based on user input of stations
    switch(input$type, 
    "Time Series"  = dplyr::filter(data, StationNumber == input$station) %>% 
      dplyr::filter(SampleDate >= min(input$dateslider), SampleDate <= max(input$dateslider)),
    "Time Series (Multiple Stations)" =dplyr::filter(data, StationNumber %in% c(input$station, input$station2)) %>% 
      dplyr::filter(SampleDate >= min(input$dateslider), SampleDate <= max(input$dateslider)),
    "Box Plot"=dplyr::filter(data, StationNumber == input$station) %>% 
      dplyr::filter(SampleDate >= min(input$dateslider), SampleDate <= max(input$dateslider)))
    
  })
 
    

  #plots
  stdplot<- reactive({ 
    if(input$type=="Time Series"){ #Using standard function in global file to calculate standards for graphs if time series is selected
      calcstand<-filt()
      calcstand<-standards(calcstand)
      `%notin%` <- Negate(`%in%`)} #Writing a function - opposite of %in%
    
    p<-ggplot(data = calcstand) + #First part of the graph - define the rest based on user inputs
      scale_y_continuous(labels = scales::comma)+
      labs(x = "Date", y = names(yoptions[which(yoptions == input$param)])) 
    #If user selects a parameter without a coded standard, use the below code for the graph
    if(input$param %notin% c("Ph","Al.T","As.D","Cd.D","Cu.D","Fe.T","Pb.D","Se.D","Zn.D")){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param))+
                                    geom_line(linetype = 2,aes_string(x=calcstand$SampleDate, y=input$param)))}
    #User has selected a parameter with a coded standard. if(selected param & show standard checkbox = True){Then use this code for graph. Ie show standard graph}
    else if(input$param=="Ph"&input$showstd==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param))+
                                          geom_line(linetype = 2,aes_string(x=calcstand$SampleDate, y=input$param))+
                                          geom_line(linetype=2,aes(x=SampleDate, y=PHMIN), color="orange3")+geom_line(linetype=2,aes(x=SampleDate, y=PHMAX), color="orange3"))}
      
      else if(input$param=="Al.T"&input$showstd==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Total Aluminum")))+
                                              geom_line(linetype = 2,aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Total Aluminum")))+
                                              geom_point(aes(x=SampleDate, y=AL.CH,color="Calc. Chronic Standard"))+geom_point(aes(x=SampleDate, y=AL.AC,color="Calc. Acute Standard"))+
                                         geom_line(aes(x=SampleDate, y=AL.CH,color="Calc. Chronic Standard"),linetype=2)+geom_line(aes(x=SampleDate, y=AL.AC,color="Calc. Acute Standard"),linetype=2)+
                                         labs(title = "Observed Total Aluminium and Calculated Standards")+
                                         scale_color_manual(name="",breaks = c("Calc. Acute Standard", "Calc. Chronic Standard", "Obs. Total Aluminum"),
                                                            values = c("Calc. Acute Standard"="red", "Calc. Chronic Standard"="orange3", "Obs. Total Aluminum"="black"))) %>% 
                                                            layout(legend = list(orientation = "h",  
                                                            xanchor = "center",  
                                                            x = 0.5, y=-0.25, bordercolor = "#000000",
                                                            borderwidth = 1))}
      else if(input$param=="As.D"&input$showstd==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Dissolved Arsenic")))+
                                              geom_line(linetype = 2,aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Dissolved Arsenic")))+
                                              geom_line(aes(x=SampleDate, y=AS.CH,color="Chronic Standard"),linetype=2)+geom_line(aes(x=SampleDate, y=AS.AC,color="Acute Standard"),linetype=2)+
                                              labs(title = "Observed Dissolved Arsenic and Standards")+
                                              scale_color_manual(name="",breaks = c("Acute Standard", "Chronic Standard", "Obs. Dissolved Arsenic"),
                                                                 values = c("Acute Standard"="red", "Chronic Standard"="orange3", "Obs. Dissolved Arsenic"="black"))) %>% 
          layout(legend = list(orientation = "h",  
                               xanchor = "center",  
                               x = 0.5, y=-0.25, bordercolor = "#000000",
                               borderwidth = 1))}
      else if(input$param=="Cd.D"&input$showstd==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Dissolved Cadmium")))+
                                              geom_line(linetype = 2,aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Dissolved Cadmium")))+
                                              geom_point(aes(x=SampleDate, y=CD.CH,color="Calc. Chronic Standard"))+
                                              geom_line(aes(x=SampleDate, y=CD.CH,color="Calc. Chronic Standard"),linetype=2)+
                                              labs(title = "Observed Dissolved Cadmium and Calculated Standards")+
                                              scale_color_manual(name="",breaks = c("Calc. Chronic Standard", "Obs. Dissolved Cadmium"),
                                                                 values = c("Calc. Chronic Standard"="orange3", "Obs. Dissolved Cadmium"="black"))) %>% 
          layout(legend = list(orientation = "h",  
                               xanchor = "center",  
                               x = 0.5, y=-0.25, bordercolor = "#000000",
                               borderwidth = 1))} 
      else if(input$param=="Cu.D"&input$showstd==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Dissolved Copper")))+
                                              geom_line(linetype = 2,aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Dissolved Copper")))+
                                              geom_point(aes(x=SampleDate, y=CU.CH,color="Calc. Chronic Standard"))+geom_point(aes(x=SampleDate, y=CU.AC,color="Calc. Acute Standard"))+
                                              geom_line(aes(x=SampleDate, y=CU.CH,color="Calc. Chronic Standard"),linetype=2)+geom_line(aes(x=SampleDate, y=CU.AC,color="Calc. Acute Standard"),linetype=2)+
                                              labs(title = "Observed Dissolved Copper and Calculated Standards")+
                                              scale_color_manual(name="",breaks = c("Calc. Acute Standard", "Calc. Chronic Standard", "Obs. Dissolved Copper"),
                                                                 values = c("Calc. Acute Standard"="red", "Calc. Chronic Standard"="orange3", "Obs. Dissolved Copper"="black"))) %>% 
          layout(legend = list(orientation = "h",  
                               xanchor = "center",  
                               x = 0.5, y=-0.25, bordercolor = "#000000",
                               borderwidth = 1))}
      else if(input$param=="Fe.T"&input$showstd==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Total Iron")))+geom_line(linetype = 2,aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Total Iron")))+ 
                                              geom_line(aes(x=SampleDate, y=FE.CH,color="Chronic Standard"),linetype=2)+
                                              labs(title = "Observed Total Iron and Chronic Standard")+
                                              scale_color_manual(name="",breaks = c("Chronic Standard", "Obs. Total Iron"),
                                                                 values = c("Chronic Standard"="orange3", "Obs. Total Iron"="black"))) %>% 
          layout(legend = list(orientation = "h",  
                               xanchor = "center",  
                               x = 0.5, y=-0.25, bordercolor = "#000000",
                               borderwidth = 1))}
      else if(input$param=="Pb.D"&input$showstd==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Dissolved Lead")))+
                                              geom_line(linetype = 2,aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Dissolved Lead")))+
                                              geom_point(aes(x=SampleDate, y=PB.CH,color="Calc. Chronic Standard"))+geom_point(aes(x=SampleDate, y=PB.AC,color="Calc. Acute Standard"))+
                                              geom_line(aes(x=SampleDate, y=PB.CH,color="Calc. Chronic Standard"),linetype=2)+geom_line(aes(x=SampleDate, y=PB.AC,color="Calc. Acute Standard"),linetype=2)+
                                              labs(title = "Observed Dissolved Lead and Calculated Standards")+
                                              scale_color_manual(name="",breaks = c("Calc. Acute Standard", "Calc. Chronic Standard", "Obs. Dissolved Lead"),
                                                                 values = c("Calc. Acute Standard"="red", "Calc. Chronic Standard"="orange3", "Obs. Dissolved Lead"="black"))) %>% 
          layout(legend = list(orientation = "h",  
                               xanchor = "center",  
                               x = 0.5, y=-0.25, bordercolor = "#000000",
                               borderwidth = 1))}
      else if(input$param=="Mn.D"&input$showstd==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Dissolved Manganese")))+
                                              geom_line(linetype = 2,aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Dissolved Manganese")))+
                                              geom_point(aes(x=SampleDate, y=MN.CH,color="Calc. Chronic Standard"))+geom_point(aes(x=SampleDate, y=MN.AC,color="Calc. Acute Standard"))+
                                              geom_line(aes(x=SampleDate, y=MN.CH,color="Calc. Chronic Standard"),linetype=2)+geom_line(aes(x=SampleDate, y=MN.AC,color="Calc. Acute Standard"),linetype=2)+
                                              labs(title = "Observed Dissolved Manganese and Calculated Standards")+
                                              scale_color_manual(name="",breaks = c("Calc. Acute Standard", "Calc. Chronic Standard", "Obs. Dissolved Manganese"),
                                                                 values = c("Calc. Acute Standard"="red", "Calc. Chronic Standard"="orange3", "Obs. Dissolved Manganese"="black"))) %>% 
          layout(legend = list(orientation = "h",  
                               xanchor = "center",  
                               x = 0.5, y=-0.25, bordercolor = "#000000",
                               borderwidth = 1))}
      else if(input$param=="Se.D"&input$showstd==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Dissolved Selenium")))+
                                              geom_line(linetype = 2,aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Dissolved Selenium")))+
                                              geom_line(aes(x=SampleDate, y=SE.CH,color="Chronic Standard"),linetype=2)+geom_line(aes(x=SampleDate, y=SE.AC,color="Acute Standard"),linetype=2)+
                                              labs(title = "Observed Dissolved Selenium and Standards")+
                                              scale_color_manual(name="",breaks = c("Acute Standard", "Chronic Standard", "Obs. Dissolved Selenium"),
                                                                 values = c("Acute Standard"="red", "Chronic Standard"="orange3", "Obs. Dissolved Selenium"="black"))) %>% 
          layout(legend = list(orientation = "h",  
                               xanchor = "center",  
                               x = 0.5, y=-0.25, bordercolor = "#000000",
                               borderwidth = 1))}
      else if(input$param=="Zn.D"&input$showstd==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Dissolved Zinc")))+
                                              geom_line(linetype = 2,aes_string(x=calcstand$SampleDate, y=input$param, color=shQuote("Obs. Dissolved Zinc")))+
                                              geom_point(aes(x=SampleDate, y=ZN.CH,color="Calc. Chronic Standard"))+geom_point(aes(x=SampleDate, y=ZN.AC,color="Calc. Acute Standard"))+
                                              geom_line(aes(x=SampleDate, y=ZN.CH,color="Calc. Chronic Standard"),linetype=2)+geom_line(aes(x=SampleDate, y=ZN.AC,color="Calc. Acute Standard"),linetype=2)+
                                              labs(title = "Observed Dissolved Zinc and Calculated Standards")+
                                              scale_color_manual(name="",breaks = c("Calc. Acute Standard", "Calc. Chronic Standard", "Obs. Dissolved Zinc"),
                                                                 values = c("Calc. Acute Standard"="red", "Calc. Chronic Standard"="orange3", "Obs. Dissolved Zinc"="black"))) %>% 
          layout(legend = list(orientation = "h",  
                               xanchor = "center",  
                               x = 0.5, y=-0.25, bordercolor = "#000000",
                               borderwidth = 1))}
          #If user has not checked show standard box, then {Use Graph without standards}
         else{ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param))+
                         geom_line(linetype = 2,aes_string(x=calcstand$SampleDate, y=input$param)))}
      
       
     
  })
  plotType <- reactive({ #Define time series and box plot here.
 
    
    switch(input$type, #input from graph selector box
           "Time Series" = stdplot(), #Uses the graph made in long show standard section above
                         
           "Time Series (Multiple Stations)" = ggplotly(ggplot(data = filt(), aes_string(x=filt()$SampleDate, y=input$param, color = filt()$StationName)) + 
                                      geom_point()+
                                      geom_line(linetype = 2)+
                                      scale_y_continuous(labels = scales::comma)+
                                      labs(x = "Date", y = names(yoptions[which(yoptions == input$param)])))%>% 
                                      layout(legend = list(orientation = "h",  
                                        xanchor = "center",  
                                        x = 0.5, y=-0.25, bordercolor = "#000000",
                                        borderwidth = 1,
                                        title=list(text='Station'))),
           "Box Plot" = ggplotly(ggplot(data = filt(), aes_string(x=filt()$month, y=input$param)) + 
                          geom_boxplot()+
                          scale_y_continuous(labels = scales::comma)+
                          labs(x = "Month")))
  })
  
  
  observe({ 
    output$plot <- renderPlotly({ #Telling Ui what our graph is
        validate(need(input$station !="Select a station here or on the map", "Enter a station number or use the map to click on a station")) #Message to display if no station selected
        req(input$dateslider) #require dateslider to be showing before the graph
        plotType() # The graph itself 
      })
  })
 
  
   
  #Data Table
  output$datatable <- renderDataTable(server = F, { #server = F means that the download button will do all the data not just the 25 showing
    DT::datatable(filt(), extensions = 'Buttons', options = list( #extensions = buttons adds the download buttons
      pageLength=25, scrollX=TRUE, scrollY = "200px", #ensures the table is scrollable
      dom = 'Bfrtip',
      buttons = #options for the download buttons
        list('copy', list(
          extend = 'collection',
          buttons = c('csv', 'excel'),
          text = 'Download')),
          searching=F,
            columnDefs = list(list(  #Limiting how much text is shown in the datatable
             targets = "_all",
              render = JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data != null && data.length > 30 ?",
              "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
               "}")))),
          filter = 'top')
  })
  
  

  
  #map

  output$currentstationmap <- renderLeaflet({
   
    #the map
    m <- leaflet() %>%
      addMapPane(name = "base", zIndex = 410) %>% #ensure that station points are clickable above the base layers
      addMapPane(name = 'layer', zIndex = 420) %>%
      addMapPane(name = 'highlight', zIndex = 415) %>% 
      addTiles(options = leafletOptions(pane="base"),group = "OSM") %>% # Add default OpenStreetMap map tiles
      addProviderTiles(providers$Esri.WorldImagery,options = leafletOptions(pane="base"), group = "Satellite") %>% 
      addLayersControl(baseGroups = c('Street Map', 'Satellite'), overlayGroups = c('Basins','Active Stations',
                                         'Non-Active Stations'),
                       options = layersControlOptions(collapsed = TRUE),
                       position = 'topright') %>%
      
      # list groups to hide on startup
      hideGroup(c('Non-Active Stations', 'Satellite', 'Basins')) %>% 
      
      addCircleMarkers(lng= active$Longitude, lat= active$Latitude, #Active Stations
                       weight=1,col='green',fillColor = 'green',fillOpacity = 0.9, radius = 6,
                       label = paste("Station Number:", active$sn,"-", #label = what shows on hover
                                     "Station Name:", active$Station.Name,"-",
                                     "River:", active$River),
                       popup= paste("Station Number:", active$sn,"<br>",
                                    "Station Name:", active$Station.Name,"<br>",
                                    "Oranization:", active$Organization,"<br>",
                                    "River:", active$River,"<br>",
                                    "WBID:", active$Water.Body.Id),
                       options = leafletOptions(pane="layer"),group = 'Active Stations',
                       layerId = active$sn) %>% 
      addCircleMarkers(lng= nonactive$Longitude, lat= nonactive$Latitude, #Active Stations
                       weight=1,col='grey',fillColor = 'grey',fillOpacity = 0.9, radius = 6,
                       label = paste("Station Number:", nonactive$sn,"-", #label = what shows on hover
                                     "Station Name:", nonactive$Station.Name,"-",
                                     "River:", nonactive$River),
                       popup= paste("Station Number:", nonactive$sn,"<br>",#popup what shows on click
                                    "Station Name:", nonactive$Station.Name,"<br>",
                                    "Oranization:", nonactive$Organization,"<br>",
                                    "River:", nonactive$River,"<br>",
                                    "WBID:", nonactive$Water.Body.Id),
                       options = leafletOptions(pane="layer"),group = 'Non-Active Stations',
                       layerId = nonactive$sn) %>% 
      addLegendCustom(colors = c('green', 'grey'), labels = c('Active', 'Non Active'), sizes = 6) %>% #legend
      addPolygons(data = basin,options = leafletOptions(pane="base"), group = "Basins", fill = T, fillOpacity = 0, popup = paste(basin$HU6NAME), 
                  popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE)) %>% 
      addFullscreenControl()
    m
  }) 
  #filtering map to zoom to current station
  
  mapsn <- reactive({
     dplyr::filter(map, input$station == sn)
  })
  mapsn2 <- reactive({
    req(input$station2)
    dplyr::filter(map, sn %in% input$station2)
  })
  #Setting map zoom
  observe({
   mapsn <- mapsn()
    if(input$station=="Select a station here or on the map"){ #sets the map view to the state if nothing is selected
    leafletProxy('currentstationmap') %>%  
        setView(lng = -105.7821, lat = 39.5501, zoom = 6)}
   else{leafletProxy('currentstationmap', data=mapsn) %>%  #zooms to station if station is selected
       setView(lng = mapsn$Longitude, lat = mapsn$Latitude, zoom = 12)}
  })
  #setting background color for first selected station
  observe({
    mapsn <- mapsn()
  
    leafletProxy('currentstationmap') %>%clearGroup("highlight") 
    leafletProxy('currentstationmap') %>% 
      addCircleMarkers(lng = mapsn$Longitude, lat = mapsn$Latitude, radius = 12, col="red", options = leafletOptions(pane="highlight"), group = "highlight") 
  })
  #Background color for additional stations
  observe({
    mapsn2<-mapsn2()
    leafletProxy('currentstationmap') %>%clearGroup("highlight2") 
    leafletProxy('currentstationmap') %>% 
      addCircleMarkers(lng = mapsn2$Longitude, lat = mapsn2$Latitude, radius = 12, col="red", options = leafletOptions(pane="highlight"), group = "highlight2")
  }) 
  #clear colors from additional stations when the graph type changes
  observe({
    if(input$type=="Time Series"|input$type=="Box Plot"){leafletProxy('currentstationmap') %>%clearGroup("highlight2")} 
  })
  #update station number input based on click. This then updates the data and map view.   
  observeEvent(input$currentstationmap_marker_click$id,{ 
    sn<-input$currentstationmap_marker_click$id
    updateSelectInput(session, 'station', choices = stations, selected = sn)
  })
  
  
####Watershed tab----
  
  #Select Watershed
  output$watershed <- renderUI({
    selectInput("watershed", label = "Select Watershed", choices = ws, multiple = F)
  })
  
  #filter data by Watershed
  wsdata<- reactive({
    dplyr::filter(data, RwWatershed %in% input$watershed)
    
  })
  #group by WBID or River
  output$wbidorriver <- renderUI({
    choice <- c("River"="River","WBID"="WaterBodyId")
    radioButtons("wbidorriver", label = "Graph By", choices = choice, selected = character(0), inline = T)
  })
  #WBID
  output$WBID <- renderUI({
    req(input$wbidorriver == "WaterBodyId")
    wbid<-c(unique(wsdata()$WaterBodyId))
    selectInput("WBID", label = "Select Water Body ID", choices = wbid, multiple = F)
  })
  #River
  output$River <- renderUI({
    req(input$wbidorriver == "River")
    riv<-c(unique(wsdata()$River))
    selectInput("River", label = "Select River", choices = riv, multiple = F)
  }) 
  #parameter
  output$param2 <- renderUI({
    req(input$wbidorriver == "River"|input$wbidorriver == "WaterBodyId")
    selectInput("param2", label = "Select Parameter", choices = yoptions, selected = NULL, multiple = F)
  })
  #Graph Options
  output$trendline <- renderUI({
    req(input$wbidorriver=="River"|input$wbidorriver == "WaterBodyId")
    
    checkboxInput("trendline", label = "Trendline")
  })
  #Show standards?
  output$showstd2<- renderUI({
    req(input$wbidorriver=="WaterBodyId"& input$param2 %in% c("Ph","Al.T","As.D","Cd.D","Cu.D","Fe.T","Pb.D","Se.D","Zn.D"))
    checkboxInput("showstd2", label = "Calculate Aquatic Life Standards", value = FALSE)
  })
  #Filter Data for date slider
  wsfilt<- reactive({
    switch(input$wbidorriver,
           "WaterBodyId"=dplyr::filter(wsdata(), WaterBodyId %in% input$WBID),
           "River"=dplyr::filter(wsdata(), River %in% input$River))
  
    
  })

  #Slider Dates WBID
  output$WBIDslider <- renderUI({
    req(input$wbidorriver == "River"|input$wbidorriver == "WaterBodyId")
    date_min = min(wsfilt()$SampleDate)
    date_max = max(wsfilt()$SampleDate)
    sliderInput("WBIDslider", "Date Range:", min=date_min, max=date_max, value=c(date_min,date_max))
  })
  #re-filter data for graphing with date slider
  wsdate <- reactive({
 wsfilt() %>% dplyr::filter(SampleDate >= min(input$WBIDslider), SampleDate <= max(input$WBIDslider))
  })
  
  
  #Graphs - each parameter with a standard is written out, else = traditional graph
  plot2<- reactive({
    
    if(input$wbidorriver=="River"){   
      p<-ggplot(data = wsdate(), aes_string(x=wsdate()$SampleDate, y=input$param2)) +
      geom_point(aes_string(color = wsdate()$StationNumber))+
      scale_y_continuous(labels = scales::comma)+
      labs(x = "Date", y = names(yoptions[which(yoptions == input$param2)]), color = "Station")
    if(input$trendline==F){ggplotly(p)}
    else{ggplotly(p+geom_smooth())}
      }
    
    else{
      if(input$wbidorriver=="WaterBodyId"){
      calcstand<-wsdate()
      calcstand<-standards(calcstand)
      `%notin%` <- Negate(`%in%`)} 
    p<-ggplot(data = calcstand) + 
      scale_y_continuous(labels = scales::comma)+
      labs(x = "Date", y = names(yoptions[which(yoptions == input$param2)]))
    
    if(input$trendline==T){p<-p+geom_smooth(aes_string(x=calcstand$SampleDate, y=input$param2))}
    
    if(input$param2 %notin% c("Ph","Al.T","As.D","Cd.D","Cu.D","Fe.T","Pb.D","Se.D","Zn.D")){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param2, color = calcstand$StationNumber))+labs(color ="Station"))}
    else if(input$param2=="Ph"&input$showstd2==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param2))+
                                                           geom_line(aes(x=SampleDate, y=PHMIN),linetype=2, color="orange3")+geom_line(aes(x=SampleDate, y=PHMAX), linetype=2,color="orange3"))}
    
    else if(input$param2=="Al.T"&input$showstd2==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param2, color=shQuote("Obs. Total Aluminum")))+
                                                             geom_point(aes(x=SampleDate, y=AL.CH,color="Calc. Chronic Standard"))+geom_point(aes(x=SampleDate, y=AL.AC,color="Calc. Acute Standard"))+
                                                             labs(title = "Observed Total Aluminium and Calculated Standards")+
                                                             scale_color_manual(name="",breaks = c("Calc. Acute Standard", "Calc. Chronic Standard", "Obs. Total Aluminum"),
                                                                                values = c("Calc. Acute Standard"="red", "Calc. Chronic Standard"="orange3", "Obs. Total Aluminum"="black"))) %>% 
        layout(hovermode="x",legend = list(orientation = "h",  
                             xanchor = "center",  
                             x = 0.5, y=-0.25, bordercolor = "#000000",
                             borderwidth = 1))}
    else if(input$param2=="As.D"&input$showstd2==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param2, color=shQuote("Obs. Dissolved Arsenic")))+
                                                             geom_line(aes(x=SampleDate, y=AS.CH,color="Chronic Standard"),linetype=2)+geom_line(aes(x=SampleDate, y=AS.AC,color="Acute Standard"),linetype=2)+
                                                             labs(title = "Observed Dissolved Arsenic and Standards")+
                                                             scale_color_manual(name="",breaks = c("Acute Standard", "Chronic Standard", "Obs. Dissolved Arsenic"),
                                                                                values = c("Acute Standard"="red", "Chronic Standard"="orange3", "Obs. Dissolved Arsenic"="black"))) %>% 
        layout(hovermode="x",legend = list(orientation = "h",  
                             xanchor = "center",  
                             x = 0.5, y=-0.25, bordercolor = "#000000",
                             borderwidth = 1))}
    else if(input$param2=="Cd.D"&input$showstd2==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param2, color=shQuote("Obs. Dissolved Cadmium")))+
                                                             geom_point(aes(x=SampleDate, y=CD.CH,color="Calc. Chronic Standard"))+
                                                             labs(title = "Observed Dissolved Cadmium and Calculated Standards")+
                                                             scale_color_manual(name="",breaks = c("Calc. Chronic Standard", "Obs. Dissolved Cadmium"),
                                                                                values = c("Calc. Chronic Standard"="orange3", "Obs. Dissolved Cadmium"="black"))) %>% 
        layout(hovermode="x",legend = list(orientation = "h",  
                             xanchor = "center",  
                             x = 0.5, y=-0.25, bordercolor = "#000000",
                             borderwidth = 1))} 
    else if(input$param2=="Cu.D"&input$showstd2==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param2, color=shQuote("Obs. Dissolved Copper")))+
                                                             geom_point(aes(x=SampleDate, y=CU.CH,color="Calc. Chronic Standard"))+geom_point(aes(x=SampleDate, y=CU.AC,color="Calc. Acute Standard"))+
                                                             labs(title = "Observed Dissolved Copper and Calculated Standards")+
                                                             scale_color_manual(name="",breaks = c("Calc. Acute Standard", "Calc. Chronic Standard", "Obs. Dissolved Copper"),
                                                                                values = c("Calc. Acute Standard"="red", "Calc. Chronic Standard"="orange3", "Obs. Dissolved Copper"="black"))) %>% 
        layout(hovermode="x",legend = list(orientation = "h",  
                             xanchor = "center",  
                             x = 0.5, y=-0.25, bordercolor = "#000000",
                             borderwidth = 1))}
    else if(input$param2=="Fe.T"&input$showstd2==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param2, color=shQuote("Obs. Total Iron")))+ 
                                                             geom_line(aes(x=SampleDate, y=FE.CH,color="Chronic Standard"),linetype=2)+
                                                             labs(title = "Observed Total Iron and Chronic Standard")+
                                                             scale_color_manual(name="",breaks = c("Chronic Standard", "Obs. Total Iron"),
                                                                                values = c("Chronic Standard"="orange3", "Obs. Total Iron"="black"))) %>% 
        layout(hovermode="x",legend = list(orientation = "h",  
                             xanchor = "center",  
                             x = 0.5, y=-0.25, bordercolor = "#000000",
                             borderwidth = 1))}
    else if(input$param2=="Pb.D"&input$showstd2==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param2, color=shQuote("Obs. Dissolved Lead")))+
                                                             geom_point(aes(x=SampleDate, y=PB.CH,color="Calc. Chronic Standard"))+geom_point(aes(x=SampleDate, y=PB.AC,color="Calc. Acute Standard"))+
                                                             labs(title = "Observed Dissolved Lead and Calculated Standards")+
                                                             scale_color_manual(name="",breaks = c("Calc. Acute Standard", "Calc. Chronic Standard", "Obs. Dissolved Lead"),
                                                                                values = c("Calc. Acute Standard"="red", "Calc. Chronic Standard"="orange3", "Obs. Dissolved Lead"="black"))) %>% 
        layout(hovermode="x",legend = list(orientation = "h",  
                             xanchor = "center",  
                             x = 0.5, y=-0.25, bordercolor = "#000000",
                             borderwidth = 1))}
    else if(input$param2=="Mn.D"&input$showstd2==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param2, color=shQuote("Obs. Dissolved Manganese")))+
                                                             geom_point(aes(x=SampleDate, y=MN.CH,color="Calc. Chronic Standard"))+geom_point(aes(x=SampleDate, y=MN.AC,color="Calc. Acute Standard"))+
                                                             labs(title = "Observed Dissolved Manganese and Calculated Standards")+
                                                             scale_color_manual(name="",breaks = c("Calc. Acute Standard", "Calc. Chronic Standard", "Obs. Dissolved Manganese"),
                                                                                values = c("Calc. Acute Standard"="red", "Calc. Chronic Standard"="orange3", "Obs. Dissolved Manganese"="black"))) %>% 
        layout(hovermode="x",legend = list(orientation = "h",  
                             xanchor = "center",  
                             x = 0.5, y=-0.25, bordercolor = "#000000",
                             borderwidth = 1))}
    else if(input$param2=="Se.D"&input$showstd2==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param2, color=shQuote("Obs. Dissolved Selenium")))+
                                                             geom_line(aes(x=SampleDate, y=SE.CH,color="Chronic Standard"),linetype=2)+geom_line(aes(x=SampleDate, y=SE.AC,color="Acute Standard"),linetype=2)+
                                                             labs(title = "Observed Dissolved Selenium and Standards")+
                                                             scale_color_manual(name="",breaks = c("Acute Standard", "Chronic Standard", "Obs. Dissolved Selenium"),
                                                                                values = c("Acute Standard"="red", "Chronic Standard"="orange3", "Obs. Dissolved Selenium"="black"))) %>% 
        layout(hovermode="x",legend = list(orientation = "h",  
                             xanchor = "center",  
                             x = 0.5, y=-0.25, bordercolor = "#000000",
                             borderwidth = 1))}
    else if(input$param2=="Zn.D"&input$showstd2==T){ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param2, color=shQuote("Obs. Dissolved Zinc")))+
                                                             geom_point(aes(x=SampleDate, y=ZN.CH,color="Calc. Chronic Standard"))+geom_point(aes(x=SampleDate, y=ZN.AC,color="Calc. Acute Standard"))+
                                                             labs(title = "Observed Dissolved Zinc and Calculated Standards")+
                                                             scale_color_manual(name="",breaks = c("Calc. Acute Standard", "Calc. Chronic Standard", "Obs. Dissolved Zinc"),
                                                                                values = c("Calc. Acute Standard"="red", "Calc. Chronic Standard"="orange3", "Obs. Dissolved Zinc"="black"))) %>% 
        layout(hovermode="x",legend = list(orientation = "h",  
                             xanchor = "center",  
                             x = 0.5, y=-0.25, bordercolor = "#000000",
                             borderwidth = 1)) }
    else{ggplotly(p+geom_point(aes_string(x=calcstand$SampleDate, y=input$param2,color=calcstand$StationNumber))+labs(color="Station"))}
    
    }
  })
  
  
  #Send graph to ui
  observe({
    output$tab2plot <-renderPlotly({
    validate(need(input$wbidorriver != "WBID"|input$wbidorriver != "River", "Select a watershed and river or WBID"))
    req(input$WBIDslider)
    plot2()
  }) 
  })
  #Data Table
  output$wsdatatable <- renderDataTable(server = F, { #server = F means that the download button will do all the data not just the 25 showing
    req(input$WBIDslider)
    DT::datatable(wsdate(), extensions = 'Buttons', options = list( #extensions = buttons adds the download buttons
      pageLength=25, scrollX=TRUE, scrollY = "200px", #ensures the table is scrollable
      dom = 'Bfrtip',
      buttons = #options for the download buttons
        list('copy', list(
          extend = 'collection',
          buttons = c('csv', 'excel'),
          text = 'Download')),
      searching=F,
      columnDefs = list(list(
        targets = "_all",
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data != null && data.length > 30 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
          "}")))),
      filter = 'top')
  })
  output$highlight <- renderLeaflet({
    #the map
      leaflet() %>%
      addMapPane(name = "base", zIndex = 410) %>% #ensure that station points are clickable above the base layers
      addMapPane(name = 'layer', zIndex = 420) %>% 
      addTiles(options = leafletOptions(pane="base"),group = "OSM") %>% # Add default OpenStreetMap map tiles
      addProviderTiles(providers$Esri.WorldImagery,options = leafletOptions(pane="base"), group = "Satellite") %>% 
      addLayersControl(baseGroups = c('Street Map', 'Satellite'), overlayGroups = c('Basins'),
                       options = layersControlOptions(collapsed = TRUE),
                       position = 'topright') %>%
      addPolygons(data = basin,options = leafletOptions(pane="base"), group = "Basins", fill = T, fillOpacity = 0, popup = paste(basin$HU6NAME), 
                  popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE)) %>%
      addLegendCustom(colors = c('green', 'grey'), labels = c('Active', 'Non Active'), sizes = 6) %>% #legend
      addFullscreenControl()
  })
  dynmap<- reactive({
    req(input$WBIDslider)
    stationnum<-wsdate()$StationNumber
    dplyr::filter(map, sn %in%stationnum)
  })
  observe({ 
    active <- dplyr::filter(dynmap(), Station.Status=="Active")
    nonactive <- dplyr::filter(dynmap(), Station.Status=="NonActive")
    leaflet::leafletProxy("highlight") %>% clearMarkers() %>% 
      addCircleMarkers(lng= nonactive$Longitude, lat= nonactive$Latitude, #Active Stations
                       weight=1,col='grey',fillColor = 'grey',fillOpacity = 0.9, radius = 6,
                       label = paste("Station Number:", nonactive$sn,"-", #label = what shows on hover
                                     "Station Name:", nonactive$Station.Name,"-",
                                     "River:", nonactive$River),
                       popup= paste("Station Number:", nonactive$sn,"<br>",#popup what shows on click
                                    "Station Name:", nonactive$Station.Name,"<br>",
                                    "Oranization:", nonactive$Organization,"<br>",
                                    "River:", nonactive$River,"<br>",
                                    "WBID:", nonactive$Water.Body.Id),
                       options = leafletOptions(pane="layer"),group = 'Non-Active Stations',
                       layerId = nonactive$sn) %>% 
      addCircleMarkers(lng= active$Longitude, lat= active$Latitude, #Active Stations
                       weight=1,col='green',fillColor = 'green',fillOpacity = 0.9, radius = 6,
                       label = paste("Station Number:", active$sn,"-", #label = what shows on hover
                                     "Station Name:", active$Station.Name,"-",
                                     "River:", active$River),
                       popup= paste("Station Number:", active$sn,"<br>",
                                    "Station Name:", active$Station.Name,"<br>",
                                    "Oranization:", active$Organization,"<br>",
                                    "River:", active$River,"<br>",
                                    "WBID:", active$Water.Body.Id),
                       options = leafletOptions(pane="layer"),group = 'Active Stations',
                       layerId = active$sn)
 
  })

}
#Created by Noah Goodkind for River Watch