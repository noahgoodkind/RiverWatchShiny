#User interface section
ui <- dashboardPage(           #Using shiny dashboard for the framework
  dashboardHeader(title = "River Watch Data Tool", #Title in upper left
                  tags$li(class="dropdown",tags$a(href="https://coloradoriverwatch.org/", "River Watch of Colorado", target = "_blank"))), #Link to RW website in upper right
  dashboardSidebar( #Specify sidebar items
    sidebarMenu(menuItem("By Station", tabName = "graphs",icon = icon("chart-line")), #Change sidebar tab labels here
                menuItem("By River or WBID", tabName = "ws",icon = icon("chart-line")),
                menuItem("User Guide", tabName = "guide",icon = icon("book")))),
  dashboardBody( #Specify content that goes in each of the dashboard sidebar tabs
    tabItems( 
      tabItem(tabName = "graphs", #First tab - linked to the tabName in sidebarMenu
        fluidRow(
          column(3, #Defining content of the tab below. Select station, select parameter, "station2" = Select additional stations, Graph Type, showstd=Show standard checkbox, and plot
            selectInput("station", label ="Select station",choices = stations, selected = "Select a Station below or on the Map", multiple = F),
            selectInput("param", label = "Select Parameter", choices = yoptions),
            uiOutput("station2"),
            radioButtons("type", label = "Graph Type", choices = c("Time Series",'Time Series (Multiple Stations)', "Box Plot"), inline=F),
            uiOutput("showstd"),
              bsTooltip(id="showstd", 
                      title="River Watch keeps the standards as up to date as possible, however they should be used as a general reference only. River Watch does not assign attainment or impairment of aquatic life standards.", placement ="bottom", trigger = "hover"),
            uiOutput("dateslider")), 
          column(9,
            plotlyOutput("plot"))),
        fluidRow(
          tabsetPanel(type = "tabs", #making tab for our map and datatable
                      tabPanel("Map",leaflet::leafletOutput("currentstationmap")),
                      tabPanel("Data Table", dataTableOutput("datatable"))))
        
      ),
      tabItem(tabName = "ws", #Second tab -  linked to the tabName in sidebarMenu 
                fluidRow(column(3, #Most inputs below are defined on the server side bsTooltip()=the popup disclaimer. Modify text in title=
                       uiOutput("watershed"),    
                       uiOutput("wbidorriver"), 
                       uiOutput("WBID"),
                        bsTooltip(id="WBID", 
                                 title="Water body identification (WBID) is an assessment unit used by the Colorado Department of Public Health and the Environment to  represent Colorado\\'s waterbodies at the watershed or sub-watershed scale.", placement ="top", trigger = "hover"),
                       
                       uiOutput("River"),
                       tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),#Ensures that the dropdown below will not be blocked by the map
                       uiOutput("param2"), 
                       uiOutput("trendline"),
                        bsTooltip(id="trendline", 
                                 title="Be careful drawing conclusions from the trendline. Consider station location, how much data is available, and the time period covered.", placement ="top", trigger = "hover"),
                      uiOutput("showstd2"),
                      bsTooltip(id="showstd2", 
                                title="River Watch keeps the standards as up to date as possible, however they should be used as a general reference only. River Watch does not assign attainment or impairment of aquatic life standards.", placement ="bottom", trigger = "hover"),
                    uiOutput("WBIDslider")),
                column(9,
                       plotlyOutput("tab2plot"))),
              fluidRow(
                tabsetPanel(type="tabs",
                            tabPanel("Map",leaflet::leafletOutput("highlight")),
                            tabPanel("Data Table", dataTableOutput("wsdatatable"))))),
      tabItem(tabName = "guide", #Third tab -  linked to the tabName in sidebarMenu
                 includeMarkdown('./userguide/userguide.md')) #User guide. Written in the markdown file, "userguide.md". 
    )
  )
)
#Created by Noah Goodkind for River Watch