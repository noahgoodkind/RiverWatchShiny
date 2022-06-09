#Everything in global.R is done once when the server starts. ui and server can both draw from the data here. 
library(shiny)
library(shinyBS)#tooltip on hover
library(tidyverse)
library(shinydashboard) #The dashboard
library(leaflet) #Map
library(leaflet.extras)#More map tools
library(plotly) #Graphs
library(DT) #The data table
library(sf) #Loading GIS data

data <- read.csv("./20220425_ChemicalResults.csv") #Update data here. Use River Watch database. Do not touch column names in excel. 
data <-  dplyr::filter(data, SampleDate != min((data$SampleDate))) #filter out unknown sample dates. They show up as 01-01-1800
data$StationNumber <-as.factor(data$StationNumber)
data$StationName <-as.factor(data$StationName)
data$RwWatershed <-as.factor(data$RwWatershed)
data$WaterBodyId <- as.factor(data$WaterBodyId)
data$SampleDate <- as.Date(data$SampleDate, format = "%m/%d/%Y") 
data <- data %>%
  separate(SampleDate, sep="-", into = c("year", "month", "day"), remove = F )
data$month <- as.factor(data$month)


yoptions <- c("Water Temperature (°C)"="TempC",
              "Dissolved Oxygen (mg/L)" = "Do",      #"display name" = "column name"
              "Dissolved Oxygen (% Saturation)"="DoSat",
              "pH"="Ph",
              'Total Hardness (mg/L CaCO3)'="TotalHard",
              "Total Alkalinity (mg/L CaCO3)"="TotalAlk",
              "Phenolphthalein Alkalinity (mg/L CaCO3)" = "PhenAlk",
              "Dissolved Aluminium (μg/L)" = "Al.D","Total Aluminium (μg/L)"="Al.T",
              "Dissolved Arsenic (μg/L)"="As.D", "Total Arsenic (μg/L)"="As.T",
              "Dissolved Cadmium (μg/L)"="Cd.D","Total Cadmium (μg/L)"="Cd.T",
              "Dissolved Copper (μg/L)"="Cu.D", "Total Copper (μg/L)"="Cu.T",
              "Dissolved Iron (μg/L)"="Fe.D", "Total Iron (μg/L)"="Fe.T",
              "Dissolved Lead (μg/L)"= "Pb.D", "Total Lead (μg/L)"="Pb.T",
              "Dissolved Manganese (μg/L)"="Mn.D", "Total Manganese (μg/L)"="Mn.T",
              "Dissolved Selenium (μg/L)"="Se.D", "Total Selenium (μg/L)"="Se.T",
              "Dissolved Zinc (μg/L)"="Zn.D","Total Zinc (μg/L)"="Zn.T",
              "Dissolved Calcium (μg/L)"="Ca.D","Total Calcium (μg/L)"="Ca.T",
              "Dissolved Potassium (μg/L)"="K.D","Total Potassium (μg/L)"="K.T",
              "Dissolved Magnesium (μg/L)"="Mg.D", "Total Magnesium (μg/L)"="Mg.T",
              "Dissolved Sodium (μg/L)"="Na.D", "Total Sodium (μg/L)"="Na.T",
              "Total Suspended Solids(mg/L)"="Tss",
              "Ammonia (mg/L)"="Ammonia",
              "Chloride (mg/L)"="Chloride",
              "Nitrate-Nitrite (mg/L)"= "NitrateNitrite",
              "Sulfate (mg/L)"="Sulfate")

stations <- c("Select a station here or on the map", levels(data$StationNumber))#creates a vector with just the station numbers. Used for selecting station number.
ws<- c("Arkansas" = "AK","Colorado"="CO","Dolores"="DO", "Green"="GR", "Gunnison"="GU", "Rio Grande"="RG", "San Juan"="SJ", "South Platte"="SP")

#Map Data
map <- read.csv("./AllStations2021.csv") #Update map data here. DO NOT RENAME COLUMNS IN EXCEL. Comes from station metadata download in RW database.  
map <- dplyr::rename(map, sn = Station.. ) 
map$sn <- as.character(map$sn)
map$Station.Status <- as.factor(map$Station.Status) #Station status must be done manually in excel. Ie.. database returns all stations as active.

#basin layers - Shpae file
basin <- sf::st_read(dsn = "./basins/All_River_Basins.shp", layer = "All_River_Basins") #Read in shape file
basin <- sf::st_transform(basin, st_crs("+proj=longlat +datum=WGS84")) #Transform projection from WGS 84 to Lat-Long for leaflet

#for the map legend
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.9){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
}
#station status
active <- dplyr::filter(map, Station.Status=="Active") #Makes separate data frames for active and non active stations
nonactive <- dplyr::filter(map, Station.Status=="NonActive")

#Standards Function - Update AQ life standards here
standards<-function(calcstand){ 
  #hardness caps are set at 400 for metals, so set any hardness greater than 400 to 400
  calcstand$TOTAL_HARD <- ifelse(calcstand$TotalHard <= 400, calcstand$TotalHard, 400)
  calcstand$AL_HARD <- ifelse(calcstand$TotalHard <= 220, calcstand$TotalHard, 220)
  
  #CH is chronic and AC is acute
  #The following section calculates the standard for each sampling event
  
  #aluminum standards using equations provided by WQCC
  calcstand$AL.CH <- with(calcstand, exp((1.3695*log(calcstand$TOTAL_HARD)-.1158)))
  calcstand$AL.AC <- with(calcstand, exp((1.3695*log(calcstand$TOTAL_HARD)+1.8308)))
  #cadmium standards using equations provided by WQCC, with modified WBIDs
  calcstand$CD.CH <- ifelse(calcstand$WaterBodyId == "COARUA02B" |calcstand$WaterBodyId == "COARUA02C" | calcstand$WaterBodyId == "COUCEA05A" | calcstand$WaterBodyId == "COUCEA05B" |  calcstand$WaterBodyId == "COUCEA05C", exp((0.7998*log(calcstand$TOTAL_HARD)-3.1725))*(1.10672-log(calcstand$TOTAL_HARD)*0.041838),
                            ifelse(calcstand$WaterBodyId == "COUCBL02B", 0.5*exp((1.0166*log(calcstand$TOTAL_HARD)-3.132)),
                                   ifelse(calcstand$WaterBodyId == "CORGGC09B", exp((0.7852*log(calcstand$TOTAL_HARD)-2.906)), exp((0.7977*log(calcstand$TOTAL_HARD)-3.909))*(1.10672-log(calcstand$TOTAL_HARD)*0.041838))))
  
  
  #copper standards using equations provided by WQCC
  calcstand$CU.AC <- ifelse (calcstand$WaterBodyId == "CORGCB09B", exp((0.8889*log(calcstand$TOTAL_HARD)+0.53)),
                             ifelse (calcstand$WaterBodyId == "COUCEA05A", 0.96*exp((0.9801*log(calcstand$TOTAL_HARD)-1.1073)),
                                     ifelse (calcstand$WaterBodyId == "COUCEA05B" | calcstand$WaterBodyId == "COCUEA05C", 0.96*exp((0.9801*log(calcstand$TOTAL_HARD)-1.5865)), exp((0.9422*log(calcstand$TOTAL_HARD)-1.7408)))))
  
  calcstand$CU.CH <- ifelse (calcstand$WaterBodyId == "CORGCB09B", exp((0.8889*log(calcstand$TOTAL_HARD)-1.1519)),
                             ifelse (calcstand$WaterBodyId == "COUCEA05A", 0.96*exp((0.5896*log(calcstand$TOTAL_HARD)-0.0053)),
                                     ifelse (calcstand$WaterBodyId == "COUCEA05B" | calcstand$WaterBodyId == "COCUEA05C", 0.96*exp((0.5897*log(calcstand$TOTAL_HARD)-0.4845)), exp((0.8545*log(calcstand$TOTAL_HARD)-1.7428)))))
  
  #lead standards using equations provided by WQCC
  calcstand$PB.CH <- with(calcstand, exp((1.273*log(calcstand$TOTAL_HARD)-4.705))*(1.46203-log(calcstand$TOTAL_HARD)*0.145712))
  calcstand$PB.AC <- with(calcstand, exp((1.273*log(calcstand$TOTAL_HARD)-1.46))*(1.46203-log(calcstand$TOTAL_HARD)*0.145712))
  
  #manganese standards using equations provided by WQCC
  calcstand$MN.AC <- with(calcstand, exp((0.3331*log(calcstand$TOTAL_HARD)+6.4676)))
  calcstand$MN.CH <- with(calcstand, exp((0.3331*log(calcstand$TOTAL_HARD)+5.8743)))
  
  #zinc standards using equations provided by WQCC, with modified WaterBodyIds
  calcstand$ZN.AC <- ifelse(calcstand$WaterBodyId == "COARUA02B" | calcstand$WaterBodyId == "COARUA02C", 0.978*exp((0.8537*log(calcstand$TOTAL_HARD)+2.2178)),
                            ifelse(calcstand$WaterBodyId == "CORGCB09B", exp((0.8179*log(calcstand$TOTAL_HARD)+3.757)),
                                   ifelse(calcstand$WaterBodyId == "COUCBL02B", exp((0.9805*log(calcstand$TOTAL_HARD)+1.402)),
                                          ifelse(calcstand$WaterBodyId == "COUCEA05A"| calcstand$WaterBodyId == "COUCEA05B", 0.978*exp((0.8537*log(calcstand$TOTAL_HARD)+2.1302)),
                                                 ifelse(calcstand$WaterBodyId == "COUCEA05C", 0.978*exp((0.8537*log(calcstand$TOTAL_HARD)+1.4189)),
                                                        ifelse(calcstand$WaterBodyId == "COUCBL02A", exp((1.25*log(calcstand$TOTAL_HARD)+0.799)),
                                                               ifelse(calcstand$WaterBodyId == "COSPCL02" | calcstand$WaterBodyId == "COSPCL02A" | calcstand$WaterBodyId == "COSPCL11", 0.978*exp((0.8537*log(calcstand$TOTAL_HARD)+1.9467)),
                                                                      ifelse(calcstand$WaterBodyId == "COSPCL05", exp((0.8404*log(calcstand$TOTAL_HARD)+1.8810)),
                                                                             ifelse(calcstand$WaterBodyId == "COUCBL06B", 0.978*exp((0.8537*log(calcstand$TOTAL_HARD)+1.5227)), 0.978*exp((0.9094*log(calcstand$TOTAL_HARD)+0.9095)))))))))))
  
  calcstand$ZN.CH <- ifelse(calcstand$WaterBodyId == "COARUA02B" | calcstand$WaterBodyId == "COARUA02C", 0.986*exp((0.8537*log(calcstand$TOTAL_HARD)+2.0469)),
                            ifelse(calcstand$WaterBodyId == "CORGCB09B", exp((0.8179*log(calcstand$TOTAL_HARD)+2.907)),
                                   ifelse(calcstand$WaterBodyId == "COUCBL02B", exp((0.9805*log(calcstand$TOTAL_HARD)+1.402)),
                                          ifelse(calcstand$WaterBodyId == "COUCEA05A"| calcstand$WaterBodyId == "COUCEA05B", 0.986*exp((0.8537*log(calcstand$TOTAL_HARD)+1.9593)),
                                                 ifelse(calcstand$WaterBodyId == "COUCEA05C", 0.986*exp((0.8537*log(calcstand$TOTAL_HARD)+1.2481)),
                                                        ifelse(calcstand$WaterBodyId == "COUCBL02A", exp((1.25*log(calcstand$TOTAL_HARD)+0.799)),
                                                               ifelse(calcstand$WaterBodyId == "COSPCL02" | calcstand$WaterBodyId == "COSPCL02A" | calcstand$WaterBodyId == "COSPCL11", 0.986*exp((0.8537*log(calcstand$TOTAL_HARD)+1.8032)),
                                                                      ifelse(calcstand$WaterBodyId == "COSPCL05", exp((0.8404*log(calcstand$TOTAL_HARD)+1.5127)),
                                                                             ifelse(calcstand$WaterBodyId == "COUCBL06B", 0.986*exp((0.8537*log(calcstand$TOTAL_HARD)+1.3519)), 0.986*exp((0.9094*log(calcstand$TOTAL_HARD)+0.6235)))))))))))
  ###pH standards from WQCC
  calcstand$PHMIN <- 6.5
  calcstand$PHMAX <- 9
  
  ###Arsenic standards from WQCC
  calcstand$AS.AC <- 340
  calcstand$AS.CH <- 150
  
  ####Iron standards from WQCC
  calcstand$FE.CH <- 1000
  
  ###Selenium standards from WQCC
  calcstand$SE.AC <- 18.4
  calcstand$SE.CH <- 4.6
  
  return(calcstand)
}
#Created by Noah Goodkind for River Watch----