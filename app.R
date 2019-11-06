
library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(leaflet)
library(DT)
library(shinydashboard)
library(scales)
library(grid)
library(shinyWidgets)
library(rsconnect)
library(leaflet.extras)
library(splitstackshape)
library(RColorBrewer)
library(leaflet.extras)
library(rgdal)


#read in tab delimited TEXT and .csv files of reports, bird observations, LRRS gps boundaries

point.coords <- read.csv("T:/Landbirds/ALMS/GPS_Points/R7MBMlb_ALL_ALMS_Points.csv", stringsAsFactors = FALSE)

point.c <- subset(point.coords, select = c("Name", "Latitude", "Longitude"))

spco <- read.csv("T:/Landbirds/ALMS/Data/ALMS/SPCO.csv", stringsAsFactors = FALSE)

specpoint6 <- read.csv("T:/Landbirds/ALMS/Data/ALMS/R7MBMlb_ALMS_distance_2016.csv", stringsAsFactors = FALSE)
specpoint7 <- read.csv("T:/Landbirds/ALMS/Data/ALMS/R7MBMlb_ALMS_distance_2017.csv", stringsAsFactors = FALSE)
specpoint8 <- read.csv("T:/Landbirds/ALMS/Data/ALMS/R7MBMlb_ALMS_distance_2018.csv", stringsAsFactors = FALSE)
specpoint9 <- read.csv("T:/Landbirds/ALMS/Data/ALMS/R7MBMlb_ALMS_distance_2019.csv", stringsAsFactors = FALSE)
specpoint <- rbind(specpoint6, specpoint7, specpoint8, specpoint9)


specpoint$block <- paste("B",specpoint$Block_No, sep = "_")
specpoint$block_point <- paste(specpoint$Block_No, specpoint$Point, sep = "_")

specpoint <- select(specpoint, block, block_point, Date, Year, Point, Hr, Min, FMLast, Species,  No_gt_1)

#sum and combine observations of same species from same point
specpoint.agg <- aggregate(No_gt_1 ~ block + block_point + Date + Year + Point + Hr + Min + FMLast + Species, specpoint, sum)
specpoint.agg$block <- as.factor(specpoint.agg$block)

gps.obs <- merge(specpoint.agg, point.c, by.x = "block_point", by.y = "Name") 

gps.obs <- merge(gps.obs, spco, by.x = "Species", by.y = "code") 
gps.obs$species_count <- 1




#input species of conservation concern
BCC <-  c("Red-faced Cormorant", "Rock Sandpiper", "Red-legged Kittiwake", "Aleutian Tern", 
          "Arctic Tern", "Kittlitz's Murrelet","Marbled Murrelet", "McKay's Bunting", "Whiskered Auklet",
          "Black Oystercatcher", "Pelagic Cormorant", "Laysan Albatross","Red-throated Loon", 
          "Yellow-billed Loon", "Peregrine Falcon", "Solitary Sandpiper", "Lesser Yellowlegs", "Whimbrel",
          "Bristle-thighed Curlew", "Hudsonian Godwit", "Bar-tailed Godwit", "Marbled Godwit", "Red Knot", 
          "Dunlin", "Short-billed Dowitcher", "Buff-breasted Sandpiper", "Smith's Longspur")
DOD <-  c("Bald Eagle", "Northern Goshawk", "Golden Eagle", "Buff-breasted Sandpiper", "Olive-sided Flycatcher", 
          "Rusty Blackbird", "Laysan Albatross", "Black-footed Albatross", "Pacific Golden-Plover")
FWSE <- c("Short-tailed Albatross")
FWST <- c("Spectacled Eider", "Steller's Eider")
ADFG <- c( "McKay's Bunting", "Gray-headed Chickadee", "Brant", "Spectacled Eider", "King Eider", 
           "Bristle-thighed Curlew", "Marbled Godwit", "Rock Sandpiper", "Kittlitz's Murrelet", "Aleutian Tern")
PIF <-  c("Gyrfalcon", "Snowy Owl", "Gray-cheeked Thrush", "Smith's Longspur", "Hoary Redpoll", "Varied Thrush", 
          "McKay's Bunting", "Golden-crowned Sparrow", "Blackpoll Warbler", "Rusty Blackbird", 
          "Olive-sided Flycatcher", "Black-backed Woodpecker", "Great Gray Owl", "Sharp-tailed Grouse",
          "White-tailed Ptarmigan", "Hammond's Flycatcher", "Northern Shrike", "American Dipper",
          "Bohemian Waxwing", "Townsend's Warbler", "White-winged Crossbill")

#assign LLRS to NWRs
#all <- c(paste(unique(specpoint.agg$block)))
ANWR <- c("B_38152", "B_38634", "B_38627", "B_39106")
KANWR <- c("B_37154", "B_37393")
INWR <- c("B_29941", "B_30903", "B_29942", "B_30664")
KONWR <- c("B_35691", "B_36416", "B_36895")
NNWR <- c("B_33074", "B_32597")
SNWR <- c("B_39274", "N/A")
TNWR <- c("B_430", "B_431")

#merge all study sites
allNWR <- c(ANWR, KANWR,  INWR, KONWR, NNWR, SNWR, TNWR)


###ui###########################################################################################################################################################################
ui <- dashboardPage(
  
  #create header
  
  dashboardHeader(
    titleWidth = 300,
    title = ""
    ),  
  
###create sidebar################################################################################################################################################################       
#blue sidebar with options to filter data
  
  dashboardSidebar(
    width = 300,

    
    
    #add sidebar features
    #dropdown checkbox using shinyWidget
    pickerInput(
      inputId = "sitesInput",
      label = h3("ALMS Blocks"), 
      choices = list( "Arctic NWR" = ANWR,
                      "Kanuti NWR" = KANWR,
                      "Innoko NWR" = INWR,
                      "Koyukuk NWR" = KONWR,
                      "Nowitna NWR" = NNWR,
                      "Selawik NWR" = SNWR,
                      "Tetlin NWR" = TNWR),
      option = pickerOptions(actionsBox = TRUE, 
                             selectedTextFormat = "count > 1",
                             size = 11,
                             dropupAuto = FALSE,
                             liveSearch = TRUE,
                             noneSelectedText = "No blocks selected"),
      #format checkbox text to be larger and black               
      choicesOpt = list(style = rep_len("font-size: 110%; 
                                        color: black;
                                        line-height: 1.6;", 
                                        40)),
      #allow multiple selections in checkbox
      multiple = TRUE),
    
    #conservation list picker
    pickerInput(
      inputId = "conservInput",
      label = h3("Conservation Status"),
      choices= list("All Species" = 0, 
                    "USFWS Birds of Conservation Concern" = 1, 
                    "USFWS Endangered Species" = 5, 
                    "USFWS Threatened Species" = 6, 
                    "DoD Mission-Sensitive Priority Species" = 2, 
                    "ADFG Species of Greatest Conservation Need" = 3,
                    "Boreal Partners in Flight Species of Concern" = 4),
      choicesOpt = list(style = rep_len("font-size: 110%;
                                        color: black;
                                        line-height: 1.6;",
                                        7)),
      #have "All species" be selected first
      select = 0),
    
    #species picker filtered by site and conservation list
    pickerInput(
      inputId = "speciesInput",
      label = h3("Species"),
      choices = "placeholder",
      multiple = TRUE,
      option = pickerOptions(actionsBox = TRUE, 
                             selectedTextFormat = "count > 1",
                             liveSearch = TRUE,
                             size = 12,
                             noneSelectedText = "No species selected")),
    
    #slider bay for year input
    sliderInput(
      inputId = "yearInput",
      label = h3("Years of Study"),
      min = 2015,
      max = 2020,
      #2 values allow 2 way slider
      value = c(2015, 2020),
      sep = "",
      width = "350px")),
  
  ###create body################################################################################################################################################################       
  #body ui for server functions to be displayed in
  
  dashboardBody(
    tags$head(tags$style(HTML(
      
      ".mycluster1 {
      border-radius: 20px;
      width: 10px;
      height: 10px;
      background-color: #1e90ff;
      text-align: center;
      font-size: 10px;
      }
      .mycluster2 {
      border-radius: 20px;
      width: 10px;
      height: 10px;
      background-color: #F5F5F5;
      text-align: center;
      font-size: 10px;
      padding-top: 1px;
      padding-left: .7px;
      box-sizing: border-box;
      }"))),
        
        
    #create top row tabs        

      #tab 2 species point map##################################
      
      tabPanel(title = h4("Map"),
               
               
               #place for rendered map
               leafletOutput(outputId = "map2", height = "92vh"),
               
               #make sidepanel partially transparent
               div(style = 
                     "opacity: 0.85;",
                   
                   #create sidepanel to filter species point map
                   absolutePanel(id = "controls", 
                                 class = "panel panel-default", 
                                 fixed = FALSE,
                                 draggable = FALSE, 
                                 top = 71, 
                                 left = "auto", 
                                 right = 20, 
                                 bottom = "auto",
                                 width = 225, 
                                 height = 280,
                                
                                 #add plot to sidebar for frequency of observations by year
                                 div(style=" padding-top:4px;",
                                 plotOutput(outputId = "speciesScatter", 
                                            height = 220, 
                                            width = 220)),
                                 
                                 #add additional checkbox to aggregate observations       
                                 div(style="padding: 14px; padding-top:5px;",
                                     awesomeCheckbox(inputId = "agg.obs",
                                                     label = "Aggregate Observations",
                                                     value = FALSE))
                   )))))

### Server######################################################################################################################################################################
server <- function(input,output,session){
  
  #reactive list that creates lists species of conservation concern   
  concernlist <- reactive({
    sp.list = c()
    if(input$conservInput == 1){sp.list = BCC}
    if(input$conservInput == 2){sp.list = DOD}
    if(input$conservInput == 3){sp.list = ADFG}
    if(input$conservInput == 4){sp.list = PIF}
    if(input$conservInput == 5){sp.list = FWSE}
    if(input$conservInput == 6){sp.list = FWST}
    
    return(sp.list)})
  
#reactive filtering df
obs <- reactive({
  
  if(input$agg.obs == TRUE & length(input$sitesInput) != 0 & length(input$speciesInput) != 0)
  {aggtemp <- aggregate(common_name ~  Latitude + Longitude + species_count + block_point,
                        data = unique({gps.obs %>% filter(
                                        block %in% input$sitesInput,
                                        common_name %in% input$speciesInput,
                                        Year >= input$yearInput[1],
                                        Year <= input$yearInput[2])}[c("Latitude", "Longitude", "common_name", "species_count", "block_point")]),
                        toString)
  
  for (i in 1:length(aggtemp$Latitude))
    
  {aggtemp$species_count[i] <- length(trimws(unlist(strsplit(aggtemp$common_name[i], split = ",")), which = "both"))}
  
  return(aggtemp)}
  
  else
    
  {gps.obs %>% filter(
                block %in% input$sitesInput,
                Year >= input$yearInput[1],
                Year <= input$yearInput[2],
                common_name %in% input$speciesInput)}
  
})  

obs.graph <- reactive ({
  gps.obs %>% filter(
    block %in% input$sitesInput,
    Year >= input$yearInput[1],
    Year <= input$yearInput[2],
    common_name %in% input$speciesInput)
  
})
  
max.agg <- reactive({
  
  if(input$agg.obs == TRUE & length(input$sitesInput) != 0 & length(input$speciesInput) != 0)
  {aggtemp <- aggregate(common_name ~  Latitude + Longitude + species_count + block_point,
                        data = unique({gps.obs %>% filter(
                          block %in% input$sitesInput,
                          common_name %in% input$speciesInput,
                          Year >= input$yearInput[1],
                          Year <= input$yearInput[2])}[c("Latitude", "Longitude", "common_name", "species_count", "block_point")]),
                        toString)
  
  for (i in 1:length(aggtemp$lat))
    
  {aggtemp$species_count[i] <- length(trimws(unlist(strsplit(aggtemp$common_name[i], split = ",")), which = "both"))}
  
  return(max(aggtemp$species_count))}
})

#create negative data that does not have duplicate points
obs.negative <- reactive({ 
  
  gps.obs %>% filter(                 
    block %in% input$sitesInput,
   Year >= input$yearInput[1],
   Year <= input$yearInput[2]
   )   
  
  })

unique.negative <- reactive({
  
  obs.negative()[!duplicated(obs.negative()$block_point),]   
  
  })

#filter by species in concernlist or not by concernlist
concern.obs <- reactive({ 
  
  gps.obs %>% filter(                 
    block %in% input$sitesInput,
    Year >= input$yearInput[1],
    Year <= input$yearInput[2],
    common_name %in% concernlist()
    )
  
})

all.obs <- reactive({ 
  
  gps.obs %>% filter(                 
    block %in% input$sitesInput,
    Year >= input$yearInput[1],
    Year <= input$yearInput[2]
    )

})

#species selector
spilist <- reactive({
  
  if(any(input$sitesInput %in% allNWR & input$conservInput == 0))
    
                   {as.data.frame(table(unique(all.obs()$common_name)))} 
  
  else
      
                   {as.data.frame(table(unique(concern.obs()$common_name)))}
  
})

#update the species picker using the reactive list spilist()
observe({ 
  
  updatePickerInput(session,
                    "speciesInput",
                    choicesOpt = list(style = rep_len("font-size: 110%; 
                                                        color: black;
                                                        line-height: 1.6;",
                                                      300)),
                   choices = as.character(spilist()[,1])) 
  
  })    
  
 agg.color <- colorNumeric(palette = "YlOrRd", domain = NULL)
  
y2016 <- reactive(length(unique(filter(obs.graph(), Year == 2016)$block_point))/length(unique(filter(obs.negative(), Year == 2016)$block_point)))
y2017 <- reactive(length(unique(filter(obs.graph(), Year == 2017)$block_point))/length(unique(filter(obs.negative(), Year == 2017)$block_point)))
y2018 <- reactive(length(unique(filter(obs.graph(), Year == 2018)$block_point))/length(unique(filter(obs.negative(), Year == 2018)$block_point)))
y2019 <- reactive(length(unique(filter(obs.graph(), Year == 2019)$block_point))/length(unique(filter(obs.negative(), Year == 2019)$block_point)))

freq <- reactive (c(y2016(), y2017(), y2018(), y2019()))
year <- c(2016, 2017, 2018, 2019)

freq.graph <- reactive (as.data.frame(cbind(year,freq())))

  #render the species point map in tab  
  output$map2 <- renderLeaflet({
    
    leaflet() %>%
      setView(lat =  64.204125,
              lng = -155.872777,
              zoom = 3) %>%

      addProviderTiles("Esri.WorldImagery",
                       options = providerTileOptions(minZoom = 4)) %>%
      addCircleMarkers(data = unique.negative(),
                       lng = unique.negative()$Longitude,
                       lat = unique.negative()$Latitude,
                       fillColor = "lightgray",
                       fillOpacity = .45,
                       radius = 5,
                       stroke = FALSE) %>%
      addCircleMarkers(data = obs(),
                       lng = obs()$Longitude,
                       lat = obs()$Latitude,
                       stroke = FALSE, 
                       fillOpacity = 1,
                       radius = 7,
                       color = if(input$agg.obs == TRUE & length(input$speciesInput) != 0) 
                       {~agg.color(obs()$species_count)},
                       popup = if(input$agg.obs == TRUE & length(input$speciesInput) != 0)
                       {paste("<b>Species List:</b>   ",obs()$common_name,"<br>",
                              "<b>Number of Species:</b>", obs()$species_count, "<br>",
                              "<b>Latitude:    </b>", obs()$Latitude,"<br>",
                              "<b>Longitude:    </b>", obs()$Longitude, "<br>",
                              "<b>Point:    </b>", obs()$block_point)}
                       
                       else
                       {paste("<b>Common Name:</b>   ",obs()$common_name, "<br>",
                              "<b># of Individuals:    </b>", obs()$No_gt_1, "<br>",
                              "<b>Observation Date:</b>    ", obs()$Date, "<br>",
                              "<b>Latitude:    </b>", obs()$Latitude, "<br>",
                              "<b>Longitude:    </b>", obs()$Longitude, "<br>",
                              "<b>Point:    </b>", obs()$block_point)},
                       clusterOptions = markerClusterOptions(freezeAtZoom = 15, 
                                                             spiderfyDistanceMultiplier = 2, 
                                                             iconCreateFunction=JS("function (cluster) {    
                                                                                   var markers = cluster.getAllChildMarkers(); 
                                                                                   var n = 0;
                                                                                   for (var i = 0; i < markers.length; i++) {
                                                                                   n += markers[i].number;
                                                                                   }
                                                                                   var small = n >0;
                                                                                   var className = small ? 'mycluster1' : 'mycluster2';
                                                                                   var size = small ? 17 : 17;
                                                                                   return L.divIcon({ html: '<b>' + cluster.getChildCount() + '</b>', className: className, iconSize: L.point(size, size) });}"))) %>%
      addScaleBar(position = "bottomright") %>%
      addMeasure(position = "topleft")                             })
  
output$speciesScatter <- renderPlot({
    
    if(length(obs()$Latitude) == 0)
      
    {ggplot(data.frame()) + 
        geom_point() + 
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text = element_text(color = "black"), 
              axis.title.x = element_text(size = 9),
              axis.title.y = element_text(size = 9)) +
        labs(x = "Year", y = "Proportion of Points Detected") +
        xlim(2016,2019) + 
        ylim(0,1)}
    
    else
      
    {ggplot(freq.graph(),
            aes(y = freq(),
                x = year)) + 
        geom_point(colour='red') + 
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text = element_text(color = "black"), 
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12)) +
        labs(x = "Year", y = "Proportion of Points Detected") +
        xlim(2016,2019) + 
        ylim(0,1)}   })
  
  

}
#compile ui and server
shinyApp(ui = ui, server = server)

