###11/23/2021 Ben Choat
#This is an R Shiny app that I am starting based on my work for Alex Funk at CWCB.
#It provides pre- and post-processing for the COMET-Planner tool within the
#South Platte River Basin (SPRB)
#Specifically, you can select an area in the SPRB, eventually by uploading a
#shape file or dragging on the map, but for now, just by selection 1 of 3 locations
#Then select the owner/manager of the land you are interested in (e.g., private)
#A map of irrigated land with that owner/manager is returned along with 
#the total acerage, which is an input to the COMET-Planner tool

#For post processing you can enter the sequestration rate and the time over
#which you want to return a ROI
#Select values or distribution to represent the social cost of carbon (SCC)
#Select a value for Discount and Time preference rates


############# NOTE'S TO SELF ----
# is feddata used currently?
# tidyr?
# scales?
#
############# NOTE'S TO SELF^




# Load libraries ----


#if only base packages are loaded then restore renv
if(length((.packages())) < 8) {
#renv package to help manage package versions across machines
#if(!require(renv)) {install.packages("renv")}
#options for the renv package
renv::restore() #update libraries that are stored
#renv::init() #initiate new project
#renv::snapshot() #store active libraries in locked env.
}

#Pacman to manage packages
if(!require(pacman)) {
  install.packages("pacman")
}


# data manipulation
#purrr for mapping functions
#rstudioapi for getting directory in which this code is stored
pacman::p_load(data.table,  dplyr, purrr, rstudioapi, update = FALSE)#tidyr,

#spatial analysis
#sf: vectors and such
#stars: rasters, datacubes, and such
#terra: rasters
#devtools::install_github("ropensci/USAboundariesData")
#devtools::install_github("isciences/exactextractr")

pacman::p_load(sf, terra, USAboundaries, update = FALSE)

#visualization
#ggplot: standard plots
#tmap: spatial mapping
#leaflet: interactive maps
pacman::p_load(ggplot2, patchwork, plotly, tmap, ggdist, RColorBrewer, leaflet, update = FALSE)

# Shiny libs
# DT for java DataTables
# shinyssloaders for loading icon
# shinythemes for extra theme options
pacman::p_load(DT, shinycssloaders, shiny, shinythemes, update = FALSE)




# Load data to keep in memory during session----
# and define variables for page creation

#Store working dir. in case want to reload


#####Define new working dir as location this file is saved
temp.wd <- rstudioapi::getSourceEditorContext()$path
temp.wd <- substring(temp.wd, 1, nchar(temp.wd) - nchar("app.R"))


#######DELETE ----
# download.file(paste0(temp.wd, "/ThorntonNorthProps.zip"), destfile = "thrntn.zip")
# dwnld.file <- unzip(paste0(temp.wd, "/ThorntonNorthProps.zip"))
# unzip(paste0(temp.wd, "/ThorntonNorthProps.zip"))
# list.files(paste0(temp.wd, "/ThorntonNorthProps"))
# 
# layer.in <- list.files()
# 
# sp.test <- sf::st_read(
#   dsn = dwnld.file,
#   layer = "tnp_farms")
##########DELETE


#Delete later ...
#temp.wd <- "J:/DataWorking/CWCB_ESLC/"

#Define names for tabs
tab0.name <- "Introduction"
tab1.name <- "1. Area of Interest" 
tab2.name <- "2. COMET-Planner"
tab3.name <- "3. Valuation"


##### LOAD SPATIAL DATA ----

##CO DSS River Basins
#sp.sprb <- sf::st_read(
#  dsn = paste0(temp.wd, "CoDSS/All_River_Basins"),
#  layer = "SPRB.bndry"
#)

#Irrigated Lands
#st_layers(dsn = paste0(temp.wd, "CoDSS/Div1_Irrig_2015/Div1_Irrig"))
if(!exists("sp.Irrig")){
  sp.Irrig <- sf::st_read(
    dsn = paste0(temp.wd, "Div1_Irrig"),#"CoDSS/Div1_Irrig_2015/Div1_Irrig"),
    layer = "Div1_Irrig_2015")
}

#COMAP land ownership and/or management
if(!exists("sp.land.mo")) {
  sp.land.mo <- sf::st_read(dsn = paste0(temp.wd, "COMAP/Land.O.M.sprb.shp"))
}

##### Define functions and variables

# Define function to calculate ROI or value from sequestration
ROI.fun <- function(sq = sq.rt, yrs = yrs.in, scc = scc[["MiddleRoad"]], 
                    dsc = discount.rt[["MiddleRoad"]], tp = time.pr[["MiddleRoad"]]) {
  # ROI in [$]
  # sq = sequestration rate (e.g., from COMET-planner) [tCO2eq/year]
  # yrs = number of years over which to sum value [years]
  # scc = value of SCC (damage avoided from next ton of carbon) [$/tCO2eq]
  # dsc = discount rate [%]
  # tp = time preference [%]
  
  #Create vector of years
  yrs.in <- seq(0, yrs, 1)
  #Apply function
  scc * sq * sum(sapply(yrs.in, function(t) {((1 + dsc/100)^(-t) * (1 + tp/100)^(-t))}))
}

# Define vars related to discounting
#read in summary table of social cost of carbon, discount rate, and time preference 
dscnt.tbl <- read.csv(paste0(temp.wd, "DscntngVlues.csv"))

#Define vectors of values
#Social cost of carbon
#conservative meaning low-end estimates of ROI and the SCC
scc <- c("VeryConservative" = 12,
         "Conservative" = 21,
         "MiddleRoad" = 62,
         "Liberal" = 100,
         "VeryLiberal" = 300)

#Discount rate
discount.rt <- c("VeryConservative" = 7,
                 "Conservative" = 5,
                 "MiddleRoad" = 3,
                 "Liberal" = 2.5,
                 "VeryLiberal" = 1)
#time preference
time.pr <- c("Conservative" = 4.4,
             "MiddleRoad" = 2,
             "Liberal" = 1)





####
#User Interface, ui ----
####
ui <- fillPage(theme = shinytheme("cerulean"), #shinythemes::themeSelector(), #theme = shinytheme("cyborg"),
        navbarPage(title = "Pre - Post - COMET",
             tabPanel(tab0.name, #introduction
                      fluidRow(
                        column(width = 12, #offset = 0.5,
                               h4("Introduction"),
                               p("This app is intended to be used with the", 
                               a("COMET-Planner Tool,", href = "http://comet-planner.com/"), 
                               "which can be used to investigate how CO2 sequestration and 
                               GHG production will change as the use of working and natural lands changes. 
                               The various practices that can be compared within the tool are based on", 
                               a("NRCS Conservation Practices.", 
                               href = "https://planner-prod2-dot-comet-201514.appspot.com/static/media/NRCS_RankingTools.87706528.pdf")
                               ),
                               br(),
                               p("The inputs required in the COMET-Planner Tool are the county in which the conservation
                                 practice will be implemented, the type of practice being assessed (options provided within 
                                 the COMET-Planner Tool), and the acreage of interest."),
                               br(),
                               p("This app, aims to assist the user in identifying the acrege of interest, within the South Platte 
                                 River Basin of Colorado. Currently, it simply returns the acres of irrigated agriculture 
                                 within an area of interst (from a shape file) and the owner (e.g., 'Private', 'City', 'State')."),
                               br(),
                               p("Possible future functionality includes filtering by non-irrigated land, irrigated land, or both, 
                                 filtering by highly disturbed land, by riparian area, the ability to upload 
                                 a zipped shape file, and more."),
                               br(),
                               p("Currently, three areas of interest are already uploaded so that you can try the tool. To upload your
                                 own area of interest, you need to upload each of the files that is associated with an esri shape (.shp)
                                 file (i.e., .cpg, .dbf, .prj, .sbn, .sbx, .shp)"),
                               br(),
                               h4("Instructions; 1 => 2 => 3"),
                               h5("1. Area of Interest"),
                               p("Here, you can select from the three pre-loaded areas of interset or load your own shape file 
                                 (i.e., .cpg, .dbf, .prj, .sbn, .sbx, .shp)."),
                               br(),
                               h5("2. COMET-Planner Tool"), 
                               p("Follow the link in this tab to open the COMET-Planner Tool. The table presented in this tab 
                                 will provide the acreage of the relevant areas of interest (also seen in the legend of the map
                                 on the first tab). You will use this area as an input in the COMET-Planner Tool. Follow the instructions 
                                 provided are the linked site."),
                               br(),
                               h5("3. Valuation"),
                               p("COMET-Planner Tool will provide outputs of CO2eq/yr. Use those values as inputs for the 'sequestration
                                 rate(s)' input. The other inputs are at reasonable defaults, but you can adjust them as you see appropriate.
                                 When you edit any of the inputs the plot will update automatically, providing a range of possible return 
                                 on investments from the GHG reductions associated with the selected conservation practices.")
                               
                                 
                        ) #close column
                      ) #close fluidrow
             ), #close tabPanel 
             tabPanel(tab1.name, # AOI
                      sidebarLayout(
                        sidebarPanel(tags$h4('Options'), position = "left", width = 4,
                         #Select input options
                         radioButtons(inputId = "shpmth", label = "Use 3 existing AOIs or load your own .shp?", 
                                            choices = c("3 AOIs", "Load Own Files (i.e., .cpg, .dbf, .prj, .sbn, .sbx, .shp)")),
                         #Select if want irrigated land, unirrigated land, or both
                         
                         #Dropdown for 3 existing AOIs
                         conditionalPanel(condition = "input.shpmth == '3 AOIs'",
                            selectInput("AOI.in", "Area Of Interest",
                                     c("ThorntonNorthProps", "Brighton_SPRCorridor", "Greeley")
                                     )
                            ),
                         # Read in shape file
                         conditionalPanel(condition = "input.shpmth == 'Load Own Files (i.e., .cpg, .dbf, .prj, .sbn, .sbx, .shp)'",
                           fileInput("AOIbndry", "Load a .shp file", 
                                     #accept = ('.zip'))
                                     accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj", ".zip"), multiple=TRUE),
                           actionButton("Map.go",
                                        label = "Process zipped .shp file")
                         #)   
                           ), #close conditional panel
                         conditionalPanel(condition = "output.fileUploaded",
                                          )
                         ),# close sidebar
                        # tableOutput("AOI.tble")),
                        # #textOutput("AOI.tble")),
                        mainPanel(
                          #output plot
                            leafletOutput("inputMap", height = 500) %>%
                              withSpinner(color="#0dc5c1"),
                            #tableOutput(filedf)
                                  ) # close main panel
                                ) # close sidebar layout
                          ), #close tabPanel 
                      
             tabPanel(tab2.name,
                      fluidRow(
                        column(width = 10, offset = 1,
                          p("Now that you have determined the acreage of the irrigated land that you are interested in 
                          assessing the ROI of carbon sequestration and/or GHG mitigation for, you can go to the", 
                          a("COMET-Planner website", href = "http://comet-planner.com/"), "to get estimates of the total CO2eq of
                          sequestration/mitigation."),
                          br(),
                          p("The CO2eq outputs from the COMET-Planner tool are used as inputs in the next tab (3. Valuation)."),
                          br()
                        ) #close column
                      ), #close fluidrow
                      fluidRow(
                        column(tags$h5('Irrigated Acres by Owner'), align = 'center', width = 8, offset = 2,
                           
                          DT::dataTableOutput("Acres.Own")
                               ) #close column
                              ) #close fluidrow
                        ), #close tabpanel,
              tabPanel(tab3.name,
                       fluidRow( #Change to fluid row and columns layout
                         column(tags$h4('Social Cost of Carbon (SCC)'), align = c("center"), width = 4,
                                #Create drop down to select which SCM classification to use
                                #in the analysis
                                selectInput("distrin", "Assumed Distribution of SCC",
                                            c('Uniform', 'Normal', 'Lognormal')
                                ),
                                conditionalPanel(
                                  condition = "input.distrin == 'Uniform'", 
                                  textInput("min.un", "Minimum SCC $", value = "0"),
                                  textInput("max.un", "Maximum SCC $", value = "300")
                                ), #close conditional panel
                                conditionalPanel(
                                  condition = "input.distrin == 'Normal' || input.distrin == 'Lognormal'",
                                  textInput("mean.norm", "Mean SCC $", value = "60"),
                                  textInput("sd.norm", "St.Dev. SCC $", value = "30")
                                ), #close  conditional panel
                                actionButton(
                                  inputId = "SCC.go",
                                  label = "1. Simulate SCC"
                                ) #close action button
                                
                         ), #close column
                         column(tags$h4('Sequestration'), align = c("center"), width = 4,
                                # Enter sequestration rates of interest as a 
                                # comma seperate string (e.g., 1, 2, 3)       
                                textInput("seq.rt.in", "Sequestration Rates From COMET-Planner - (tCO2eq/yr) (comma separated)",
                                          value = "7833, 11522, 5377"),
                                
                                #Enter how many years to consider in analysis
                                #ROI can be returned for any of the years from 0
                                #to the end year entered.
                                textInput("yrs.in", "How many years into the future?",
                                          value = "20")
                                ), #Close column
                          column(tags$h4('Valuation'), align = c("center"), width = 4,
                                 #Enter how many years to consider in analysis
                                 textInput("dscnt.in", "Discount Rate(s) as % (comma separated)",
                                           value = "1, 2.5, 5, 7"),

                                 #Enter how many years to consider in analysis
                                 textInput("timepr.in", "Time Preference(s) as % (comma separated)",
                                           value = "0, 2, 4.4"
                                          ), #Close textInput
                                 actionButton(
                                   inputId = "ROI.go",
                                   label = "2. Estimate ROI"
                                    ) #close action button
                                 ) #Close column
                                ), #Close fluidRow
                        fluidRow(tags$h4('ROI Estimates'), align = "center",
                           column(width = 4, #offset = 1,
                           # textOutput("roi.results")
                            #DT::dataTableOutput("roi.results")
                            # column(width = 3,
                                    #DT::dataTableOutput("table.out")
                                  # DT::dataTableOutput("roi.results") %>% 
                                  #   withSpinner(color="#0dc5c1")
                                    plotlyOutput("roi.sccs") %>% 
                                    withSpinner(color="#0dc5c1")
                            #        )
                                  ), #close column
                                #), #close fluidrow
                      # fluidRow(tags$h4('Social Cost of Carbon'), align = "center",
                                column(width = 8, #offset = 1, 
                                       plotlyOutput("roi.results")%>% 
                                         withSpinner(color = "#0dc5c1")
                                       ) # close column
                                ) # close fluidRow
                      ) #close tabPanel
                  ) #close navbarPage
        ) #close fillPage








####
#server ----
####

server <- function(input, output, session) {

  #In Shiny app have user input .shp file of their AOI.
  #Areas of interest (Thornton norther properties, Brighton SPR corridor, Greeley long range growth area)
  
  
  # Define variables to be available throughout server environment ----
  
  ###Geoprocessing: Irrigated area by owner/manager
  #area of interest as sf polygon
  AOI <- reactive({
    if(input$shpmth == "3 AOIs") {
    
      #Input options for now are "ThrontonNorthProps", "Brightin_SPRCorridor", "Greeley"
      #define layer names based on input
      layer.in <- #reactive({
        if(input$AOI.in == "ThorntonNorthProps") {
          "tnp_farmsZ13N"  
        } else if(input$AOI.in == "Brighton_SPRCorridor") {
          "South_Platte_Heritage_Corridor"
        } else if(input$AOI.in == "Greeley") {
          "Greeley_LongRangeExpctdGrwthArea"
        }
      #})
      
      sf::st_make_valid(st_read(
      dsn = paste0(temp.wd, input$AOI.in),#"CWCB_Data/", input$AOI.in),
      layer = layer.in#() 
      ) %>% 
    sf::st_transform("+proj=utm +zone=13 +datum=NAD83 +no_defs"))
    } else if (input$shpmth == "Load Own Files (i.e., .cpg, .dbf, .prj, .sbn, .sbx, .shp)") {
      #"AOIbndry"
      
      #Input options for now are "ThrontonNorthProps", "Brighton_SPRCorridor", "Greeley"
      #define layer names based on input
      # layer.in <- #reactive({
      #   input$AOIstrng

      
      # Load zipped shape file when Map.go button is clicked
      # following method shown here: 
      # https://stackoverflow.com/questions/48261595/uploading-zip-with-multiple-files-into-a-shiny-application
      observeEvent(input$Map.go, {
        files <- unzip(input$AOIbndry$datapath, list = TRUE)
        
        unzip_shp <- function(x) {
          
        }
      })
      
    #shpdf <- unzip(input$AOIbndry$datapath, exdir = getwd())#list = TRUE, 
    #shpdf <- dir(input$AOIbndry)
    shpdf <- input$AOIbndry
    if(is.null(shpdf)){
      return()
    }
    
    previouswd <- getwd()
    uploaddirectory <- dirname(shpdf$datapath[1])
    setwd(uploaddirectory)
    for(i in 1:nrow(shpdf)){
      file.rename(shpdf$datapath[i], shpdf$name[i])
    }
   setwd(previouswd)
    dsn.in <- paste(uploaddirectory, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/")

    sf::st_make_valid(st_read(
      dsn = dsn.in,
      #layer = layer.in
    ) %>% 
      sf::st_transform("+proj=utm +zone=13 +datum=NAD83 +no_defs")
    ) #closing make valid
    }
  }) #closing reactive
  ###
  
  
  aoi.irrig <- reactive({
    sf::st_intersection(AOI(), sp.Irrig)
  })
  
  aoi.irrig.om <- reactive({
    sf::st_intersection(aoi.irrig(), sp.land.mo)
  })
  
  #define dataframe to work with
  area.sumz <- reactive({
    data.frame("Owner" = aoi.irrig.om()[["OWNER"]],
                                "area" = sf::st_area(aoi.irrig.om())) %>% 
  #area.sumz <-
    #aoi.irrig.om.in %>%
    dplyr::group_by(Owner) %>%
    dplyr::summarise(Acres = round(sum(as.numeric(area))/4046.86, 2))
  })
  
  

  
  
  # Table to display in the second tab, showing the land owners and acres of land owned by them
  output$Acres.Own <- DT::renderDataTable(
    #       # DT::datatable(data.frame("Discount.Rate.Percent" = dscnt,
    #       #                          "Time.Preference.Percent" = timepr,
    #       #                          "Sequestration.Rate.CO2.yr" = seq.rt,
    #       #                          "Year" = yrs.in)) %>%
    #       #   DT::formatStyle(c('Discount.Rate.Percent', 'Time.Preference.Percent',
    #       #                     'Sequestration.Rate.CO2.yr', 'Year'),
    #       #                   color = 'black')
    #         #formatStyle('Sepal.Length',  color = 'red', backgroundColor = 'orange', fontWeight = 'bold')
    #         #backgroundColor = styleEqual(c(0, 1), c('gray', 'yellow'))
    DT::datatable(
      data.frame(area.sumz())
      ) %>%  
        DT::formatStyle(c('Owner', 'Acres'), color = 'black')
    ) #close renderDataTable
  
 # DT::formatStyle(c('Discount.Rate.Percent', 'Time.Preference.Percent',
                    #       #                     'Sequestration.Rate.CO2.yr', 'Year'),
                    #       #                   color = 'black')
# Table for troubleshooting unzipping shape files
  output$AOI.tble <- renderTable(
    #shpdf <- unzip(input$AOIbndry$datapath, exdir = getwd())#list = TRUE, 
    #shpdf <- dir(input$AOIbndry)
    #shpdf <- input$AOIbndry
    #input$AOIbndry$datapath
    #dir(input$AOIbndry)
    input$AOIbndry
  )
 # end trouble shooting table
  ###
  #MAP

  output$inputMap <- renderLeaflet({
    
    # #define dataframe to work with
    # aoi.irrig.om.in <- data.frame("OWNER" = aoi.irrig.om()[["OWNER"]],
    #                               "area" = sf::st_area(aoi.irrig.om()))
    # 
    # area.sumz <-
    #   aoi.irrig.om.in %>%
    #   dplyr::group_by(OWNER) %>%
    #   dplyr::summarise(Area = sum(as.numeric(area))/4046.86)
    
    #tmap object
   
    wndw <- st_bbox(AOI())
    w.tmp <- wndw[3] - wndw[1] 
    l.tmp <- wndw[4] - wndw[2]
    
    lbls.tmp <- paste0(sort(unique(area.sumz()$Owner)), " ", round(area.sumz()$Acres, 1), " acres")
    lngth.tmp <- length(unique(area.sumz()$Owner))
    set.seed(731)
    clrs.tmp <- sample(palette("Polychrome 36"), lngth.tmp, replace = TRUE)
    
    #tmap_options(check.and.fix = TRUE)
    tmp.out <- tm_shape(st_as_sfc(wndw)) +
                  tm_polygons(alpha = 0, border.alpha = 0) +
                  
                tm_shape(AOI()) +
                  tm_polygons(border.col = "red", alpha = 0) +
      
                tm_shape(aoi.irrig.om()) + 
                  tm_fill(col = "OWNER",
                          palette = clrs.tmp,
                          legend.show = F, #) +  
                          popup.vars = c("Owner" = "OWNER", "Acres" = "ACRES"), 
                          popup.format = list(ACRES = list(digits = 2))) +
                  tm_borders(col = "black",
                             #alpha = 0.5,
                             lwd = 0.2) +
                tm_add_legend(type = "fill", labels = lbls.tmp,
                                col = clrs.tmp, title = "Owner") +
                tm_layout(legend.outside = TRUE, 
                            #legend.position = if(w.tmp>l.tmp) {c(0.4,0.3)
                            #}else{c(0, 0.4)}, 
                            legend.text.size = 0.9, 
                            legend.outside.position = if(w.tmp>l.tmp) {"bottom"
                            }else{"right"},
                            legend.stack = "horizontal") +
                  #tm_compass(position = c(-0.02, 0.12), text.size = 0.9) +
                  tm_scale_bar(position = c(0, 0), text.size = 0.9)
    
    #Convert to leaflet object
    tmap_leaflet(
      tmp.out,
      mode = "view",
      show = TRUE,
      #add.titles = TRUE,
      in.shiny = TRUE
      
    ) %>% 
      
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(opacity = 0.5)) #%>% 
      
      #removePopup(layerId = c(~OBJECTID))
      # addPolygons(
      #   popup = "OWNER"
      # )
      #addPopups(NULL)
        #tmp.out,
                # data = getMapData(tmp.out),
                # lng = ~long,
                # lat = ~lat,
                # popup = "OWNER")
  })
    
# Valuation module

  # only run after action button is clicked
  
  # random scc values
  scc.rndm <- eventReactive(input$SCC.go, {
    
    
    #Select random values using the above variables as inputs
    n.scc <- 300
    # n.dscnt <- 1
    # n.tpr <- 1
    mean.in = as.numeric(input$mean.norm)
    sd.in = as.numeric(input$sd.norm)
    
    if(input$distrin == "Uniform") {
      #scc.rndm <- 
      runif(n = n.scc, 
            min = as.numeric(input$min.un), 
            max = as.numeric(input$max.un))
    } else if(input$distrin == "Normal") {
      #scc.rndm <- 
      temp <- rnorm(n = n.scc, 
                    mean = mean.in,
                    sd = sd.in)
      temp[temp > 0]
      #temp
    } else if(input$distrin == 'Lognormal') {
      # scc.rndm <- 
      temp <- rlnorm(n = n.scc, 
                     meanlog = log(mean.in), 
                     sdlog = log(sd.in))
      temp[temp < 1000]
    }
  })

  dscnt <- eventReactive(input$ROI.go, {as.numeric(unlist(strsplit(input$dscnt.in,",|, | ")))})
  timepr <- eventReactive(input$ROI.go, {as.numeric(unlist(strsplit(input$timepr.in,",|, | ")))})
  seq.rt <- eventReactive(input$ROI.go, {as.numeric(unlist(strsplit(input$seq.rt.in,",|, | ")))})
  yrs.in <- eventReactive(input$ROI.go, {as.numeric(unlist(strsplit(input$yrs.in,",|, | ")))})
  
    # boxplots + violin plots
    output$roi.results <- renderPlotly({

      # Produce all combinations of scc, discount rate, and time preference
      # list of varaibles
      scc.in <- round(scc.rndm(), 2)
      dsc.in <- dscnt()#discount.rt.rndm
      tp.in <- timepr()#time.pr.rndm
      #
      l.tmp <- list(sq = seq.rt(), yrs = yrs.in(), #seq(0, yrs.in, 1),
                    scc = scc.in, dsc = dsc.in, tp = tp.in)
      vars.in <- expand.grid(l.tmp)
      ROI.out <- round(unlist(purrr::pmap(vars.in, .f = ROI.fun)), 2)
      ROI.df <- data.frame(vars.in, "ROI" = ROI.out)
      n.sims.out <- nrow(ROI.df)

      #Create a column that lists all of the varibles for each scenario
      #This is useful
      # ROI.df$Vars <- paste0(ROI.df$sq, "-",
      #                       ROI.df$scc, "-",
      #                       ROI.df$dsc, "-",
      #                       ROI.df$tp)
      #
      data.in <- ROI.df[ROI.df$yrs == yrs.in(),]
      data.in$ROI <- data.in$ROI/1000000

      # user input to group plots by
      group.in <-"sq"
      # Max value for ROI scale
      max.x <- max(data.in$ROI)
      
      
      # Plot
      p3 <- ggplot(data = data.in, 
                 aes(x = factor(.data[[group.in]]), y = ROI, fill = factor(.data[[group.in]]))) +
        
        geom_violin( #aes(color = factor(.data[[group.in]])),
                    alpha = 0.5,
                    draw_quantiles = TRUE
                    ) +
        
        geom_boxplot( #aes(color = factor(.data[[group.in]])),
                    width = 0.25,
                    #remove outliers
                    outlier.color = "gray",
                    alpha = 0.7
                    ) +
        
        geom_jitter(position = position_nudge(x = 0), alpha = 0.1, size = 0.1) +
        #adjust theme and labels
        scale_color_brewer(palette = "Dark2") + 
        theme_light() + 
        labs(#title = "ROI from Climate-Related Ecosystem Services",
          #subtitle = paste0("Grouped by ", group.in),
          y = paste0("ROI [million $] in today's value after ", yrs.in(), "\n years into the future"), 
          x = "Sequestration Rate [tCO2eq/yr]",#group.in), 
          title = paste0("\n\n (", n.sims.out, " total simulations)")
          #x = "Social Cost of Carbon [$/tCO2eq]",
          ) + #fill = "Sequestration Rate \n [tCO2eq/yr]") +
        #fill = "Social Cost of Carbon [$/tCO2eq]") +
        theme(legend.position = "none", axis.text = element_text(size = 12), 
              axis.text.x = element_text(angle = 45), plot.title = element_text(hjust = -0.5)) +
        scale_y_continuous(#breaks = seq(0, max.x, round(max.x/12)), #Commented these out because they were 
                           #labels = seq(0, max.x, round(max.x/12)), #causing errors with some input values                     
                           #minor_breaks = seq(0, max.x, round(max.x/24)),
                           limits = c(0, max.x)) +
        #annotate("text", x=3.8, y=100, label= "Brighton") +
        coord_flip()

      
      #ggplotly(scc.pl)
      
      ggplotly(p3, tooltip = "text")
      #ggplotly(gridExtra::grid.arrange(p3, scc.pl, ncol = 1), tooltip = "text")

    }) # close renderPlotly
    
    
    
    #scc plot
    output$roi.sccs <- renderPlotly({
      #scc.in <- round(scc.rndm, 2)
      scc.in <- round(scc.rndm(), 2)

      scc.pl <- ggplot() + 
        geom_histogram(aes(x = scc.in)) + 
        labs(#title = "SCC \n ($/ton CO2eq)",
             x = "SCC ($/ton CO2eq)", 
             y = "Counts") + 
        theme_light() +
        # geom_text(aes(x = Inf, 
        #               y = Inf, 
        #               label = paste0("n = ", length(scc.in)
        #               )
        #             )
        #           ) +
        theme(legend.position = "none", 
              axis.text = element_text(size = 12), 
              axis.text.x = element_text(angle = 45))#, 
              #plot.title = element_text(hjust = -0.3, size = 12)) #+
        #coord_flip()
      
      ggplotly(scc.pl, dynamicTicks = TRUE)
    })
  #})# close observe event
}
    

# # DataTable of combinations of input variables ----
###
#     output$roi.results <- DT::renderDataTable({
# 
#       dscnt <- as.numeric(unlist(strsplit(input$dscnt.in,",|, | ")))
#       timepr <- as.numeric(unlist(strsplit(input$timepr.in,",|, | ")))
#       seq.rt <- as.numeric(unlist(strsplit(input$seq.rt.in,",|, | ")))
#       yrs.in <- as.numeric(unlist(strsplit(input$yrs.in,",|, | ")))
#       # DT::datatable(data.frame("Discount.Rate.Percent" = dscnt,
#       #                          "Time.Preference.Percent" = timepr,
#       #                          "Sequestration.Rate.CO2.yr" = seq.rt,
#       #                          "Year" = yrs.in)) %>%
#       #   DT::formatStyle(c('Discount.Rate.Percent', 'Time.Preference.Percent',
#       #                     'Sequestration.Rate.CO2.yr', 'Year'),
#       #                   color = 'black')
#         #formatStyle('Sepal.Length',  color = 'red', backgroundColor = 'orange', fontWeight = 'bold')
#         #backgroundColor = styleEqual(c(0, 1), c('gray', 'yellow'))
# 
#       #Select random values using the above variables as inputs
#       n.scc <- 100
#       n.dscnt <- 1
#       n.tpr <- 1
#       if(input$distrin == "Uniform") {
#         scc.rndm <- runif(n.scc, min = scc[["VeryConservative"]], max = scc[["VeryLiberal"]])
#         #discount.rt.rndm <- runif(n.dscnt, min = discount.rt[["VeryLiberal"]], max = discount.rt[["VeryConservative"]])
#         #time.pr.rndm <- runif(n.tpr, min = time.pr[["Liberal"]], max = time.pr[["Conservative"]])
#       } else if(input$distrin == "def.increment"){
#         scc.rndm <- seq(scc[["VeryConservative"]], scc[["VeryLiberal"]], n.scc)
#         discount.rt.rndm <- seq(discount.rt[["VeryLiberal"]], discount.rt[["VeryConservative"]], n.dscnt)
#         time.pr.rndm <- seq(time.pr[["Liberal"]], time.pr[["Conservative"]], n.tpr)
#       }
# 
# #       #Produce all combinations of scc, discount rate, and time preference
# #       #list of varaibles
#       scc.in <- round(scc.rndm, 2)
#       dsc.in <- dscnt#discount.rt.rndm
#       tp.in <- timepr#time.pr.rndm
# #
#       l.tmp <- list(sq = seq.rt, yrs = yrs.in, #seq(0, yrs.in, 1), 
#                     scc = scc.in, dsc = dsc.in, tp = tp.in)
#       vars.in <- expand.grid(l.tmp)
#       ROI.out <- round(unlist(purrr::pmap(vars.in, .f = ROI.fun)), 2)
#       ROI.df <- data.frame(vars.in, "ROI" = ROI.out)
# 
#       #Create a columnn that lists all of the varibles for each scenario
#       #This is useful
#       # ROI.df$Vars <- paste0(ROI.df$sq, "-",
#       #                       ROI.df$scc, "-",
#       #                       ROI.df$dsc, "-",
#       #                       ROI.df$tp)
# #
# 
#       DT::datatable(ROI.df) %>%
#         DT::formatStyle(c(colnames(ROI.df)), color = 'black')
# 
#     })



# ###Table of irrigated acres owned by different parties in AOI
# output$table.out <- DT::renderDataTable({
# 
# aoi.irrig.om.in <- data.frame("OWNER" = aoi.irrig.om()[["OWNER"]],
#                               "area" = sf::st_area(aoi.irrig.om()))
#                           
# 
# area.sumz <- 
#   aoi.irrig.om.in %>%
#   dplyr::group_by(OWNER) %>%
#   dplyr::summarise(Area = sum(as.numeric(area))/4046.86)
# 
# 
# 
#   datatable(data.frame("Owner" = area.sumz$OWNER, "Acres" = round(as.numeric(area.sumz$Area), 1))) %>% 
#     DT::formatStyle(c('Owner', 'Acres'), color = 'black') #
#   #formatStyle('Sepal.Length',  color = 'red', backgroundColor = 'orange', fontWeight = 'bold')
#   #data.frame("t" = c(1,2,3), "q" = c("a","b","c"))
# })
###
#list of variables

    
  #SOME SAMPLE CODE  
#####
 #expl.in <- reactive({
    #  table.Expl[c("City", input$ExpVars)]
    #})
  
  
  ##Define SCMsys to be reactive input data
  #SCMsys.in <- reactive({
  #  get(paste0("table.", input$SCMsystem))
  #  })
  ##Define choice of SCM types as names of SCMsystem that was selected
  #SCMs.choices <- NULL
  #observeEvent(SCMsys.in(), {
  #  SCMs.choices <- names(SCMsys.in())[-1]
  #  updateSelectInput(session, "SCMs", choices = SCMs.choices) 
  #})
  #
  ##Define explanatory input var 
  #expl.in <- reactive({
  #  table.Expl[c("City", input$ExpVars)]
  #})
  ##update options of statistical methods based on numeric or factor expl.vars
  #method.choices <- NULL
  #observeEvent(expl.in(), {
  #  if(is.numeric(table.Expl[[input$ExpVars]])) {
  #    method.choices <- c("spearman", "pearson")
  #  } else if (length(levels(table.Expl[[input$ExpVars]])) == 2 |
  #             length(unique(table.Expl[[input$ExpVars]])) == 2) {
  #    method.choices <- c("wilcoxon rank-sum", "two-sample t-test")
  #  } else if (length(levels(table.Expl[[input$ExpVars]])) > 2 |
  #             length(unique(table.Expl[[input$ExpVars]])) > 2) {
  #    method.choices <- c("Kruskal-Wallis", "ANOVA")
  #  }
  #  updateSelectInput(session, "method", choices = method.choices)
  #})
  #
  #
  #output$test.text <- renderPrint({
  #  names(SCMsys.in())
  #  })
#####
  #SOME SAMPLE CODE
 

#####
#Execute
#####
shinyApp(ui, server)
