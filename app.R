###____________________________________###
### SHINY APP TO ASSESS HARD DOWNLOADS ###
###____________________________________###

## load necessary packages
source('code/Collar_Shiny_Packages.R')


source('code/Rename_Files.R')

####_______________________________________________________________
#### Rename and fiddle with files from Rename script ----

## rename files from Rename_Files.R
to.process <- to_process_correct
to.correct <- to_process_irregular

## pull out files that shouldn't be processed yet (VITs et al, CBC, and non-txt or csv files
to.process$Type <- NA
# isolate VITs, SEPs, MORTs, and PRXs
vitrows <- grepl('SEP|VIT|PRX|MORT', to.process$file.ext)

# remove .tag and .gdf files
weird <- to.process$ext == 'gdf'|to.process$ext == 'GDF'|to.process$ext == 'kml'|
  to.process$ext == 'tag'|to.process$ext == 'tdf'|to.process$ext == 'KML'|
  to.process$ext == 'TAG'|to.process$ext == 'TDF'

# isolate CBC files
cbc <- grepl('CBC', to.process$path)

# isolate files with 'unk' or 'UK' in animalID
unk <- grepl('UK|Unk', to.process$file.ext)

## assign 'process' or 'unnecessary' to table
to.process[!vitrows & !weird & !cbc & !unk,]$Type <- 'Process'
to.process[vitrows | weird | cbc | unk,]$Type <- 'Unnecessary'

to.process <- to.process %>% 
  select('newfile.ext', 'new.path', 'ext', 'Type') %>% 
  rename(`File Name` = newfile.ext,
         `File Path` = new.path,
         Ext = ext,
         `Should process?` = Type
  )

to.correct <- to.correct %>% 
  select('file.ext', 'path') %>%
  rename(`File Name` = file.ext,
         `File Path` = path
  )

####_________________________________________________________________
#### Read in capture and necropsy tables ----

# set up server connection
con <- dbConnect(
  odbc::odbc(),
  driver = "SQL Server",
  database = "IFWIS_WildlifeReporting",
  uid = "ShinyUserInternal",
  pwd = "hurt seven sat pupil",
  server = "164.165.105.241",
  port = "1433"
)
# read in capture table 
caps <- dbGetQuery(con, 'SELECT CaptureID, Animal_ID, Capture_Date, Biological_Year, Game, GMU, Radio_Frequency, Latitude, 
                                Longitude, Age_Class, Years_Old, Capture_Method, Sex, Weight,
                                CollarType, Chest_Girth, HindFootLength, BCS, CollarMake, CollarModel, Hairline,
                                StdAgeClass, Temperature, DateEntered
                             FROM VU_SAMM_CAPTURE')

caps$Capture_Date <- as.character(caps$Capture_Date)
caps$DateEntered <- as.character(caps$DateEntered)

# read in necropsy table
necs <- dbGetQuery(con, 'SELECT CaptureID, Animal_ID, GameID, FateID, FateDate, CensorID, CensorDate, RadFreq,
                                DateNec, Latitude, Longitude, GMU, MaxTemp, LastLoc, CausCert, NecID
                            FROM SAMM_NECROPSY')

necs$FateDate <- as.character(necs$FateDate)
necs$CensorDate <- as.character(necs$CensorDate)
necs$DateNec <- as.character(necs$DateNec)


# add 'Game' column in nec table to be actual animal instead of code
necs$Game <- NA
necs[!is.na(necs$GameID) & necs$GameID == 2,]$Game <- 'Black Bear'
necs[!is.na(necs$GameID) & necs$GameID == 3,]$Game <- 'Mountain Lion'
necs[!is.na(necs$GameID) & necs$GameID == 4,]$Game <- 'Elk'
necs[!is.na(necs$GameID) & necs$GameID == 5,]$Game <- 'Mule Deer'
necs[!is.na(necs$GameID) & necs$GameID == 6,]$Game <- 'White-tailed Deer'
necs[!is.na(necs$GameID) & necs$GameID == 7,]$Game <- 'Pronghorn Antelope'
necs[!is.na(necs$GameID) & necs$GameID == 8,]$Game <- 'Moose'
necs[!is.na(necs$GameID) & necs$GameID == 9,]$Game <- 'Rocky Mountain Bighorn Sheep'
necs[!is.na(necs$GameID) & necs$GameID == 10,]$Game <- 'California Bighorn Sheep'
necs[!is.na(necs$GameID) & necs$GameID == 11,]$Game <- 'Mountain Goat'
necs[!is.na(necs$GameID) & necs$GameID == 44,]$Game <- 'Wolf'
if(nrow(necs[!is.na(necs$GameID) & necs$GameID == 55,]) > 0) necs[!is.na(necs$GameID) & necs$GameID == 55,]$Game <- 'American White Pelican'

# add capture date to nec table
necs <- left_join(necs, caps[,c(1,3)])

# add capture locs to nec table
necs <- left_join(necs, caps[,c(1,8,9)], by = 'CaptureID')
# rename latitude and longitude
necs <- necs %>%
  rename(Longitude = Longitude.x,
         Latitude = Latitude.x,
         CaptureLongitude = Longitude.y,
         CaptureLatitude = Latitude.y)


# _functions to fingerprint with javascript #### **change**
# fingerprint_code
inputUserid <- function(inputId, value='') {
  #   print(paste(inputId, "=", value))
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "userid", value=as.character(value), type="text", style="display:none;")
  )
}
# fingerprint_code
inputIp <- function(inputId, value=''){
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "ipaddr", value=as.character(value), type="text", style="display:none;")
  )
}


####________________________________________________________________
#### 1. UI ----

## Start ui
ui <- fluidPage(
  
  
  ####________________________________________________
  #### 1.1 Appearance ----
  
  # set tab name **change**
  tags$head(HTML("<title>Collar and Capture Data Check</title>")),
  
  # set app theme
  theme = shinytheme('cerulean'),
  
  # set position for popup dialogs on page
  tags$style(
    type = 'text/css',
    '.modal-dialog {margin-top:15%}'
  ),
  
  # **change** send data to avoid timeout
  # tags$head(
  #   HTML(
  #     "
  #         <script>
  #         var socket_timeout_interval
  #         var n = 0
  #         $(document).on('shiny:connected', function(event) {
  #         socket_timeout_interval = setInterval(function(){
  #         Shiny.onInputChange('count', n++)
  #         }, 15000)
  #         });
  #         $(document).on('shiny:disconnected', function(event) {
  #         clearInterval(socket_timeout_interval)
  #         });
  #         </script>
  #         "
  #   )
  # ),
  
  # add 'thinking' spinner
  use_busy_spinner(spin = 'fading-circle', position = 'full-page', margins = c('300px', '300px')),
  
  ####_______________________________________________________________
  #### 1.1.2 Title Bar----
  # use navbarPage even though only one tab
  navbarPage(
    id = 'navbar-header',
    tags$style('#navbar-header {
                                           height: 120px;
                                          }'),
    inverse = T, 
    fluid = T,
    selected = 2,
    title = div(
      # add logos to top of page
      a(href = 'http://ifwisshiny.idfg.state.id.us:3838/MC/WildlifeResearchWebsite//',
        title = 'View the adventures of the Wildlife Research Team',
        img(
          src = 'WildlifeResearchPage_logo_small.png',
          height = '80px',
          style = 'position:relative; top:5px; bottom:15px; left:5px'
        )
      ), # img
      
      div(a(
        href = 'https://idfg.idaho.gov/',
        title = 'Idaho Fish and Game',
        img(
          src = 'idfglogo.png',
          height = '80px',
          style = 'position: absolute; top:20px; right:15px'
        ) # img
      )
      ), # div
      
      div(
        a(
          href = 'http://ifwisshiny.idfg.state.id.us:3838/EAR/IDFG_Wildlife_Shiny_Apps/',
          title = 'Return to Wildlife Shiny Applications',
          img(
            src = 'r-shiny-logo.png',
            height = '50px',
            style = 'position:absolute; left:89px; top:62px'
          )
        )
      )
      
    ), # title div
    tabPanel(
      value = 'tab2',
      title = h1('Collar and Capture Data Check', font = 'Calibri'),
      tags$style('h1 {
                                        color: #F5F5F5;
                                   ')
    )
  ), # navbarPage
  
  ####____________________________________________________________________
  #### 1.1.3 Password ----
  
  ## password input'
  conditionalPanel(
    condition = "input.password != 'knick knack paddy whack'",
    fluidPage(
      passwordInput("password", "Password:")
    )
  ),

  conditionalPanel(
    condition = "input.password == 'knick knack paddy whack'",
  
  
  
  ####_________________________________________________________
  #### 1.2 Sidebar Panel ----
  sidebarLayout(
    fluid = T,
    sidebarPanel(
      inputIp("ipid"),# fingerprint_code **change**
      inputUserid("fingerprint"),# fingerprint_code **change**
      ####_______________________________________________
      #### Action Buttons for checking/viewing files ----
      ## Header
      h4('Check Status of Hard Downloaded Files'),
      
      fluidRow(
        actionButton(inputId = 'processed', label = 'Processed files',
                     style = 'width:200px'),
        actionButton(inputId = 'ready', label = 'Ready to be processed',
                     style = 'width:200px'),
        actionButton(inputId = 'incorrect', label = 'Need to be reviewed',
                     style = 'width:200px')
      ), # fluidRow
      
      br(),
      
      # Action Button/header for sorting files
      h4(HTML('Sort files that need to be processed into one folder for easy viewing<br/>Note: This might take several minutes')),
      fluidRow(
        actionButton(inputId = 'sort', label = 'Sort Files',
                     style = 'width:200px')
      ),
      
      br(),
      
      h4('View last location for each collar'),
      fluidRow(
        actionButton(inputId = 'lastloc', label = 'View Last Locs',
                     style = 'width:200px')
      ), # fluidRow
      
      br(),
      
      # Action Button/Header for checking for errors in capture data
      h4('Check for Errors in Capture Data'),
      fluidRow(
        column(3, actionButton(inputId = 'capcheck', label = 'Check Capture Records',
                               style = 'width:200px')
        )
      ), # fluidRow
      
      fluidRow(
        column(1), 
        column(11, h5(strong('View Histograms for Potential Outliers:'))
        )
      ), # fluidRow
      
      fluidRow(
        column(1),
        # dropdown menus to view outlier histograms
        
        # Variable
        column(3,
               varSelectInput(
                 inputId = 'outliersVar', label = 'Variable to check:', data = caps %>% select(c('Chest_Girth', 'HindFootLength', 'Hairline'))
               )
        ),
        
        # Game
        column(3,
               selectizeInput(
                 inputId = 'outliersGame', label = 'Game:', choices = base::unique(caps$Game)
               )
        ),
        
        # Age_Class
        column(2,
               selectizeInput(
                 inputId = 'outliersAge', label = 'Age_Class:', choices = NULL
               )
        ),
        
        # View histograms
        column(2,
               actionButton(
                 inputId = 'viewoutliers', label = 'View Outliers', style = 'width:150px; position:relative; top:23px;'
               )
        )
        
      ), # fluidRow
      
      br(),
      
      # Action Button/Header for checking for errors in necropsy data
      h4(HTML('Check for Errors in Necropsy Data<br/>Note: This will take about a minute')),
      
      fluidRow(
        column(3, actionButton(inputId = 'neccheck', label = 'Check Necropsy Records',
                               style = 'width200px')#,
               # textOutput("keepAlive") # **change** goes with timeout code
        )
      ), # fluidRow
      
      width = 12
      
    ), # sidebarPanel
    
    ####______________________________________________________
    #### 1.2.2 Main Panel ----
    mainPanel(
      
      # set modal table to fill 95% of screen, set offset from the top of page
      tags$style(
        type = 'text/css',
        '.modal-lg {width:95%; margin-top:1%; padding-bottom:1%}'
      ),
      
      
      # set modal table to fill 50% of screen, set offset from the top of page
      tags$style(
        type = 'text/css',
        '.modal-sm {width:50%; margin-top:1%}'
      ),
      
      
      ####___________________________________________________________
      #### Popout tables ----
      # Table output for correct filenames to be processed
      bsModal(id = 'correcttable', title = 'Correct Filenames', trigger = 'ready', size = 'large',
              dataTableOutput('tablecorrect'),
              
              br(),
              # add button to download csv
              downloadButton('downloadCorrect', 'Download')
      ),
      
      # Table output for irregular filenames
      bsModal(id = 'irregtable', title = 'Irregular Filenames', trigger = 'incorrect', size = 'large',
              dataTableOutput('tableirreg'),
              
              br(),
              # add button to download csv
              downloadButton('downloadIrreg', 'Download')
      ),
      
      # Table output for last locations
      bsModal(id = 'lastlocs', title = 'Last Locations', trigger = 'lastloc', size = 'small',
              dataTableOutput('tablelastloc'),
              
              br(),
              # add button to download csv
              downloadButton('downloadlastlocs', 'Download')
      ),
      
      # Table output for capture table errors
      bsModal(id = 'caperrors', title = 'Capture Table Errors', trigger = 'capcheck', size = 'large',
              dataTableOutput('tablecaperrors'),
              br(),
              # add button to download csv
              downloadButton('downloadCapErrors', 'Download')
      ),
      
      # histogram output for outliers
      bsModal(id = 'capoutliers', title = 'Potential Outliers', trigger = 'viewoutliers', size = 'large',
              plotOutput('histoutliers', 
                         hover = 'plot_hover'
              ),
              br(),
              
              
              # slider to set breaks
              sliderInput('breaks', 'Number of breaks?', min = 5, max = 100, value = 40)
      ),
      
      # Table output for capture table errors
      bsModal(id = 'necerrors', title = 'Necropsy Table Errors', trigger = 'neccheck', size = 'large',
              dataTableOutput('tablenecerrors'),
              br(),
              # button to download csv
              downloadButton('downloadNecErrors', 'Download')
      ),
      h5(textOutput("counter"),style="color:white;"),# fingerprint_code **change**
      h5(textOutput("testtext"),style="color:white;"),# fingerprint_code **change**
      width = 12
    ) # mainPanel
    
    # *password ----
    ) # conditionalPanel (password)
    
  ) # sidebarLayout
) # ui


####________________________________________________________________________
#### 2. --SERVER ----

server <- function(input, output, session) {
  
  # **change** goes with timeout code
  # output$keepAlive <- renderText({
  #   req(input$count)
  #   paste("keep alive ", input$count)
  # })
  
  ####_______________________________________________________________
  #### 2.1 Text Output for 'Processed files' button ----
  observeEvent(input$processed, {
    # busy spinner
    show_spinner()
    # modalDialog to create popup bubble
    output$processed <- showModal(modalDialog(
      paste('There are', nrow(processed),
            'files that have already been processed and are in the server.'
      ),
      easyClose = T # close by clicking outside of box
    ) # modalDialog
    ) # showModal
    hide_spinner()
  }) # observeEvent
  
  
  ####____________________________________________________________
  #### 2.2 Table outputs ----
  
  ####_________________________________
  #### 2.2.1 Correct Names Table ----
  output$tablecorrect <- renderDataTable({
    to.process
  }, # renderDataTable {}
  
  caption = htmltools::tags$caption(HTML(
    paste(
      paste('There are', sum(to.process$`Should process?` == 'Process'),
            'files that are named correctly and ready to be processed.'
      ),
      paste('There are also', sum(cbc), 'CBC files,',
            sum(vitrows), 'VIT/SEP/PRX/Mort files,',
            sum(weird), '.tag/.gdf/.kml files,'
      ),
      paste('and',sum(unk), 'files with AnimalID \'Unk\' or \'UK\', none of which need to be processed.'),
      sep = '<br/>'
    ) # initial paste
  ), # HTML
  style='color:black; font-size:125%;'
  ), # tags$caption
  filter = 'top',
  
  # add vertical scroll bar
  extensions = c('Scroller'),
  options = list(
    deferRender = T,
    scrollY = 500,
    scroller = T
  ),
  fillContainer = T
  
  ) # renderDataTable
  
  ####___________________________________________
  #### 2.2.2 Irregular Names Table ----
  output$tableirreg <- renderDataTable({
    to.correct
  }, #renderDataTable {}
  
  caption = htmltools::tags$caption(
    HTML(
      paste(
        paste(
          'There are', nrow(to.correct), 
          'files that are named incorrectly and need to be reviewed.'
        ),
        'Note: Many of these are not collar files.',
        sep = '<br/>'
      )
    ),
    style='color:black; font-size:125%'
  ),
  filter = 'top',
  # add vertical scroll bar
  extensions = c('Scroller'),
  options = list(
    deferRender = T,
    scrollY = 500,
    scroller = T
  ),
  
  fillContainer = T
  ) # renderDataTable
  
  ####_______________________________________________
  #### 2.2.3 Last Locs Table ----
  output$tablelastloc <- {
    show_spinner()
    lastlocs <- dbGetQuery(con, 'SELECT * FROM Collars_LastLoc')
    lastlocs$LastLoc <- as.character(lastlocs$LastLoc)
    hide_spinner()
    renderDataTable({
      lastlocs
    }, # renderDataTable {}
    filter = 'top',
    # add vertical scroll bar
    extensions = c('Scroller'),
    options = list(
      deferRender = T,
      scrollY = 400,
      scroller = T
    ),
    
    fillContainer = T
    ) # renderDataTable
  } # tablelastloc {}
  
  ####____________________________________________________________________
  #### 2.2.4 Check Capture Records ----
  
  ## update Age_Class options 
  observeEvent(input$outliersGame, 
               {
                 updateSelectizeInput(session, input = 'outliersAge',
                                      choices = caps[caps$Game %in% input$outliersGame, 'Age_Class', drop = T])
               }
  )
  
  ## when button is clicked, check for errors
  output$tablecaperrors <- {
    show_spinner()
    
    
    ### get list of rows for each type of error
    # initialize error column
    # make columns for all possible errors, initialize to ''
    caps$`Capture_Date Error` <- '-'
    caps$`Capture Year Wrong` <- '-'
    caps$`Biological_Year Null` <- '-'
    caps$`GMU Error` <- '-'
    caps$`Radio_Frequency Error` <- '-'
    caps$`LatLong Error` <- '-'
    caps$`Age_Class Error` <- '-'
    caps$`Capture_Method Missing` <- '-'
    caps$`Sex Missing` <- '-'
    caps$`Not GPS Collar` <- '-'
    caps$`CollarMake Missing` <- '-'
    caps$`CollarModel Missing` <- '-'
    caps$`StdAgeClass Missing` <- '-'
    
    #outliers
    caps$`Chest_Girth Error` <- '-'
    caps$`HindFootLength Error` <- '-'
    caps$`BCS Error` <- '-'
    caps$`Temperature Error` <- '-'
    caps$`Hairline Error` <- '-'
    
    
    
    #### outliers in chest_girth, hind foot length, and hairline
    # group by species then age class
    caps <- caps %>%
      group_by(Game, Age_Class) %>%
      mutate(girthout = Chest_Girth %in% boxplot.stats(Chest_Girth)$out,
             hindfootout = HindFootLength %in% boxplot.stats(HindFootLength)$out,
             hairlineout = Hairline %in% boxplot.stats(Hairline)$out) %>%
      ungroup()
    
    # girthouttest <- boxplot.stats(caps[caps$Game == 'White-tailed Deer' & caps$Age_Class == '6 Mo. Old',]$Chest_Girth)$out
    
    # footout <- boxplot.stats(caps[caps$Game == 'Elk' & caps$Age_Class == 'Neonate',]$HindFootLength)$out
    
    
    # now add error message 
    if(nrow(caps[which(caps$girthout == T),]) > 0) caps[which(caps$girthout == T),]$`Chest_Girth Error` <- 'Chest_Girth is an outlier'
    if(nrow(caps[which(caps$hindfootout == T),]) > 0) caps[which(caps$hindfootout == T),]$`HindFootLength Error` <- 'HindFootLength is an outlier'
    if(nrow(caps[which(caps$hairlineout == T | caps$Hairline > caps$HindFootLength),]) > 0) caps[which(caps$hairlineout == T),]$`Hairline Error` <- 'Hairline is an outlier or larger than HindFootLength'
    
    #### BCS error
    if(nrow(caps[which(is.na(caps$BCS) | caps$BCS < 1 | caps$BCS > 5),]) > 0) caps[which(is.na(caps$BCS) | caps$BCS < 1 | caps$BCS > 5),]$`BCS Error` <- 'BCS is NULL or outside of the normal range'
    
    #### cap date in future
    if(nrow(caps[which(ymd(caps$Capture_Date) > Sys.Date()), ]) > 0) caps[which(ymd(caps$Capture_Date) > Sys.Date()), ]$`Capture_Date Error` <- 'Capture_Date is in the future'
    
    #### capture date is wrong year
    # account for people entering wrong year during beginning of year
    # jan-mar, and year is 1 year before entry date
    caps$capyear <- as.numeric(gsub('^([[:digit:]]{4})-.*', '\\1', caps$Capture_Date))
    caps$capmonth <- as.numeric(gsub('^[[:digit:]]{4}-([[:digit:]]{1,2})-.*', '\\1', caps$Capture_Date))
    caps$enteryear <- as.numeric(gsub('^([[:digit:]]{4})-.*', '\\1', caps$DateEntered))
    caps$entermonth <- as.numeric(gsub('^[[:digit:]]{4}-([[:digit:]]{1,2})-.*', '\\1', caps$DateEntered))
    
    
    if(nrow(caps[which((caps$entermonth >= 1 & caps$entermonth <= 3) & (caps$capmonth >= 1 & caps$capmonth <= 3) & (caps$enteryear > caps$capyear)),])) {
      caps[which((caps$entermonth >= 1 & caps$entermonth <= 3) & (caps$enteryear > caps$capyear)),]$`Capture Year Wrong` <- 'Capture year may be off by one year'
    }
    
    #### Biological_Year is null
    if(nrow(caps[which(is.na(caps$Biological_Year)),]) > 0) caps[which(is.na(caps$Biological_Year)),]$`Biological_Year Null` <- 'Biological_Year is null'
    
    #### GMU not in list of GMUs
    gmus <- dbGetQuery(con, 'SELECT GMU FROM SAMM_PIC_GMU')
    if(nrow(caps[which(!(caps$GMU %in% gmus$GMU)),]) > 0) caps[which(!(caps$GMU %in% gmus$GMU)),]$`GMU Error` <- 'GMU is missing or wrong'
    
    #### radio frequency in wrong format
    if(nrow(caps[grep('[[:digit:]]{3}\\.[[:digit:]]{3}', caps$Radio_Frequency, invert = T),])) caps[grep('[[:digit:]]{3}\\.[[:digit:]]{3}', caps$Radio_Frequency, invert = T),]$`Radio_Frequency Error` <- 'Radio_Frequency is wrong format or out of range'
    
    # if(nrow(caps[which(as.numeric(caps$Radio_Frequency) < 146 | as.numeric(caps$Radio_Frequency) >= 220), ]) > 0) {
    #   caps[which(as.numeric(caps$Radio_Frequency) < 146 | as.numeric(caps$Radio_Frequency) >= 220), ]$`Radio_Frequency Error` <- 'Radio_Frequency is wrong format or out of range'
    # }
    
    #### capture lat/long not in state lat between 49 and 42 deg, long between -111 and 117.25
    if(nrow(caps[which(caps$Longitude < -117.25 | caps$Longitude > -111 |
                       caps$Latitude > 49 | caps$Latitude < 42),]) > 0) caps[which(caps$Longitude < -117.25 | caps$Longitude > -111 |
                                                                                     caps$Latitude > 49 | caps$Latitude < 42),]$`LatLong Error` <- 'Latitude/Longitude not in Idaho'
    
    #### outliers in temperature
    tempout <- boxplot.stats(caps$Temperature, coef = .1)$out
    if(nrow(caps[which(caps$Temperature %in% tempout | caps$Temperature == 0 | is.na(caps$Temperature)),]) > 0) {
      caps[which(caps$Temperature %in% tempout | caps$Temperature == 0 | is.na(caps$Temperature)),]$`Temperature Error` <- 'Temperature is an outlier or missing'
    }
    
    
    
    #### AgeClass and years_old don't match up
    ageclasserror <- 
      which(
        (caps$Game == 'Mule Deer' | caps$Game == 'White-tailed Deer' | caps$Game == 'Elk') &
          !( # start of age class/year checks
            ((caps$Age_Class == 'Neonate' | caps$Age_Class == '6 Mo. Old') & caps$Years_Old == .1) | 
              ((caps$Age_Class == 'Calf' | caps$Age_Class == 'Fawn') & caps$Years_Old < 1) | 
              (caps$Age_Class == 'Yearling' & caps$Years_Old >= 1 & caps$Years_Old < 2) |
              ((caps$Age_Class == 'Adult' | caps$Age_Class == '2-9yr old' | caps$Age_Class == '2-yr old' | 
                  caps$Age_Class == '10-14yr old' | caps$Age_Class == '3-15yr old') & caps$Years_Old >= 2)
          ) # end of age class/year checks
      )
    
    #### Capture method is null
    if(nrow(caps[which(is.na(caps$Capture_Method)),]) > 0) caps[which(is.na(caps$Capture_Method)),]$`Capture_Method Missing` <- 'Capture_Method is missing'
    
    #### sex is null
    if(nrow(caps[which(is.na(caps$Sex)),]) > 0) caps[which(is.na(caps$Sex)),]$`Sex Missing` <- 'Sex is missing'
    
    #### weight should be in pounds; not sure how to check though
    
    #### CollarTypeID isn't '3' (GPS)
    if(nrow(caps[which(caps$CollarType != '3'),]) > 0) caps[which(caps$CollarType != '3'),]$`Not GPS Collar` <- 'CollarType is not GPS'
    
    #### collar make and model missing
    if(nrow(caps[which(is.na(caps$CollarMake)),]) > 0) caps[which(is.na(caps$CollarMake)),]$`CollarMake Missing` <- 'CollarMake is missing'
    
    if(nrow(caps[which(is.na(caps$CollarModel)),]) > 0) caps[which(is.na(caps$CollarModel)),]$`CollarModel Missing` <- 'CollarModel is missing'
    
    #### stdAgeClass null
    if(nrow(caps[which(is.na(caps$StdAgeClass)),]) > 0) caps[which(is.na(caps$StdAgeClass)),]$`StdAgeClass Missing` <- 'StdAgeClass is missing'
    
    ### put all the errors together 
    caps$Error <- paste(caps$`Capture_Date Error`, 
                        caps$`Biological_Year Null`, 
                        caps$`GMU Error`,
                        caps$`Radio_Frequency Error`,
                        caps$`Age_Class Error`,
                        caps$`LatLong Error`, 
                        caps$`Capture_Method Missing`, 
                        caps$`Sex Missing`, 
                        caps$`Not GPS Collar`, 
                        caps$`CollarMake Missing`,
                        caps$`CollarModel Missing`,
                        caps$`StdAgeClass Missing`,
                        
                        caps$`Chest_Girth Error`,
                        caps$`HindFootLength Error`,
                        caps$`BCS Error`,
                        caps$`Temperature Error`,
                        caps$`Hairline Error`,
                        sep = ', ')
    
    # remove '-, '
    caps$Error <- gsub('-, ', '', caps$Error)
    caps$Error <- gsub(', -', '', caps$Error)
    caps$Error <- gsub(', $', '', caps$Error)
    
    # remove unneeded variables
    caps <- caps %>% 
      select(-c('girthout', 'hindfootout', 'hairlineout', 'capyear', 'capmonth', 'enteryear', 'entermonth'))
    
    
    hide_spinner()
    
    # now actually render table
    renderDataTable({
      caps
    },
    filter = 'top',
    # add vertical scroll bar
    extensions = c('Scroller'),
    options = list(
      deferRender = T,
      scrollY = 500,
      scroller = T
    ),
    fillContainer = T
    ) # renderDataTable
  } # observeEvent
  
  ####_________________________________________________________________
  #### 2.2.5 Capture Outlier Histograms ----
  
  output$histoutliers <- renderPlot({
    
    toview <- caps %>%
      filter(Game == input$outliersGame & Age_Class == input$outliersAge)
    
    
    ggplot(toview, aes(x = !!input$outliersVar) ) + 
      geom_histogram(bins = input$breaks) +
      xlab(paste(input$outliersVar, 'for', input$outliersGame, 'in the', input$outliersAge, 'age class')) +
      ggtitle(input$outliersVar) + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 50)) + 
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 18, face = 'bold'),
            plot.title = element_text(size = 24, face = 'bold')) 
    
  })
  
  ####__________________________________________________________________
  #### 2.2.6 Check Necropsy Records ----
  
  observeEvent(input$neccheck, {
    output$tablenecerrors <- {
      
      show_spinner()
      
      ### Error message row for each error
      necs$`CaptureID Null` <- '-' # should not be null
      necs$`Duplicate CaptureID` <- '-' # should only be one entry for each capture ID
      necs$`Capture Table Error` <- '-' # there should be an entry in the capture table
      necs$`Animal_ID Null` <- '-' # should not be null
      necs$`GameID Null` <- '-' # should not be null, should be in list of GameIDs
      necs$`Animal/GameID Capture Error` <- '-' # should be same for linked capture record (linked by capture ID)
      necs$`FateID Error` <- '-' # fateID corresponds to value in list
      necs$`FateID/Date Error` <- '-' # fateID and fateDate both exist if one of them does
      necs$`FateDate Error` <- '-' # fateDate is >= capture date and is not in future
      necs$`CensorID/Date Error` <- '-' # if censor ID exists, so does censor date (and vice versa)
      necs$`DateNec Error` <- '-' # should be >= capture date and fate/censor dates, not in future
      necs$`RadFreq Error` <- '-' # in ###.### format
      necs$`LatLong Error` <- '-' # in Idaho, should be within 100 mi of capture location
      necs$`GMU Error` <- '-' # is in list of GMUs
      necs$`CausCert Error` <- '-' # in list (certain, probable, possible); shouldn't be null if fate listed, unless fate unknown
      necs$`LastLoc Error` <- '-' # should be after Capture Date
      
      ### Outliers
      necs$`MaxTemp Error` <- '-' # shouldn't be an outlier
      
      #### Check for errors
      ###  Capture ID Null
      if(nrow(necs[is.na(necs$CaptureID),]) > 0) necs[is.na(necs$CaptureID),]$`CaptureID Null` <- 'CaptureID is null'
      
      ### Duplicate CaptureID
      if( sum(duplicated(necs$CaptureID)) > 0 ) necs[which(duplicated(necs$CaptureID)),]$`Duplicate CaptureID` <- 'CaptureID has multiple necropsy records'
      
      ### Corresponding record in capture table
      if (sum(!(necs$CaptureID %in% caps$CaptureID)) > 0) necs[which(!(necs$CaptureID %in% caps$CaptureID)),]$`Capture Table Error` <- 'no corresponding entry in capture table'
      
      ### Animal_ID Null
      if (sum(is.na(necs$Animal_ID)) > 0) necs[which(is.na(necs$Animal_ID)),]$`Animal_ID Null` <- 'Animal_ID is null'
      
      ### GameID Null
      if (sum(is.na(necs$GameID)) > 0) necs[which(is.na(necs$GameID)),]$`GameID Null` <- 'GameID is null'
      
      ### Animal/GameID Capture Error
      
      errors <- c()
      for (i in 1:nrow(necs)) {
        if (
          !is.na(necs[i,]$Animal_ID) & # make sure that row has an Animal_ID
          !is.na(necs[i,]$CaptureID) &
          nrow(caps[caps$CaptureID == necs[i,]$CaptureID,]) > 0 # make sure that record exists in capture table
        ) {
          if( necs[i,]$Animal_ID != caps[ caps$CaptureID == necs[i,]$CaptureID, ]$Animal_ID ) { # now see if Animal_IDs match
            errors <- c(errors, i)
          } # second if
        } # first if
        
      } # for
      
      
      for (i in 1:nrow(necs)) {
        if (
          !is.na(necs[i,]$GameID) &
          !is.na(necs[i,]$CaptureID) &
          nrow(caps[caps$CaptureID == necs[i,]$CaptureID,]) > 0
        ) {
          if ( necs[i,]$Game != caps[ caps$CaptureID == necs[i,]$CaptureID, ]$Game ) {
            errors <- c(errors, i)
          } # second if
        } # first if 
      }
      
      
      necs[errors,]$`Animal/GameID Capture Error` <- 'Animal_ID or GameID does not match capture table for that CaptureID'
      
      ### FateID Error
      if (sum(!(necs$FateID %in% 1:27)) > 0) necs[which(!(necs$FateID %in% 1:27)),]$`FateID Error` <- 'FateID is null or not a value in list'
      
      ### FateID/Date Error  # fateID and fateDate both exist if one of them does
      if(sum( !is.na(necs$FateID) & is.na(necs$FateDate)  |  is.na(necs$FateID) & !is.na(necs$FateDate) ) > 0) {
        necs[which(!is.na(necs$FateID) & is.na(necs$FateDate)  |  is.na(necs$FateID) & !is.na(necs$FateDate)),]$`FateID/Date Error` <-
          'only one of FateID/FateDate is null'
      }
      
      ### FateDate Error  # fateDate should be >= capture date and not in future
      if (nrow(necs[which(ymd(necs$FateDate) < ymd(necs$Capture_Date) | ymd(necs$FateDate) > Sys.Date()),]) > 0) {
        necs[which(ymd(necs$FateDate) < ymd(necs$Capture_Date) | ymd(necs$FateDate) > Sys.Date()),]$`FateDate Error` <- 'FateDate is in future or before capture date'
      }
      
      
      ### CensorID/Date Error  # if censor ID exists, so does censor date (and vice versa)
      if(sum( !is.na(necs$CensorID) & is.na(necs$CensorDate)  |  is.na(necs$CensorID) & !is.na(necs$CensorDate) ) > 0) {
        necs[which(!is.na(necs$CensorID) & is.na(necs$CensorDate)  |  is.na(necs$CensorID) & !is.na(necs$CensorDate)),]$`CensorID/Date Error` <-
          'only one of CensorID/CensorDate is null'
      }
      
      ### DateNec Error  # should be >= capture date and fate/censor dates, not in future
      if (length(
        which(ymd(necs$DateNec) < ymd(necs$Capture_Date) |
              ymd(necs$DateNec) < ymd(necs$FateDate) |
              ymd(necs$DateNec) < ymd(necs$CensorDate) |
              ymd(necs$DateNec) > Sys.Date()
        )
      )
      ) necs[which(ymd(necs$DateNec) < ymd(necs$Capture_Date) |
                     ymd(necs$DateNec) < ymd(necs$FateDate) |
                     ymd(necs$DateNec) < ymd(necs$CensorDate) |
                     ymd(necs$DateNec) > Sys.Date()
      ), # which
      ]$`DateNec Error` <- 'DateNec is before capture or fate/censor date or in future'
      
      
      ### RadFreq Error  # in ###.### format
      if(nrow(necs[grep('^[[:digit:]]{3}\\.[[:digit:]]{3}$', necs$RadFreq, invert = T),]) > 0) {
        necs[grep('^[[:digit:]]{3}\\.[[:digit:]]{3}$', necs$RadFreq, invert = T),]$`RadFreq Error` <- 'RadFreq is in wrong format or out of range'
      }
      
      if(nrow(necs[which(!is.na(necs$RadFreq) & (necs$RadFreq < 146 | necs$RadFreq >= 220)),]) > 0) {
        necs[which(!is.na(necs$RadFreq) & (as.numeric(necs$RadFreq) < 146 | as.numeric(necs$RadFreq) >= 220)),]$`RadFreq Error` <- 'RadFreq is in wrong format or out of range'
      }
      
      if(nrow(necs[which(is.na(necs$RadFreq)),]) > 0) {
        necs[which(is.na(necs$RadFreq)),]$`RadFreq Error` <- 'RadFreq is missing'
      }
      
      
      
      ## LatLong Error  # in Idaho, should be within 100 mi of capture location
      if(nrow(necs[which(necs$Longitude < -117.25 | necs$Longitude > -111 |
                         necs$Latitude > 49 | necs$Latitude < 42),]) > 0) necs[which(necs$Longitude < -117.25 | necs$Longitude > -111 |
                                                                                       necs$Latitude > 49 | necs$Latitude < 42),]$`LatLong Error` <- 'Latitude/Longitude not in Idaho or >100mi from capture loc'
      
      # initialize to 1000 so it gets flagged
      necs$dist <- 1000
      
      # make sure numbers aren't in scientific format
      options(scipen = 10)
      
      # loop through capture and necropsy locations and determine distance between capture and necropsy
      for (i in 1:nrow(necs)) {
        # need to make sure that all the values are valid
        if(!is.na(necs$Latitude[i]) & !is.na(necs$Longitude[i]) & !is.na(necs$Longitude[i]) & !is.na(necs$CaptureLongitude[i]) &
           necs$Latitude[i] < 90 & necs$Latitude[i] > 0 &
           necs$Longitude[i] < 180 & necs$Longitude[i] > -180 &
           necs$CaptureLatitude[i] < 90 & necs$CaptureLatitude[i] > 0 &
           necs$CaptureLongitude[i] < 180 & necs$CaptureLongitude[i] > -180
        ) {
          # this gives distance in meters between two points
          necs$dist[i] <- geosphere::distm(c(necs$Longitude[i], necs$Latitude[i]), c(necs$CaptureLongitude[i], necs$CaptureLatitude[i]), fun = distHaversine)/1609 # convert back to miles **change**
        }
      }
      
      if (nrow(necs[necs$dist > 100,]) > 0) necs[necs$dist > 100,]$`LatLong Error` <- 'Latitude/Longitude not in Idaho or >100mi from capture loc'
      
      
      ### GMU Error  # is in list of GMUs
      gmus <- dbGetQuery(con, 'SELECT GMU FROM SAMM_PIC_GMU')
      if (nrow(necs[which(!(necs$GMU %in% gmus$GMU)),]) > 0) necs[which(!(necs$GMU %in% gmus$GMU)),]$`GMU Error` <- 'GMU is missing or wrong'
      
      ### CausCert Error  # in list (certain, probable, possible); shouldn't be null if fate listed, unless fate unknown
      if (nrow(necs[which(!(necs$CausCert %in% c('Certain', 'Probable', 'Possible'))), ]) > 0) {
        necs[which(!(necs$CausCert %in% c('Certain', 'Probable', 'Possible'))), ]$`CausCert Error` <- 'CausCert is missing or incorrect'
      }
      
      ### LastLoc Error  # should be after Capture Date
      
      if(nrow(necs[which(!is.na(necs$LastLoc) & (necs$LastLoc < necs$Capture_Date)),]) > 0) necs[which(!is.na(necs$LastLoc) & (necs$LastLoc < necs$Capture_Date)),]$LastLoc <- 'LastLoc is before Capture_Date'
      
      ### MaxTemp Error  # outlier distribution
      
      tempout <- boxplot.stats(necs$MaxTemp, coef = .1)$out
      if(nrow(necs[which(necs$MaxTemp %in% tempout | necs$MaxTemp == 0 | is.na(necs$MaxTemp)),]) > 0) {
        necs[which(necs$MaxTemp %in% tempout | necs$MaxTemp == 0 | is.na(necs$MaxTemp)),]$`MaxTemp Error` <- 'MaxTemp is an outlier or missing'
      }
      
      necs$Error <- paste(
        necs$`CaptureID Null`,
        necs$`Duplicate CaptureID`,
        necs$`Capture Table Error`,
        necs$`Animal_ID Null`,
        necs$`GameID Null`,
        necs$`Animal/GameID Capture Error`,
        necs$`FateID Error`,
        necs$`FateID/Date Error`,
        necs$`FateDate Error`,
        necs$`CensorID/Date Error`,
        necs$`DateNec Error`,
        necs$`RadFreq Error`,
        necs$`LatLong Error`,
        necs$`GMU Error`,
        necs$`CausCert Error`,
        necs$`LastLoc Error`,
        necs$`MaxTemp Error`,
        sep = ', '
      )
      
      # remove '-, '
      necs$Error <- gsub('-, ', '', necs$Error)
      necs$Error <- gsub(', -', '', necs$Error)
      necs$Error <- gsub(', $', '', necs$Error)
      
      necs <- necs %>%
        select(-c('dist'))
      
      hide_spinner()
      
      # now actually render table
      renderDataTable({
        necs
      },
      
      filter = 'top',
      
      # add vertical scroll bar
      extensions = c('Scroller'),
      options = list(
        deferRender = T,
        scrollY = 500,
        scroller = T
      ),
      fillContainer = T
      ) # renderDataTable
      
      
      
      
    } # table output
  })
  
  
  
  ####_________________________________________________________________
  #### 2.3 Download buttons ----
  
  ## correct files
  output$downloadCorrect <- downloadHandler(
    filename = 'Correct_Files.csv',
    content = function(cont) { 
      write.csv(to.process, cont, row.names = F)
    }
  ) # downloadHandler
  ## irregular files
  output$downloadIrreg <- downloadHandler(
    filename = 'Irregular_Files.csv',
    content = function(cont) {
      write.csv(to.correct, cont, row.names = F)
    }
  ) # downloadHandler
  
  ## last locs
  output$downloadlastlocs <- downloadHandler(
    filename = 'Last_Locations.csv',
    content = function(cont) {
      write.csv(lastlocs, cont, row.names = F)
    }
  ) # downloadHandler
  
  ## capture errors
  output$downloadCapErrors <- downloadHandler(
    filename = 'Capture_Errors_Table.csv',
    content = function(cont) {
      write.csv(caps, cont, row.names = F)
    } 
  ) # downloadHandler
  
  ## necropsy errors
  output$downloadNecErrors <- downloadHandler(
    filename = 'Necropsy_Errors_Table.csv',
    content = function(cont) {
      write.csv(necs, cont, row.names = F)
    } 
  ) # downloadHandler
  
  ####____________________________________________________________________
  #### 2.4 Sort files ----
  
  ##
  observeEvent(input$sort, {
    show_modal_gif(src = 'raptors.gif')
    source('code/Move_Files.R')
    remove_modal_gif()
    output$sort <- showModal(modalDialog(
      HTML(
        paste(
          paste('Collar files are located in:',
                'K:/Wildlife/Collars/Files to process/TXTs to process DONT ADD FILES TO THIS,',
                'K:/Wildlife/Collars/Files to process/CSVs to process DONT ADD FILES TO THIS,',
                'K:/Wildlife/Collars/Files to process/CSVs to process pt 2 DONT ADD FILES TO THIS,',
                'K:/Wildlife/Collars/Files to process/TXTs to process pt 2 DONT ADD FILES TO THIS, and',
                'K:/Wildlife/Collars/Files to process/XLSXs to process DONT ADD FILES TO THIS;', 
                'as well as their original locations.', sep = '<br/>'
          ) # paste
        ) # paste
      ) # paste
    )) # showModal, modalDialog
  }) # observeEvent
  
  
  # VISIT COUNTER #### **change**
  
  # fingerprint_code
  output$testtext <- shiny::renderText(paste("     fingerprint: ", input$fingerprint, "     ip: ", input$ipid))
  
  # fingerprint_code
  output$counter <- shiny::renderText({
    appname <- "TheOneRing"
    df <- session$clientData
    if(!file.exists(paste("/srv/shiny-server/EAR/userdata/",appname,"_counter.RData",sep=""))){
      # if(!file.exists(paste("K:/Wildlife/Shiny_Apps/EAR/userdata/",appname,"_counter.RData",sep=""))){
      df <- session$clientData
      counter <- 0
      datetime <- Sys.time()
      fingerprint <- input$fingerprint
      ipid <- input$ipid
      df_counter <- data.frame(datetime = datetime,
                               appname = appname,
                               fingerprint = fingerprint,
                               ipid = ipid)
      df_counter <- df_counter %>%
        dplyr::filter(fingerprint != "" & ipid != "")
      if(nrow(df_counter)>=1){
        df_counter$counter <- 1:nrow(df_counter)
      }
    } else {
      load(file=paste("/srv/shiny-server/EAR/userdata/",appname,"_counter.RData",sep=""))
      # load(file=paste("K:/Wildlife/Shiny_Apps/EAR/userdata/",appname,"_counter.RData",sep=""))
      df <- session$clientData
      counter <- df_counter$counter + 1
      datetime <- Sys.time()
      fingerprint <- input$fingerprint
      ipid <- input$ipid
      df_counter_new <- data.frame(counter = min(counter),
                                   datetime = datetime,
                                   appname = appname,
                                   fingerprint = fingerprint,
                                   ipid = ipid)
      df_counter_new <- df_counter_new %>%
        dplyr::filter(fingerprint != "" & ipid != "")
      df_counter <- dplyr::bind_rows(df_counter,df_counter_new)
      if(nrow(df_counter)>=1){
        df_counter$counter <- 1:nrow(df_counter)
      }
    }
    save(list=c("df_counter","counter"), file=paste("/srv/shiny-server/EAR/userdata/",appname,"_counter.RData",sep=""))
    return(paste("Hits: ",!file.exists(paste("/srv/shiny-server/EAR/userdata/",appname,"_counter.RData",sep=""))))
    # save(list=c("df_counter","counter"), file=paste("K:/Wildlife/Shiny_Apps/EAR/userdata/",appname,"_counter.RData",sep=""))
    # return(paste("Hits: ",!file.exists(paste("K:/Wildlife/Shiny_Apps/EAR/userdata/",appname,"_counter.RData",sep=""))))
  })  
  
  
} # server

shinyApp(ui, server)
















































































