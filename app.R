###____________________________________###
### SHINY APP TO ASSESS HARD DOWNLOADS ###
###____________________________________###

## load necessary packages
source('K:/Wildlife/Shiny_Apps/ZCF/Hard-Downloads-App/code/Collar_Shiny_Packages.R')


source('K:/Wildlife/Shiny_Apps/ZCF/Hard-Downloads-App/code/Rename_Files.R')

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


####________________________________________________________________
#### 1. UI ----

## Start ui
ui <- fluidPage(
  ####________________________________________________
  #### 1.1 Appearance ----
  
  # set app theme
  theme = shinytheme('yeti'),
  
  # set position for popup dialogs on page
  tags$style(
             type = 'text/css',
             '.modal-dialog {margin-top:15%}'
            ),
  
  # add 'thinking' spinner
  use_busy_spinner(spin = 'fading-circle', position = 'full-page', margins = c('300px', '300px')),
  
    ####_______________________________________________________________
    #### 1.1.2 Title Bar----
    # use navbarPage even though only one tab
    navbarPage(
               tags$style('height: 100px'),
               inverse = T, # dark background light text
               fluid = T, 
               selected = 2, # default to second tab (not really important here because only 1 tab to choose from)
               title = div(
                           # add logos to top of page
                           img(src = 'WildlifeResearchPage_logo_small.png',
                               height = '90px',
                               style = 'padding-top:10px; padding-bottom:10px; padding-right:30px'
                               ),
        
                           div(
                               img(
                                   src = 'idfglogo.png',
                                   height = '80px',
                                   style = 'position: absolute; top:10px; right:10px'
                                   )
                               ),
                           
                           div(
                               img(
                                   src = 'r-shiny-logo.png',
                                   height = '50px',
                                   style = 'position:absolute; left:70px; top:55px'
                                   )
                               )
  
                           ), # title div
               tabPanel(
                        value = 'tab2',
                        title = h1('Check Hard Downloads', font = 'Arial')
                        )
               ), # navbarPage
    

  ####_________________________________________________________
  #### 1.2 Sidebar Panel ----
  sidebarLayout(
                fluid = T,
                sidebarPanel(
                             ####_______________________________________________
                             #### Action Buttons for checking/viewing files ----
                             
                             ## Header
                             h4('Check Status of Hard Downloaded Files'),
                             
                             ## Add buttons
                             fluidRow(
                                      actionButton(inputId = 'processed', label = 'Processed files',
                                                   style = 'width:200px'),
                                      actionButton(inputId = 'ready', label = 'Ready to be processed',
                                                   style = 'width:200px'),
                                      actionButton(inputId = 'incorrect', label = 'Need to be reviewed',
                                                   style = 'width:200px')
                             ),
                             
                             br(),
                             
                             # Action Button/header for sorting files
                             h4(HTML('Sort files that need to be processed into one folder for easy viewing<br/>Note: This might take several minutes')),
                             fluidRow(
                                      actionButton(inputId = 'sort', label = 'Sort Files',
                                                   style = 'width:200px')
                             ),
                             
                             br(),
                             
                             # Action button to view last locs
                             h4('View last location for each collar'),
                             fluidRow(
                                      actionButton(inputId = 'lastloc', label = 'View Last Locs',
                                                   style = 'width:200px')
                             ),
                             
                             width = 12
                             
                ), # sidebarPanel
                
                ####______________________________________________________
                #### 1.2.2 Main Panel ----
                mainPanel(
                  
                          ####___________________________________________________________
                          #### Popout tables ----
                          # Table output for correct filenames to be processed
                          bsModal(id = 'correcttable', title = 'Correct Filenames', trigger = 'ready', size = 'large',
                                  dataTableOutput('tablecorrect'),
                                  # set modal table to fill 90% of screen, set offset from the top of page
                                  tags$style(
                                             type = 'text/css',
                                             '.modal-lg {width:90%; margin-top:5%}'
                                             ),
                                  br(),
                                  # add button to download csv
                                  downloadButton('downloadCorrect', 'Download')
                                  ),
                          
                          # Table output for irregular filenames
                          bsModal(id = 'irregtable', title = 'Irregular Filenames', trigger = 'incorrect', size = 'large',
                                  dataTableOutput('tableirreg'),
                                  # set modal table to fill 90% of screen, set offset from the top of page
                                  tags$style(
                                             type = 'text/css',
                                             '.modal-lg {width:90%; margin-top:5%}' 
                                             ),
                                  br(),
                                  # add button to download csv
                                  downloadButton('downloadIrreg', 'Download')
                                  ),
                          
                          # Table output for last locations
                          bsModal(id = 'lastlocs', title = 'Last Locations', trigger = 'lastloc', size = 'small',
                                  dataTableOutput('tablelastloc'),
                                  # set modal table to fill 90% of screen, set offset from the top of page
                                  tags$style(
                                             type = 'text/css',
                                             '.modal-sm {width:50%; margin-top:5%}'
                                            ),
                                  br(),
                                  # add button to download csv
                                  downloadButton('downloadlastlocs', 'Download')
                                  ),
                          
                          width = 12
                          ) # mainPanel
  ) # sidebarLayout
) # ui


####________________________________________________________________________
#### 2. SERVER ----

server <- function(input, output, session) {

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
                                                                                      ), # initial paste
                                                                                ), # HTML
                                                                            style='color:black; font-size:125%;'
                                                                           ), # tags$caption
                                         
                                          
                                          # add vertical scroll bar
                                          extensions = c('Scroller'),
                                          options = list(
                                                         deferRender = T,
                                                         scrollY = 400,
                                                         scroller = T
                                                        )
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
                                        # add vertical scroll bar
                                        extensions = c('Scroller'),
                                        options = list(
                                                       deferRender = T,
                                                       scrollY = 500,
                                                       scroller = T
                                                      )
                                      ) # renderDataTable
  
  ####_______________________________________________
  #### 2.2.3 Last Locs Table ----
  output$tablelastloc <- {
                          show_spinner()
                          lastlocs <- dbGetQuery(con, 'SELECT * FROM Collars_LastLoc')
                          hide_spinner()
                          renderDataTable({
                                           lastlocs
                                          }, # renderDataTable {}
                                          # add vertical scroll bar
                                          extensions = c('Scroller'),
                                          options = list(
                                                         deferRender = T,
                                                         scrollY = 400,
                                                         scroller = T
                                                        )
                                          ) # renderDataTable
                         } # tablelastloc {}
  
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
  
  ####____________________________________________________________________
  #### 2.4 Sort files ----
  
  ##
  observeEvent(input$sort, {
                            show_modal_gif(src = 'raptors.gif')
                            source('K:/Wildlife/Shiny_Apps/ZCF/Hard-Downloads-App/Move_Files.R')
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
  
} # server

shinyApp(ui, server)

















































































