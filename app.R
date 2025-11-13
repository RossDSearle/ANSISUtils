library(shiny)
library(shinycssloaders)
library(jsonlite)
library(DT)
library(leaflet)
library(rhandsontable)
library(leaflet.extras)
library(stringr)
library(tictoc)
library(sf)
library(leafem)
library(listviewer)
library(shinyWidgets)
library(munsell)
library(terra)
library(rgl)
library(rayshader)
library(magrittr)
library(RColorBrewer)

machineName <- as.character(Sys.info()['nodename'])
if (machineName=='soils-discover2') {
  rgb <- terra::rast('/datasets/work/lw-soildatarepo/work/Ross/ShinyData/ANSISSitesViewer/AustRgb.tif')
  dem <- terra::rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/Covariates/Mosaics/90m/Relief_dems_3s_mosaic1.tif')
  demoData <- '/srv/shiny-server/Apps/SoliDataFederator2/DemoData/4_ANSIS_Sites.json'
} else {
  rgb <- terra::rast('C:/Projects/GIS/National/Rasters/AustRgb.tif')
  dem <- terra::rast('C:/Projects/GIS/National/Rasters/Relief_dems_3s_mosaic1.tif')
  demoData <- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ShowANSISSites2/DemoData/4_ANSIS_Sites.json'
}



options(shiny.maxRequestSize = 60*1024^2)
options(rgl.useNULL = T)

source('./config.R')
source('./parsing.R')
source('./guiHelpers.R')
source('./plots.R')
source('./functions.R')
source('./StandAloneCode/parseANSISJson.R')

mps <<- read.csv('./schemaFieldMapping2.csv')
mp <<- unique(mps[mps$Domain!='' & mps$SchemaLocation=='Horizons', ]$Domain )
#CodesTable <<- fromJSON('https://www.asris.csiro.au/NatSoil_Services/api/CSISCodes')
## there is a small issue 0f some domains starting with "N_" instead of "C_" from this source so decided
## to edit a local version. Could deal with it in code if I had to

CodesTable <<- read.csv('./ANSISCodes.csv')

devel = T


ui <- fluidPage(

    tags$div(HTML('
                  <a href="https://www.tern.org.au//" target="_blank"><img src="./Logos/TERN_logo-v2.png" height=80px, width=360px></a>

                  <a href="https://ansis.net/" target="_blank"><img style="float: right;" src="./Logos/ANSISwithText.PNG"></a>') ),

    tags$head(tags$style(".shiny-notification {position: fixed; top: 20% ;left: 50%")),


    sidebarLayout(
        sidebarPanel(
          width=3,

      fluidRow(

            HTML(paste0('<H2>ANSIS Data Viewer</H2>')),
            fileInput('wgtFile', 'Drag ANSIS query result here'),
            selectInput('wgtSiteIDs', label='Sites', choices = NULL),
            HTML('Select a site name from the dropdown list above or click on a site in the map below to show the site info.<BR>'),
            leafletOutput("mainMap", height = defMapHeight, width = defMapWidth),
            HTML('<BR><BR>'),

            selectInput('wgtDownloadOption',label='Data Downloads',
                     choices = c('Current Site Visit as CSV',
                                 'Current Site Horizons as CSV',
                                 'Current Site Lab Results as CSV',
                                 'All of Current Site as CSV',
                                 'All Site Data as CSV',
                                 'All Sites as GIS Point Data'
                               )),
           downloadButton('wgtDownloadCSV', 'Download Data'),
      ),
      fluidRow(HTML('<BR><BR><BR>'),
         #actionButton('wgtBtnScreenShot', 'Print'),
        # screenshotButton(label = "Capture Screen", download = T )
      )
        ),

        mainPanel(

          tabsetPanel(type = "tabs",
                      ##### Site Description Tab  ####
                      tabPanel(id= 'asdfghjkkl',  "Site Description",

                               fluidRow(column(2,plotOutput('wgtProfile')),

                                         column(10,

                                            # hidden(downloadButton("wgtSiteSheetDownload", "Download Site Description", class = "btn-primary")),
                                             htmlOutput('wgtSiteVisitDescription'),
                                            htmlOutput('wgtProfileDescription'),


                                            # hidden(htmlOutput("wgtLabResultsLabel2")),
                                            HTML('<BR><BR>'),
                                            rHandsontableOutput('UI_SiteDescription_LabResults' )
                                            )
                                         )),

                      tabPanel("Site Data Tables",
                               HTML('<H3>Site Visit Table</H3>'),
                               rHandsontableOutput('UI_SiteDataTables_SiteVisit' ),
                               HTML('<H3>Horizons Table</H3>'),
                               rHandsontableOutput('UI_SiteDataTables_Morphology' ),
                               HTML('<H3>Laboratory Results Table</H3>'),
                               rHandsontableOutput('UI_SiteDataTables_LabResults' )
                               ),
                      tabPanel("Lab Results Plots",
                               sidebarPanel( width=2,
                                            # HTML('HELLO'),
                                             selectInput('wgtLabResultsSelect',label='Choose a lab Method to plot',choices = NULL),
                                            HTML('<BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR>
                                                 <BR><BR><BR><BR><BR><BR><BR><BR><BR>'),
                               ),

                               mainPanel(
                                 HTML('<H4> Lab Data Soil Profile Plot</H4>'),
                                 #
                                 plotOutput('UI_LabResultsPlots', height = '730px'),
                                 HTML('<H3> All lab data for this site</H3>'),
                                 rHandsontableOutput('UI_LabPlotsAllLabsTable'),
                               )
                      ),

                      tabPanel("Landscape Diagram", column(1,),
                               column(10,
                                HTML('Click on the 3D diagram and hold down the left button to rotate the scence and use the scroll wheel to zoom in and out on the scene.'),
                               rglwidgetOutput("sctPlot", height = '800px', width='800px') %>% withSpinner(color="#0dc5c1")
                               ),
                      ),
                      tabPanel("Site Raw CSV",
                               rHandsontableOutput('UI_DataViewer'),
                               ),
                      tabPanel('JsonView', jsoneditOutput( "jsed", height = '1600px' ) %>% withSpinner(color="#0dc5c1")),

                      tabPanel('About', HTML('<div style="max-width: 800px;">

                                        <h2>What is ANSIS</h2><BR>
                                             <p>The Australian National Soil Information System (<a href="https://ansis.net/" target="_blank"> ANSIS</a>) provides access to nationally consistent soil data and information to support the sustainable management of soil.

                                             ANSIS is the new place to find readily useable Australian soil data and information. </p>

                                             <p>ANSIS is the Australian National Soil Information System. It brings together soil data from across Australia, connecting multiple data sources to provide access to nationally consistent soil data and information.
                                            ANSIS has been developed through a collaboration between governments, research organisations, industry, the private sector, and the community.
                                            For more info have a read <a href="https://ansis.net/about/" target="_blank"> here.</a></p>

                                             <p>ANSIS delivers soil data as Javascript Object Notation (JSON) formated files. This data format allows the delivery
                                             of complex structured information in a lossless form. But..... it can be challenging to manipulate this data into simpler
                                             formats without coding skills. </p>

                                             <p>That is where this ANSIS Utils comes in .....</p>


                                        <h2>How to use this App</h2><BR>

                                            <p>First, you need to go the the <a href="https://portal.ansis.net/" target="_blank">ANSIS Data Portal</a> to query the data you want, and download the JSON file it generates.</p>

                                            <p>ANSIS Utils allows you to upload an ANSIS JSON query response
                                                and parse it in to other formats as well as visualise the soil profile information.</p>

                                             <p>Simply click on the browse button in the top left corner to find the ANSIS JSON reponse file to upload
                                             (or drag it to the text box with file Explorer). The App will then parse the json into a number of simple formats.</p>

                                             <p>Select a soil site from the "Sites" drop down list to view the soil site description in plain english.</p>

                                             <p>The tabs along the top of the App allow you to view the ANSIS query response in a range of formats.</p>

                                             <p>Use the "Data Downloads" drop down list to select components of the ANSIS query response to download as csv files
                                             or a geoJson of site locations.</p>

                                              <h2>Acknowledgements</h2><BR>


                                             <p> ANSISUtils is powered by TERN. </p>
                                              <br>
                                              <p>ANSIS : When using data from ANSIS please include this acknoweldgement :- </p><br><p>"ANSIS has been supported by funding through the Australian Government Natural Heritage Trust (Department of Agriculture, Fisheries and Forestry) in collaboration with CSIRO."</p>
                                              <p>Partners :  CSIRO, Corangamite CMA, Glenelg Hopkins CMA, Monash University, Sydney University, Federation University, NSW Govt, NT Govt, Qld Govt, SA Govt, Tas Govt, Vic Govt, WA Govt. </p>


                                             </div>'

                                             ))
                      )
        )
    )
)


server <- function(input, output, session) {

  RV <- reactiveValues()
  RV$CurrentJson=NULL
  RV$AllSiteDataList=NULL
  RV$CurrenSiteID=NULL
  RV$CurrentANSISResults=NULL
  RV$SiteLocations=NULL
  RV$CurrentHorizons=NULL
  RV$CurrentLabResults=NULL
  RV$CurrentDenormMorphResults=NULL
  RV$CurrentLabData=NULL
  RV$CurrentFileName=NULL

  RV$CurrentSiteVisitTable=NULL



  output$wgtSiteVisitDescription = renderText({



    req(RV$CurrentANSISResults, input$wgtSiteIDs)
    sv <- RV$CurrentANSISResults[[input$wgtSiteIDs]]$siteVisitTable
    #write.csv(sv, 'c:/temp/sv2.csv')

    print(sv)

    t <- '<H3>Site Visit</H3> <table style="width: 100%;">'

    t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Site Name : ', fields1 ='S_ID', title2 = 'Organisation : ', fields2 ='AGENCY_CODE'))
    t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Date Described : ', fields1 ='S_DATE_DESC', title2 = 'Observation Type : ', fields2 ='O_TYPE'))
    #t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Landform Pattern : ', fields1 ='S_PATT_TYPE', title2 = 'Landform Element : ', fields2 ='S_ELEM_TYPE'))
    t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Slope : ', fields1 ='S_SLOPE', title2 = 'Landform Element : ', fields2 ='S_ELEM_TYPE'))
    t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Element Height : ', fields1 ='S_ELEM_HEIGHT;S_ELEM_HEIGHT_UNIT', title2 = 'Morphological Type : ', fields2 ='S_MORPH_TYPE'))
    t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Site Height : ', fields1 ='O_ELEVATION;O_ELEVATION_UNIT;O_ELEVATION_EVAL', title2 = 'Runoff : ', fields2 ='O_RUNOFF'))
    t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Rock Outcrop : ', fields1 ='RO_ABUN;RO_LITH', title2 = 'Substrate : ', fields2 ='O_SB_LITH;O_SB_DEPTH;O_SB_DEPTH_UNIT'))
    t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Surface Coarse Frags : ', fields1 ='SCF_ABUN;SCF_SIZE;SCF_SHAPE;SCF_LITH', title2 = 'Soil Classification : ', fields2 ='O_CLASSIFICATION'))

    t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Surface Condition : ', fields1 ='SCON_STAT', title2 = 'Soil Disturbance : ', fields2 ='O_SOIL_DISTURB'))


   # t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Substrate', fields1 ='O_SB_LITH;O_SB_DEPTH;O_SB_DEPTH_UNIT', title2 = '', fields2 =''))


    t <- paste0(t, '</table>')

    return(t)

  })



  # observeEvent(input$wgtBtnScreenShot, {
  #   screenshot( id ='wgtProfile',  download = T)
  # })

  output$UI_LabResultsPlots = renderPlot({

    req(input$wgtLabResultsSelect, RV$CurrentLabResults)

    df <- data.frame(ud=RV$CurrentLabResults$ud, ld=RV$CurrentLabResults$ld, vals=RV$CurrentLabResults[input$wgtLabResultsSelect])
    colnames(df) <- c('ud', 'ld',  'strV')
    c <- RV$CurrentLabResults[input$wgtLabResultsSelect]
    idxs <- which( df$strV !='')
    df <- df[idxs,]


    #colnames(c) <-'strV'
    depths <- data.frame(ud=df$ud, ld=df$ld, check.names = F)
    colnames(depths) <- c('ud', 'ld')

    out <- strsplit(as.character(df$strV),';')
    mdf <- do.call(rbind, out)
    df2 <- data.frame(apply(mdf, 2, function(x) as.numeric(as.character(x))))

    vatt=input$wgtLabResultsSelect

    if(nrow(depths) > 0){
        plotLabResults(depths = depths, vals=df2, att=vatt)
    }
  }, height = 700, width = 400)



  observe({
    req(input$wgtSiteIDs, RV$CurrentLabResults, RV$CurrentANSISResults)

    dfDenorm <- RV$CurrentANSISResults[[input$wgtSiteIDs]]$data

    if(!is.null(dfDenorm)){

        if(nrow(dfDenorm) > 0){

          labData <- dfDenorm[dfDenorm$propType=='Lab', ]

          if(nrow(labData)==0)
          {
            df=data.frame()
          }else{
            bt <- generateBlankTable(dfDenorm=labData)
            labs <- populateTable(blankTable=bt, dfDenorm=labData, decode=F)
            df <- labs
          }
        }

        nms <- sort( colnames(df)[3:ncol(df)])
        RV$CurrentLabResults <- df
        updateSelectInput(session, "wgtLabResultsSelect", choices = nms)

    }

  })


  output$UI_LabPlotsAllLabsTable = renderRHandsontable({

    req(input$wgtSiteIDs)
      rhandsontable(RV$CurrentLabResults,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F) %>% hot_cols(fixedColumnsLeft = 3)
  })



  output$sctPlot <- renderRglwidget({

    req(input$wgtSiteIDs, RV$SiteLocations)
    try(close3d())
    #options(rgl.printRglwidget = F)

    # X <- 152.909
    # Y <- -28.478

    sid <- input$wgtSiteIDs
    idxs <- which(RV$SiteLocations==sid)
    rec<-RV$SiteLocations[idxs,]
     X <- rec$X
     Y <- rec$Y

     pts <- as.matrix(data.frame(x=c(X), y=c(Y)))
     if(nrow(pts)==1){

           hgt <- terra::extract(dem, pts)

          e <- terra::ext(X-0.1, X+0.1, Y-0.1, Y+0.1)

          demc <- terra::crop(dem, e)
          mm <- minmax(demc)
          minh <- mm[1,]
          maxh <- mm[2,]

          hdif <- maxh-minh
          scl <- floor( hdif / 7)
          scl=100

          elmat = matrix(demc, nrow=ncol(demc),ncol=nrow(demc)) * 20

          rgbc <-  terra::crop(rgb, e)

          tf <- tempfile(fileext = '.png')
          png(filename=tf, width = ncol(demc), height = nrow(demc))
          par(mar = c(0, 0, 0, 0))
          plotRGB(rgbc, r=1, g=2, b=3, stretch="lin")
          dev.off()
          overlay_img <- png::readPNG(tf)


          elmat_3d <- elmat %>%
            sphere_shade(sunangle = 35) %>%
            add_water(detect_water(elmat), color="desert") %>%
            add_shadow(ray_shade(elmat,zscale=5,maxsearch = 300),0.5) %>%
            add_overlay(overlay_img, alphalayer = 1) %>%
            plot_3d(elmat,zscale=scl,fov=0,theta=0,zoom=1,phi=25, windowsize = c(ncol(demc)*4, height = nrow(demc)*4),
                    soil = T, plot_new=F, clear_previous=T, close_previous = TRUE,)

          render_label(elmat, lat = Y, long = X, extent=e,  altitude= hgt + 5000, text = sid,
                        zscale = scl, textcolor = "red", linecolor="red",
                        dashed = F, clear_previous = TRUE)
           render_scalebar(limits=c(0, 5, 10),label_unit = "km",position = "W", y=50, color_first='sandybrown',
                           scale_length = c(0.33,1))
           render_compass(position = "N", compass_radius=10)
          unlink(tf)
          rglwidget(altText = "Hello")
     }
  })


  output$UI_SiteDataTables_SiteVisit = renderRHandsontable({

    req(input$wgtSiteIDs)
    dfDenorm <- RV$CurrentANSISResults[[input$wgtSiteIDs]]$siteVisitTable
    RV$CurrentSiteVisitTable <- dfDenorm
    rhandsontable(dfDenorm,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F) %>% hot_cols(fixedColumnsLeft = 3)

  })


  output$UI_SiteDataTables_Morphology = renderRHandsontable({

    req(input$wgtSiteIDs)
    dfDenorm <- RV$CurrentANSISResults[[input$wgtSiteIDs]]$data

    if(nrow(dfDenorm) > 0){

      morphData <- dfDenorm[dfDenorm$propType=='Horizons', ]
      bt <- generateBlankTable(dfDenorm=morphData)
      morph <- populateTable(blankTable=bt, dfDenorm=morphData, decode=F)

      if(nrow(morph)==0)
      {
        df=data.frame()
      }else{
        df <- morph
      }
      rhandsontable(df,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F) %>% hot_cols(fixedColumnsLeft = 3)
    }
  })


  output$UI_SiteDescription_LabResults = renderRHandsontable({

    req(input$wgtSiteIDs, RV$CurrentANSISResults)
    dfDenorm <- RV$CurrentANSISResults[[input$wgtSiteIDs]]$data

    if(!is.null(dfDenorm)){
          if(nrow(dfDenorm) > 0){

            labData <- dfDenorm[dfDenorm$propType=='Lab', ]


            if(nrow(labData)==0)
            {
              df=data.frame()
            }else{
              bt <- generateBlankTable(dfDenorm=labData)
              labs <- populateTable(blankTable=bt, dfDenorm=labData, decode=F)
              df <- labs
            }

            rhandsontable(df,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F) %>% hot_cols(fixedColumnsLeft = 3)
          }
    }else{
      df=data.frame()
      rhandsontable(df,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F) %>% hot_cols(fixedColumnsLeft = 3)
    }
  })

  output$UI_SiteDataTables_LabResults = renderRHandsontable({

    req(input$wgtSiteIDs)
    dfDenorm <- RV$CurrentANSISResults[[input$wgtSiteIDs]]$data

    if(nrow(dfDenorm) > 0){

      labData <- dfDenorm[dfDenorm$propType=='Lab', ]


      if(nrow(labData)==0)
      {
        df=data.frame()
      }else{
        bt <- generateBlankTable(dfDenorm=labData)
        labs <- populateTable(blankTable=bt, dfDenorm=labData, decode=F)
        df <- labs
      }

    }else{
      df=data.frame()
    }

    rhandsontable(df,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F) %>% hot_cols(fixedColumnsLeft = 3)

  })


  ####  Render profile diagram   ####

  output$wgtProfile = renderPlot({
    req(RV$CurrentHorizons)

    if(nrow(RV$CurrentHorizons) > 0){
      df <- RV$CurrentHorizons
      getSoilProfileDiagram('test', df)
    }
  }, height = 700, width = 400)





  output$wgtProfileDescription = renderText({

     req(RV$CurrentANSISResults, input$wgtSiteIDs)

     dfDenorm <- RV$CurrentANSISResults[[input$wgtSiteIDs]]$data
     print(head(dfDenorm, 100))

     if(!is.null(dfDenorm)){

     indf <- dfDenorm[dfDenorm$propType=='Horizons', ]

     if(nrow(indf) > 0){

     labData <- dfDenorm[dfDenorm$propType=='Lab', ]


     if(nrow(labData)==0)
     {
       labsTable=NULL
     }else{
       bt <- generateBlankTable(dfDenorm=labData)
       labs <- populateTable(blankTable=bt, dfDenorm=labData, decode=F)
       labsTable <- labs
     }

     RV$CurrentLabResults <-  labsTable


  bt <- generateBlankTable(dfDenorm=indf)
  hs <- populateTable(blankTable=bt, dfDenorm=indf, decode=T)



  RV$CurrentHorizons <- hs

  t <- '<H3>Horizons</H3><table>'
  for (i in 1:nrow(hs)) {

    rec <- hs[i,]

    hname <- getHTMLText(rec, 'H_DESIG_MASTER', bold=T)
    tex <- getHTMLText(rec, 'H_TEXTURE', suffix =';')
    col <- getHTMLText(rec, 'COL_HUE_VAL_CHROM', suffix =';')

    str_grade <- getHTMLText(rec, 'STR_PED_GRADE', suffix =' ')
    str_size <- getHTMLText(rec, 'STR_PED_SIZE', suffix =' ')
    str_type <- getHTMLText(rec, 'H_TEXSTR_PED_TYPETURE', suffix ='; ')
    hstructure <- paste0(' ', str_grade,  str_size, str_type)
    if(hstructure !=' '){hstructure <- paste0(hstructure, 'structure; ')}

    cf_abun <- getHTMLText(rec, 'CF_ABUN', suffix =' ')
    cf_size <- getHTMLText(rec, 'CF_SIZE', suffix =' ')
    cf_shape <- getHTMLText(rec, 'CF_SHAPE', suffix =' ')
    cf_lith <- getHTMLText(rec, 'CF_LITH', suffix =' ')
    cfs <- paste0(' ', cf_abun,  cf_size, cf_shape, cf_lith)
    if(cfs!=' '){cfs <- paste0(cfs, 'coarse fragments; ')}

    SEG_ABUN <- getHTMLText(rec, 'SEG_ABUN', suffix =' ')
    SEG_NATURE <- getHTMLText(rec, 'SEG_NATURE', suffix =' ')
    SEG_FORM <- getHTMLText(rec, 'SEG_FORM', suffix =' ')
    SEG_SIZE <- getHTMLText(rec, 'SEG_SIZE', suffix =' ')
    SEG_STRENGTH <- getHTMLText(rec, 'SEG_STRENGTH', suffix =' ')
    segs <- paste0(' ', SEG_ABUN,  SEG_NATURE, SEG_FORM, SEG_SIZE, SEG_STRENGTH)
    if(segs!=' '){segs <- paste0(segs, 'segregations; ')}

    PORE_ABUN <- getHTMLText(rec, 'PORE_ABUN', suffix =' ')
    PORE_DIAMETER <- getHTMLText(rec, 'PORE_DIAMETER', suffix =' ')
    pores<- paste0(' ', PORE_ABUN,  PORE_DIAMETER)
    if(pores!=' '){pores <- paste0(pores, 'pores; ')}

    ROOT_ABUN <- getHTMLText(rec, 'ROOT_ABUN', suffix =' ')
    ROOT_SIZE <- getHTMLText(rec, 'ROOT_SIZE', suffix =' ')
    roots<- paste0(' ', ROOT_ABUN,  ROOT_SIZE)
    if(roots!=' '){roots <- paste0(roots, 'roots; ')}

    STRG_CLASS <- getHTMLText(rec, 'STRG_CLASS', suffix =' ')
    strength <- paste0(' ', STRG_CLASS)
    if(strength!=' '){strength <- paste0(strength, 'consistence; ')}

    MOTT_ABUN <- getHTMLText(rec, 'MOTT_ABUN', suffix =' ')
    MOTT_SIZE <- getHTMLText(rec, 'MOTT_SIZE', suffix =' ')
    MOTT_CONTRAST <- getHTMLText(rec, 'MOTT_CONTRAST', suffix =' ')
    MOTT_COLOUR <- getHTMLText(rec, 'MOTT_COLOUR', suffix =' ')
    motts <- paste0(' ', MOTT_ABUN,  MOTT_SIZE, MOTT_CONTRAST, MOTT_COLOUR)
    if(motts!=' '){motts <- paste0(motts, 'mottles; ')}


    #desc <- paste0(hname, ' ', col, colcode, tex, hstructure, ph, ec, ocf, oseg, omot, bdy)
    desc <- paste0(hname, ' : ',  tex, ' ', col, strength, hstructure, cfs, motts, segs, pores, roots )
    td <- '<td style="padding: 8px;">'
    t <- paste0(t, '<tr>', td, rec['ud'], '</td>', td, ' to </td>', td, rec['ld'], '</td><td>', desc, '</td></tr>' )

  }
  t <- paste0(t, '</table>')

   return(t)
}
else{
     return("<H3>There is no profile morphology data available for this site")
}

     }
  })



  output$mainMap <- renderLeaflet({
    req(RV$SiteLocations)


    sfdf <- st_as_sf(RV$SiteLocations, coords = c("X", "Y"), crs = 4326)
    b <- st_bbox(sfdf)

    m <-leaflet() %>%
      clearMarkers() %>%
      addTiles(group = "Map") %>%
      addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = F), group = "Satellite") %>%
      addMouseCoordinates()  %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to full extent",
        onClick=JS("function(btn, map){ map.setView({lon: 135, lat: -28}, 3); }"))) %>%
    #  fitBounds(Ausminx, Ausminy, Ausmaxx, Ausmaxy) %>%
      addLayersControl(
        baseGroups = c("Map", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%

    fitBounds(lng1 = as.numeric(b$xmin), lng2 = as.numeric(b$xmax), lat1 = as.numeric(b$ymin), lat2 = as.numeric(b$ymax)) %>%
     addCircleMarkers(data=sfdf, radius=6, color="black", stroke=FALSE, fillOpacity=0.5, group="locations", layerId = sfdf$sid, label=sfdf$sid)
  })

  acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=6, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")

  observeEvent(input$mainMap_marker_click, { # update the map markers and view on map clicks
    p <- input$mainMap_marker_click
    proxy <- leafletProxy("mainMap")
    if(p$id=="Selected"){
      proxy %>% removeMarker(layerId="Selected")
    } else {
      updateSelectInput(session, "wgtSiteIDs", selected=p$id)
      proxy %>% acm_defaults(p$lng, p$lat)
    }
  })




  # observe({
  #   updateSelectInput(session, "wgtSiteIDs", choices=names(RV$CurrentANSISResults))
  # })
  #

  observe({
    updateSelectInput(session, "wgtSiteIDs", choices=names(RV$CurrentANSISResults))
  })



####  Data Downloads   ##################################

  output$wgtDownloadCSV <- downloadHandler(

    filename = function() {

      fn='def.csv'
      if(input$wgtDownloadOption=='Current Site Horizons as CSV'){
        fn <- paste0(input$wgtSiteIDs, '-Horizons.csv')
      }
      else if(input$wgtDownloadOption=='Current Site Visit as CSV'){
        fn <- paste0(input$wgtSiteIDs, '-SiteVisit.csv')
      }
      else if(input$wgtDownloadOption=='Current Site Lab Results as CSV'){
        fn <- paste0(input$wgtSiteIDs, '-LabResults.csv')
      }
      else if(input$wgtDownloadOption=='All of Current Site as CSV'){
        fn <- paste0(input$wgtSiteIDs, '-SiteData.csv')
      }
      else if(input$wgtDownloadOption=='All Site Data as CSV'){
        fn <- paste0(RV$CurrentFileName, '_AllSiteData.csv')
      }
      else if(input$wgtDownloadOption=='All Sites as GIS Point Data'){
        fn <- paste0(RV$CurrentFileName, '_GISPoints.geojsn')
      }

      fn
    },
    content = function(file) {


      if(input$wgtDownloadOption=='Current Site Horizons as CSV'){
        dfDenorm <- RV$CurrentANSISResults[[input$wgtSiteIDs]]$data

        df=data.frame()
        if(nrow(dfDenorm) > 0){

          morphData <- dfDenorm[dfDenorm$propType=='Horizons', ]
          bt <- generateBlankTable(dfDenorm=morphData)
          morph <- populateTable(blankTable=bt, dfDenorm=morphData, decode=F)

          if(nrow(morph)==0)
          {
            df=data.frame()
          }else{
            df <- morph
          }
        }
        write.csv( df, file, quote = F, row.names = F)
      }
      else if(input$wgtDownloadOption=='Current Site Visit as CSV'){
        sv <- RV$CurrentANSISResults[[input$wgtSiteIDs]]$siteVisitTable
        write.csv( sv, file, quote = F, row.names = F)
      }
      else if(input$wgtDownloadOption=='Current Site Lab Results as CSV'){
        dfDenorm <- RV$CurrentANSISResults[[input$wgtSiteIDs]]$data
        df=data.frame()
        if(nrow(dfDenorm) > 0){
          labData <- dfDenorm[dfDenorm$propType=='Lab', ]
          if(nrow(labData)>0)
          {
            bt <- generateBlankTable(dfDenorm=labData)
            labs <- populateTable(blankTable=bt, dfDenorm=labData, decode=F)
            df <- labs
          }
        }
        write.csv( df, file, quote = F, row.names = F)
      }
      else if(input$wgtDownloadOption=='All of Current Site as CSV'){
        #dfDenorm <- RV$CurrentANSISResults[[input$wgtSiteIDs]]$data
        sdf <- makeSiteCSV(sl = RV$CurrentANSISResults[[input$wgtSiteIDs]])
        write.csv( sdf, file, quote = F, row.names = F)
      }
      else if(input$wgtDownloadOption=='All Site Data as CSV'){
        ddf <- makeAllDataCSV(RV$CurrentANSISResults)
        write.csv( ddf, file, quote = F, row.names = F)
      }
      else if(input$wgtDownloadOption=='All Sites as GIS Point Data'){
        s = st_as_sf(RV$SiteLocations, coords = c("X", "Y"), crs = 3264)
        tf <- tempfile(fileext = ".geojson" )
        suppressMessages( st_write(s, tf))
        file.copy(tf, file)

      }

      session$onSessionEnded(function() {
        cat("Session Ended\n")
        unlink(paste0(tempdir(),'/*.geojson'))
        unlink(paste0(tempdir(),'/*.png'))
      })

    }
  )


  #####  Read in json from file in load if in develop mode  #####
   observe({

     if(devel){

       fn <- demoData
      # fn <- 'C:/temp/willonew.json'
       #fn <- 'C:/temp/onefromeachState.json'
       #fn <- 'C:/Users/sea084/Downloads/230348919aea3646c017e1cc89b4093cc874c9c03ea359955cf371deaa16b584Standardised (1).json'
       # fn <- 'C:/Projects/ANSIS/ANSISAPI/QueryResponses/4_ANSIS_Sites.json'
       # fn <- 'C:/Projects/ANSIS/ANSISAPI/QueryResponses/DarlingDowns.json'
       # fn <- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ShowANSISSites2/DemoData/224_ANSIS_Sites.json'


       jL <- openANSISJson(jsnFile = fn)


       RV$CurrentANSISResults <- jL$dfDenorm
       #saveRDS(RV$CurrentANSISResults, 'c:/temp/224sites.rds')
       RV$SiteLocations <- jL$locsDF
       #saveRDS(RV$SiteLocations , 'c:/temp/locations.rds')
       RV$CurrentJson <- jL$jsonList
       RV$CurrentFileName <- basename(fn)
     }
   })

  #####  Open from drop on File upload widget #######
  observe({
    req(input$wgtFile)

    updateSelectInput(session, 'wgtSiteIDs', selected = '')
    RV$CurrentANSISResults = NULL
    RV$SiteLocations = NULL
    RV$CurrentJson = NULL

    file <- input$wgtFile
    RV$CurrentFileName <- basename(file$datapath)

   jL <- openANSISJson(jsnFile=file$datapath)

   RV$CurrentANSISResults <- jL$dfDenorm
   RV$SiteLocations <- jL$locsDF
   RV$CurrentJson <- jL$jsonList

  })



  # output$UI_DataTable = renderRHandsontable({
  #   req(input$wgtSiteIDs)
  #
  #   dfDenorm <- RV$CurrentANSISResults[[input$wgtSiteIDs]]$data
  #
  #   #dfDenorm <- RV$CurrentANSISResults[[1]]
  #
  #   c('Horizons', 'Laboratory')
  #   if(input$wgtDataTypes=='Horizons'){
  #     indf<-dfDenorm[dfDenorm$propType=='Horizons', ]
  #   }else{
  #     indf<-dfDenorm[dfDenorm$propType=='Lab', ]
  #   }
  #
  #   bt <- generateBlankTable(dfDenorm=indf)
  #   tData <- populateTable(blankTable=bt, dfDenorm=indf)
  #   RV$CurrenTable <- tData
  #   if(nrow(tData) > 0){
  #     rhandsontable(tData,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F )
  #   }
  # })
  #


    output$UI_DataViewer = renderRHandsontable({
      req(input$wgtSiteIDs)
      dfDenorm <- RV$CurrentANSISResults[[input$wgtSiteIDs]]$data
      rhandsontable(dfDenorm,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F )
    })


    output$jsed <- renderJsonedit({

      req(RV$CurrentJson)
      jsonedit(
        RV$CurrentJson
        ,"onChange" = htmlwidgets::JS('function(after, before, patch){
        console.log( after.json )
      }')
      )

    })





}


shinyApp(ui = ui, server = server)
