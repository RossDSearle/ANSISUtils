library(jsonlite)

source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ShowANSISSites2/parsing.R')

mps <- read.csv('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ShowANSISSites2/schemaFieldMapping2.csv')
mp <- unique(mps[mps$SchemaLocation=='Horizons' & mps$Domain!='',]$Domain)
CodesTable <<- read.csv('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ShowANSISSites2/ANSISCodes.csv')

#demoData <- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ShowANSISSites2/DemoData/4_ANSIS_Sites.json'
#demoData <- 'C:/Users/sea084/Downloads/230348919aea3646c017e1cc89b4093cc874c9c03ea359955cf371deaa16b584Standardised (1).json'
demoData <- 'C:/temp/willonew.json'

jsnFile <- 'C:/temp/willonew.json'
sl <- fromJSON(demoData , simplifyDataFrame = F)
r <- sl
s <- r$data[[1]]
sid <- getSiteID(siteAsList=s)
# parseANSISSiteVistToDenormalisedTable
layersTable <- parseANSISSiteLayersToDenormalisedTable(siteAsList=s)
siteVistTable <- parseANSISSiteVistToDenormalisedTable(siteAsList=s)


