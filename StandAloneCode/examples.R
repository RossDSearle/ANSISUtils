source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ShowANSISSites2/StandAloneCode/parseANSISJson.R')

### some info tables to read in 
mps <<- read.csv('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ShowANSISSites2/schemaFieldMapping2.csv')
mp <<- unique(mps[mps$Domain!='' & mps$SchemaLocation=='Horizons', ]$Domain )
CodesTable <<- read.csv('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ShowANSISSites2/ANSISCodes.csv')


### The ANSIS JSON file to be parsed
jsnFile <- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ShowANSISSites2/DemoData/4_ANSIS_Sites.json'
jsnFile <- 'C:/Users/sea084/Downloads/8b8fbb9e6ea22752beda5f3baa0abc65d494fb2950021eaa5e825d6811183a6dStandardised.json'

jsnFile <- 'C:/Projects/ANSIS/V2/Responses/big.json'
jsnFile <- 'C:/Projects/ANSIS/V2/Responses/medium.json'

### this parses the ANSIS JSON file into a useable format
ao <- parseANSISJson(jsnFile, outDir='c:/temp')
ao <- parseANSISJson(jsnFile)


### Returns the available properties in the the JSON response for each property type
getAvailableProperties(anisObject=ao, propertyType = 'Lab')
getAvailableProperties(anisObject=ao, propertyType = 'SiteVisit')
getAvailableProperties(anisObject=ao, propertyType = 'Horizons')

### Return all of the available properties in the the JSON response for a given property type as a wide format CSV file including siteID, location and depths
makeWideTable(anisObject=ao, propertyType='Horizons', properties=NULL, decode=F)

makeWideTable(anisObject=ao, propertyType=NULL, properties=c('3A1'), decode=F)


