


sl <- fromJSON('C:/Projects/ANSIS/ANSISAPI/QueryResponses/allButPermNoRestricted224.json' , simplifyDataFrame = F)

mps <- read.csv('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ShowANSISSites2/schemaFieldMapping2.csv')

CodesTable <- read.csv('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ShowANSISSites2/ANSISCodes.csv')

sv <- sl$data[[89]]


s <- r$data[[k]]
sid <- getSiteID(siteAsList=s)
