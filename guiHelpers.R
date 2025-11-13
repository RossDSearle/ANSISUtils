



openANSISJson <- function(jsnFile){

  sl <- fromJSON(jsnFile , simplifyDataFrame = F)

  #mps <- read.csv('C:/Projects/ANSIS/ANSISAPI/schemaFieldMapping2.csv')
  #mp <- unique(mps[mps$SchemaLocation=='Horizons' & mps$Domain!='',]$Domain)

  r <- sl

  sol <- list()
  nsites <- length(r$data)

  withProgress(message = 'Formating Your ANSIS Soil Data For Display : ', value = 0, {

    for (k in  1:length(r$data)) {

      s <- r$data[[k]]
      sid <- getSiteID(siteAsList=s)
     # parseANSISSiteVistToDenormalisedTable
      layersTable <- parseANSISSiteLayersToDenormalisedTable(siteAsList=s)
      siteVistTable <- parseANSISSiteVistToDenormalisedTable(siteAsList=s)

      loc <- getSiteLocation(siteAsList=s)
      pl <- list()
      pl$Site=sid
      pl$X=loc$X
      pl$Y=loc$Y
      pl$data <-  layersTable
      pl$siteVisitTable <- siteVistTable

      sol[[sid]] <- pl

      pr <-(k/nsites) * 100
      if (pr < 99) {
        status <- "Loading..."
      }else{
        status <- "Finished loading data"
      }
      incProgress(1/nsites, detail = paste("Site ", k, ' of ', nsites))
    }
  })

  #dfDenorm <- sol
    locsDF <- makeSitesLocationTableFromDataList(sol)
   jL <- list()
   jL$dfDenorm <- sol
   jL$locsDF <- locsDF
   jL$jsonList <- r

 return(jL)

}

getHTMLText <- function(rec, att, bold=F, suffix=''){

      t <- ''
      hbs <- ''
      hbe <- ''

      if(bold){
        hbs <- '<B>'
        hbe <- '</B>'
      }
    if(att %in% colnames(rec))  {

        if(rec[att] == ''){
           t=''
        }else{
           t=paste0(hbs, rec[att], hbe, suffix )
        }
    }
}


getSVHTMLText <- function(sv, title1, title2, fields1, fields2){

  pad='2'
  #b <-' border: 1px solid black;'
  b <-''
  tcWidth='20'
  vcWidth = '20'

  vs1 <- str_split(fields1,';')[[1]]
  ov1 <- ''
  for (i in 1:length(vs1)) {
    rec <- sv[sv$property==vs1[i], ]
    ov1 <- paste0(ov1, " ", rec$desc)
  }

  vs2 <- str_split(fields2,';')[[1]]
  ov2 <- ''
  for (i in 1:length(vs2)) {
    rec <- sv[sv$property==vs2[i], ]
    ov2 <- paste0(ov2, " ", rec$desc)
  }

  t <- ''
  t <- paste0(t, '<tr><td style="padding: ', pad, 'px; text-align: left; width: ', tcWidth, '%; ',b, '"><B>', title1, '</B></td><td style="padding: ', pad, 'px; text-align: left; width: ', vcWidth, '%;">', ov1, '</td>
                      <td style="width: 5%"></td>
                      <td style="padding: ', pad, 'px; text-align: left;  width: ', tcWidth, '%; ',b, '"><B>', title2, '</B></td style="padding: ', pad, 'px; text-align: left;  width: ', vcWidth, '%;"><td>', ov2, '</td>')
 return(t)
}





