

# makeAllDataCSV <- function(allsites){
#
# alldf <- data.frame()
#   for (i in 1:length(allsites)) {
#     s <- allsites[[i]]
#     sitedf<- makeSiteCSV(sl=s)
#     alldf <- rbind(alldf, sitedf)
#   }
#
#   return(alldf)
# }
#
# makeSiteCSV <- function(sl){
#
#   s<-sl
#   odf <- data.frame()
#
#   sid <- s$Site
#   head(s$data)
#   sdf <- data.frame(ud='', ld='', property='Location', propType='SiteVisit', field=c('Longitude, Latitude'),  value=c(s$X, s$Y), desc='')
#   odf <- rbind(odf, sdf)
#   svdf <- data.frame(ud='', ld='', s$siteVisitTable)
#   odf <- rbind(odf, svdf)
#   odf <- rbind(odf, s$data)
#   sitedf <- data.frame(sid, odf)
#   return(sitedf)
# }
#
#
# makeShapefile <- function(indf){
#
#   cols <- sort(unique(indf$Method))
#   odf <- unique(indf[c(4,5)])
#
#
#   for (i in 1:length(cols)) {
#     prop=as.character(cols[i])
#     idxs <- which(indf$Method==prop)
#     df <- indf[idxs,]
#
#     rdf <- data.frame(UD=df$UD, LD=df$LD, Value=df$Value)
#     odf <- merge(odf, rdf, by=c('UD','LD'), all=T)
#     colnames(odf)[ncol(odf)] <- prop
#   }
#   odf
#
#   sfdf <- st_as_sf( odf, wkt = 'Loc')
#
#   return(odf)
# }
#
#
