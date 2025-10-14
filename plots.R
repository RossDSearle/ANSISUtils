getSoilProfileDiagram <- function(sid, p){
  
  if('COL_HUE_VAL_CHROM' %in% colnames(p)){
    
   
    col <- p$COL_HUE_VAL_CHROM
    col2 <- str_remove_all(col, ' ')
    col3 <- str_remove_all(col2, '/')
    hue <- str_sub(col3, start = 1, end = nchar(col3) - 2)
    value <- str_sub(col3, start = nchar(hue)+1, end = nchar(hue)+1)
    chroma <- str_sub(col3, start = nchar(hue)+2, end = nchar(hue)+2)
  }else{
    hue <- ''
    value <- ''
    chroma <- ''
  }
  
  if('H_DESIG_MASTER' %in% colnames(p)){
    hname=p$H_DESIG_MASTER
  }else{
    hname=''
  }
  
 
  
  sp <-  data.frame(ud=as.numeric(p$ud)*100, ld=as.numeric(p$ld)*100, hname=hname, hue, value, chroma)
  
  depth = max(sp$ld)
  # par(oma=c(0,0,0,0)) # all sides have 3 lines of space
  par(mar=c(0.1,3,6,2))
  
  lableft = 1.5
  profleft = 2.5
  profWidth = 2
  
  plot.new(); plot.window(xlim=c(1,10),ylim=rev(c(0,depth)) )
  text(lableft, sp$ud[1], 'Depth (cm)')
  for (i in 1:nrow(sp)) {
    rec=sp[i,]
    
    tv <- str_sub(rec$hue, 1,1)
    
    if(!is.na(as.numeric(tv))){
      mnsl <- paste0(rec$hue, ' ', rec$value, '/', rec$chroma)
      col <- munsell::mnsl2hex(in_gamut(mnsl, fix = T))
    }else{
      col='white'
    }
    rect(xleft = profleft, xright = profleft+profWidth, ybottom = rec$ld,   ytop = rec$ud, col=col)
    text(lableft, rec$ld, rec$ld, cex = 1, font=1)
    text(profleft + profWidth/2, rec$ud +  ((rec$ld-rec$ud)/2), rec$hname, cex = 1, font=2, col='gray')
    
  }
} 


plotLabResults <- function(depths, vals, att){
      
      xBound <-  max(vals)  + (max(vals) * 0.2)
      depth <- max(as.numeric(depths$ld))
      par(oma=c(0,0,0,0)) 
      par(mar=c(5,5,3,2))
      plot( 0, type="n",  col.main = 'blue', cex.main=2,
            #xlab= paste0( inDF$Units[1]), 
            ylab='Soil Depth (m)',
            xlab='Value',
            yaxs = "i", xaxs = "i", xlim = c(0, xBound), ylim = rev(range(c(0,depth))),
            cex.lab = 2,
            main = paste0( att)
      )
      
      cols =  brewer.pal(ncol(vals), "Dark2")
      
      for (s in 1:ncol(vals)) {
        mid <-  depths$ud + (depths$ld - depths$ud)/2
        vc <- vals[s]
        n <- data.frame(x=vc, y=mid)
        lines(n, lwd=3, pch=19, col=cols[s])
      }
}


