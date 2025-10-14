options(rgl.useNULL=TRUE)

library(shiny)
library(rgl)
library(rayshader)
library(magrittr)
library(terra)

#Here, I load a map with the raster package.
loadzip = tempfile()
download.file("https://tylermw.com/data/dem_01.tif.zip", loadzip)
localtif = raster::raster(unzip(loadzip, "dem_01.tif"))
unlink(loadzip)

#And convert it to a matrix:
elmat = matrix(raster::extract(localtif,raster::extent(localtif),buffer=1000),
               nrow=ncol(localtif),ncol=nrow(localtif))


b <- rast('C:/Temp/image/Veg_Landsat30Bare_1.tif')
g <- rast('C:/Temp/image/Veg_Landsat30Bare_2.tif')
r <- rast('C:/Temp/image/Veg_Landsat30Bare_3.tif')

stk <- rast(list(b,g,r))

e <- terra::ext(152, 152.1, -29, -28.9 )
eb <- terra::ext(152, 153, -29, -28 )
c <- terra::crop(stk, e)
plotRGB(c)


writeRaster(stk, 'C:/Temp/image/AustRgb.tif', overwrite=T)


mbr <- rast('C:/Temp/image/rgb.tif')
c <- terra::crop(mbr, eb)
plotRGB(c)
writeRaster(c, 'C:/Temp/image/c.tif', overwrite=T)
