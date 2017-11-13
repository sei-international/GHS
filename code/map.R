# Simple map of GHS adoption
# Steve Fick
# 2017 05 03

library(raster)
library(readxl)
library(data.table)

# output file
outfile <- paste0(outdir,'/figures/map ', format(Sys.time(), "%Y-%m-%d"), '.png')
outfile2 <- paste0(outdir, '/figures/map_robinsonVersion_', format(Sys.time(), "%Y-%m-%d"), '.png')
outfile <- gsub(' ', '_', outfile)
# input files
worldmap <- 'data/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp'

# load data
wm <- shapefile(worldmap)
d <- read.csv(file.path(outdir, 'data.csv'), check.names=F,strings=F)

setnames(d, c('Country code', 'GHS  implementation key (0-2)'), c('ISO3','GHS'))

#merge
wm$id <- 1:nrow(wm)
wm@data <- merge(wm@data, d[, c('ISO3', 'GHS')], by = 'ISO3', all.x = TRUE)
wm$GHS[which(is.na(wm$GHS))] <- -1
wm$GHS2 <- wm$GHS + 2
wm@data <- wm@data[order(wm$id),]

# play around with colors
colors = c( '-1' = 'gray', 
            '0' = 'red', 
            '1' = 'purple',
            '2' = 'blue')
colors = c('#f1eef6','#bdc9e1','#74a9cf','#0570b0')
colors = c('#edf8fb','#b3cde3','#8c96c6','#88419d')
bord = rgb(0,0,0,.2)

# generate background grid
r <- raster()
res(r) <- 20
R <- rasterToPolygons(r)


#plot
png( file = outfile, width = 13, height = 6.45, res = 500, units = 'in')
plot(R, col = grey(.95))
plot(wm, col = colors[wm$GHS2], border = bord, add =TRUE)
# plot(wm, col = wm$GHS2, border = bord, add =TRUE)
legend(-175,-25, legend = c( 'no implementation', 'partial implementation', 'full implementation'), fill = colors[-1], bg = 'white')
dev.off()

# other version


wmr <- spTransform(wm, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
RR <- spTransform(R, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

png( file = outfile2, width = 13, height = 6.45, res = 500, units = 'in')
#x11(width = 13, height = 6.45)
plot(RR, col = grey(.95), bord = grey(.9))
plot(wmr, col = colors[wm$GHS2], border = bord, add =TRUE)
# plot(wm, col = wm$GHS2, border = bord, add =TRUE)
legend(-19820459, -6568919, legend = c( 'no implementation', 'partial implementation', 'full implementation'), fill = colors[-1], bty = 'n')

dev.off()


