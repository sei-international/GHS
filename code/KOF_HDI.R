library(readxl)
library(plotly)

outfile <- paste0(outdir, '/figures/KOF total x HDI ',  format(Sys.time(), "%Y-%m-%d"),'.png')
outfile2 <- paste0(outdir, '/figures/KOF Political x HDI ',  format(Sys.time(), "%Y-%m-%d"),'.png')
outfile3 <- paste0(outdir, '/figures/KOF Social x HDI ',  format(Sys.time(), "%Y-%m-%d"),'.png')
outfile4 <- paste0(outdir, '/figures/KOF Economioc x HDI ',  format(Sys.time(), "%Y-%m-%d"),'.png')

d <- read.csv(file.path(outdir,'data.csv'), check.names=F)
d$GHS <- d$`GHS  implementation`
d$ghs <- d$GHS >0
i <- which(d$ghs)

d$GDPP <- d$`GDP per Capita PPP for 2015`
d$GDPT <- d$`Total GDP, PPP, for 2015`
d$HDI <- d$`HDI for 2015`
d$trade <- as.numeric(as.character(d$`Trade Open-ness for 2013`))

d$kof <- d$`KOF index, total`
d$kofPol <- d$`KOF sub-index C`
d$kofSoc <- d$`KOF sub-index B`
d$kofEco <- d$`KOF sub-index A`


xlb <- 'HDI (2015)'
ylb <- 'KOF globalization'



png( file = outfile, width = 7, height = 7, units = 'in', res = 300)

plot(d$kof ~ d$HDI, pch = c(1,2)[as.numeric(d$ghs) + 1], xlab = xlb, ylab = ylb)

points(d$kof ~ d$HDI, pch = c(16,17)[as.numeric(d$ghs) + 1], col = rgb(as.numeric(d$ghs),0,0,.2))
legend('topleft', legend = c('non GHS', 'GHS'), pch = c(16,17), col = c(rgb(0,0,0,.3), rgb(1,0,0,.2)))

legend('topleft', legend = c('non GHS', 'GHS'), pch = c(1,2))

dev.off()



xlb <- 'HDI (2015)'
ylb <- 'KOF political globalization'


png( file = outfile2, width = 7, height = 7, units = 'in', res = 300)

plot(d$kofPol ~ d$HDI, pch = c(1,2)[as.numeric(d$ghs) + 1], xlab = xlb, ylab = ylb)

points(d$kofPol ~ d$HDI, pch = c(16,17)[as.numeric(d$ghs) + 1], col = rgb(as.numeric(d$ghs),0,0,.2))
legend('topleft', legend = c('non GHS', 'GHS'), pch = c(16,17), col = c(rgb(0,0,0,.3), rgb(1,0,0,.2)))

legend('topleft', legend = c('non GHS', 'GHS'), pch = c(1,2))

dev.off()


ylb <- 'KOF social globalization'


png( file = outfile3, width = 7, height = 7, units = 'in', res = 300)

plot(d$kofSoc ~ d$HDI, pch = c(1,2)[as.numeric(d$ghs) + 1], xlab = xlb, ylab = ylb)

points(d$kofSoc ~ d$HDI, pch = c(16,17)[as.numeric(d$ghs) + 1], col = rgb(as.numeric(d$ghs),0,0,.2))
legend('topleft', legend = c('non GHS', 'GHS'), pch = c(16,17), col = c(rgb(0,0,0,.3), rgb(1,0,0,.2)))

legend('topleft', legend = c('non GHS', 'GHS'), pch = c(1,2))

dev.off()


ylb <- 'KOF economic globalization'


png( file = outfile4, width = 7, height = 7, units = 'in', res = 300)

plot(d$kofEco ~ d$HDI, pch = c(1,2)[as.numeric(d$ghs) + 1], xlab = xlb, ylab = ylb)

points(d$kofEco ~ d$HDI, pch = c(16,17)[as.numeric(d$ghs) + 1], col = rgb(as.numeric(d$ghs),0,0,.2))
legend('topleft', legend = c('non GHS', 'GHS'), pch = c(16,17), col = c(rgb(0,0,0,.3), rgb(1,0,0,.2)))

legend('topleft', legend = c('non GHS', 'GHS'), pch = c(1,2))

dev.off()




