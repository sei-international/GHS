library(readxl)
library(plotly)

outfile <- paste0(outdir,'/figures/GDP x GHS ',  format(Sys.time(), "%Y-%m-%d"),'.png')
outfile2 <- paste0(outdir,'/figures/HDI + Trade x GHS ',  format(Sys.time(), "%Y-%m-%d"),'.png')

d <- read.csv(file.path(outdir,'data.csv'), check.names=F)

d$GHS <- d$`GHS  implementation`
d$ghs <- d$GHS >0
i <- which(d$ghs)

d$GDPP <- d$`GDP per Capita PPP for 2015`
d$GDPT <- d$`Total GDP, PPP, for 2015`
d$HDI <- d$`HDI for 2015`
d$trade <- as.numeric(as.character(d$`Trade Open-ness for 2013`))

set.seed(100)
d$x <- d$ghs+1+ rnorm(n = nrow(d), 0,.1) 

d$dstatus <- factor(d$Dev_status, levels = c('Least Developed', 
'Other Low Income', 'Lower Middle Income', 'Upper Middle Income', 'Non-DAC'), ordered = TRUE)

shapes <- c(16, 16,16,16,16)

png(outfile, width = 12, height = 7, units = 'in', res = 300 )

par(mfrow = c(1,2))



plot(d$GDPP ~ as.factor(d$ghs), border = rgb(0,0,0,.3), outline = F, xlab = '', ylab = 'GDP per capita (2015)', xaxt = 'n', log = 'y')

points(d$GDPP ~ d$x, pch = shapes[d$dstatus], col = rgb(0,0,0,.2) )
points(d$GDPP ~ d$x, pch = 1, col = rgb(0,0,0,.5) , lwd = 1.3)

tst <- t.test(d$GDPP[i], d$GDPP[-i])
#tst <- t.test(d$GDPP[i], d$GDPP[-i], var.equal = TRUE)
legend('bottomright', legend =  c( paste0( 't: \t', round(tst$statistic,2)), paste0('df: \t', round(tst$parameter,1)), paste0('p \t', '< .001***')))

# text(1.4, 50000, paste0( 't: ', round(tst$statistic,2), ' \n df: ', round(tst$parameter,1), ' \np ', '< .001***') )
# axis(1, at = 1:2,tck=0, labels = c('no GHS', 'GHS'), cex.axis = 1)



plot(d$GDPT ~ as.factor(d$ghs), border = rgb(0,0,0,.3), outline = F, xlab = '', ylab = 'GDP total (2015)', xaxt = 'n', log = 'y')

points(d$GDPT ~ d$x, pch = shapes[d$dstatus], col = rgb(0,0,0,.2) )
points(d$GDPT ~ d$x, pch = 1, col = rgb(0,0,0,.5) , lwd = 1.3)

tst <- t.test(d$GDPT[i], d$GDPT[-i])
#tst <- t.test(d$GDPT[i], d$GDPT[-i], var.equal = TRUE)
# text(1.4, 1e12, paste0( 't: ', round(tst$statistic,2), ' \n df: ', round(tst$parameter,1), ' \np = ', round(tst$p.value,3)) )
# axis(1, at = 1:2,tck=0, labels = c('no GHS', 'GHS'), cex.axis = 1)

# legend('bottomright', legend = levels(d$dstatus), pch = shapes)

legend('bottomright', legend =  c( paste0( 't: \t', round(tst$statistic,2)), paste0('df: \t', round(tst$parameter,1)), paste0('p: \t', round(tst$p.value,3))))


dev.off()

############

png(outfile2, width = 12, height = 7, units = 'in', res = 300 )

par(mfrow = c(1,2))


plot(d$HDI ~ as.factor(d$ghs), border = rgb(0,0,0,.3), outline = F, xlab = '',ylim =c(min(d$HDI,na.rm =TRUE), max(d$HDI, na.rm =TRUE)), ylab = 'HDI (2015)', xaxt = 'n')
points(d$HDI ~ d$x, pch = shapes[d$dstatus] )

legend('bottomright', legend = levels(d$dstatus), pch = shapes)

tst <- t.test(d$HDI[i], d$HDI[-i])
text(1.4, .9, paste0( 't: ', round(tst$statistic,2), ' \n df: ', round(tst$parameter,1), ' \np ', '< .001***') )
axis(1, at = 1:2,tck=0, labels = c('no GHS', 'GHS'), cex.axis = 1)

plot(d$trade ~ as.factor(d$ghs), border = rgb(0,0,0,.3), outline = F, xlab = '',ylim =c(min(d$trade,na.rm =TRUE), max(d$trade, na.rm =TRUE)), ylab = 'Trade Open-ness ( Imports + Exports / GDP ; 2013)', xaxt = 'n')
points(d$trade ~ d$x, pch = shapes[d$dstatus] )

tst <- t.test(d$trade[i], d$trade[-i])
text(1.4, 350, paste0( 't: ', round(tst$statistic,2), ' \n df: ', round(tst$parameter,1), ' \np = ', round(tst$p.value,3)) )
axis(1, at = 1:2,tck=0, labels = c('no GHS', 'GHS'), cex.axis = 1)


dev.off()


###########


