library(readxl)
library(plotly)

outfile <- paste0(outdir,'/figures/KOF political x GHS ',  format(Sys.time(), "%Y-%m-%d"),'.png')


d <- read.csv(file.path(outdir,'data.csv'), check.names=F)


d$GHS <- d$`GHS  implementation`
d$GHS <- d$`GHS  implementation`
d$ghs <- d$GHS >0
i <- which(d$ghs)


set.seed(501)
d$x <- d$ghs+1+ rnorm(n = nrow(d), 0,.1) 

d$dstatus <- factor(d$Dev_status, levels = c('Least Developed', 'Other Low Income', 'Lower Middle Income', 'Upper Middle Income', 'Non-DAC'), ordered = TRUE)



# multi-panel
#layout(matrix(1:4,2,2))

shapes <- c(1,2, 3, 4, 20)
shapes <- rep(16, 5)



png( file = outfile, width = 7, height = 7, units = 'in', res = 300)


plot(d$`KOF sub-index C` ~ as.factor(d$ghs), border = rgb(0,0,0,.3), outline = F, xlab = '', ylab = 'KOF Political Globalization Score', xaxt = 'n')
points(d$`KOF sub-index C` ~ d$x, pch = shapes[d$dstatus], col = rgb(0,0,0,.3))
points(d$`KOF sub-index C` ~ d$x, pch = 1, col = rgb(0,0,0,.7))

tst <- t.test(d$`KOF sub-index C`[i], d$`KOF sub-index C`[-i], var.equal = TRUE)
tst <- t.test(d$`KOF sub-index C`[i], d$`KOF sub-index C`[-i])

legend('bottomright', legend =  c( paste0( 't: \t', round(tst$statistic,2)), paste0('df: \t', round(tst$parameter,1)), paste0('p \t', '< .001***')))


# text(1.4, 85, paste0( 't: ', round(tst$statistic,2), ' \n df: ', round(tst$parameter,1), ' \np ', '< .001***') )
axis(1, at = 1:2,tck=0, labels = c('no GHS', 'GHS'), cex.axis = 1)

dev.off()


