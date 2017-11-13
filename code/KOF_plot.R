library(readxl)
library(plotly)
library(wordcloud)


outfile <- paste0(outdir,'/figures/KOF political x GHS ',  format(Sys.time(), "%Y-%m-%d"),'.png')


ylabel <- 'KOF Political Globalization Score'
YLIM <- c(19,100)

d <- read.csv(file.path(outdir,'data.csv'), check.names=F)


d$GHS <- d$`GHS  implementation`
d$ghs <- d$GHS >0
i <- which(d$ghs)
d$y <- d$`KOF sub-index C`


set.seed(501)
d$x <- d$ghs+1+ rnorm(n = nrow(d), 0,.08) 

a <- d[, c('Country name','Country code', 'y', 'x', 'ghs')]
a <- na.omit(a)

red <- function(nc, reduce){
  centx <- nc[,1] + .5*nc[,3]
  centy <- nc[,2] + .5*nc[,4]
  
  nc[,1] <- centx - .5*nc[,3]*reduce
  nc[,2] <- centy - .5*nc[,4]*reduce
  nc[,3] <- nc[,3]*reduce
  nc[,4] <- nc[,4]*reduce
  
  nc
}

pfun <- function(p) { 

  x <- round(p, 3)
  if(x == 0) return ( ' < 0.001 ') else return(x)
}


tst <- t.test(d$y[i], d$y[-i], var.equal = TRUE)
tst <- t.test(d$y[i], d$y[-i])

r <- .9 


png( file = outfile, width = 7, height = 7, units = 'in', res = 300)

plot(d$y ~ as.factor(d$ghs), border = rgb(0,0,0,.7), outline = F, xlab = '', ylab = ylabel, xaxt = 'n', col = grey(1), ylim = YLIM, lwd = 3)

nc <- wordlayout(a$x, a$y, a$`Country code`, cex = .95)
nc <- red(nc, r)

rect(nc[,1], nc[,2], nc[,1] + nc[,3], nc[,2] + nc[,4], col = rgb(1-as.numeric(a$ghs),0,0,.5), border = NA)
text(nc[,1] + .5*nc[,3], nc[,2] + .5*nc[,4]+.09*nc[,4], a$`Country code`, cex = .65, col = 'white')
axis(1, at = 1:2,tck=0, labels = c('Non-implemented', 'GHS implemented'), cex.axis = 1)
legend('bottomright', legend =  c( paste0( 't: ', round(tst$statistic,2),'; df: ', round(tst$parameter,1),'; p: ', pfun(tst$p.value))), cex = .7, bty = 'n', text.font = 3)
box(lwd=3)

dev.off()




