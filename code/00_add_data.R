library(readxl)
library(plotly)

d <- read_excel('data/Master sheet, GHS implementation and indicators, 20170919.xls')

d <- as.data.frame(d)

# Load Datasets

kfile <- 'data/KOF globalization_2017_short.xlsx'
k.econ <- as.data.frame(read_excel(kfile, sheet = 'a'))
k.soc <- as.data.frame(read_excel(kfile, sheet = 'b'))
k.pol <- as.data.frame(read_excel(kfile, sheet = 'c'))
k.tot <- as.data.frame(read_excel(kfile, sheet = 'index'))

hdi <- read.csv('data/HDI_2015_UNDP_20170504.csv', skip = 1, check.names =F, strings =F)

gdppp <- as.data.frame(read_excel('data/World Bank GDP per capita, PPP, API_NY.GDP.PCAP.PP.CD_DS2_en_excel_v2.xls', skip = 3))
gdpt <- as.data.frame(read_excel('data/World Bank total GDP PPP API_NY.GDP.MKTP.PP.CD_DS2_en_excel_v2.xls', skip = 3))

tr <- read.csv('data/UNCTAD_us_merchandservtradeopenness_95020020243586.csv', skip = 4, check.names=F, strings=F)
tr <- tr[2:nrow(tr),]

gov <- as.data.frame(read_excel('data/Copy of wgidataset.xlsx', sheet = 'GovernmentEffectiveness', skip = 10))
names(gov) <- paste0(gov[2,],';', gov[1,])
gov <- gov[-c(1:2),]
names(gov) <- gsub(';NA', '', names(gov))


reg <- as.data.frame(read_excel('data/Copy of wgidataset.xlsx', sheet = 'RegulatoryQuality', skip = 10))
names(reg) <- paste0(reg[2,],';', reg[1,])
names(reg) <- gsub(';NA', '', names(reg))
reg <- reg[-c(1:2),]



# code development status
key = c('LDC' = 'Least Developed', 'UM' = 'Upper Middle Income', 'LM' = 'Lower Middle Income', 'OL' = 'Other Low Income', 'HI' = 'High Income')

d$Dev_status <- key[d$`Income category (no tag = high income country)`] 
d$Dev_status <- ifelse(is.na(d$Dev_status), 'Non-DAC', d$Dev_status)
d$dstatus <- factor(d$Dev_status, levels = c('Least Developed', 'Other Low Income', 'Lower Middle Income', 'Upper Middle Income', 'Non-DAC'), ordered = TRUE)
d$stat <- (as.numeric(d$dstatus)+rnorm(n = length(d$dstatus), 0,.1))


# add KOF Globalization

i <- which.max( as.numeric( names( k.econ )))
j <- match( d$`Country code`, k.econ$code )
d[["KOF sub-index A"]] <- k.econ[,i][j]
  
i <- which.max( as.numeric( names( k.soc )))
j <- match( d$`Country code`, k.soc$code )
d[["KOF sub-index B"]] <- k.soc[,i][j]
  
i <- which.max( as.numeric( names( k.pol )))
j <- match( d$`Country code`, k.pol$code )
d[["KOF sub-index C"]] <- k.pol[,i][j]
  
i <- which.max( as.numeric( names( k.tot )))
j <- match( d$`Country code`, k.tot$code )
d[["KOF index, total"]] <- k.tot[,i][j]


# add HDI

library(stringdist)
hdi$ctry <- gsub('^ ', '',hdi$Country)
hdi$ctry <- gsub(' $', '',hdi$ctry)
a <- amatch(d$`Country name`,hdi$ctry )
# cbind(d$`Country name`, hdi$ctry[a])
d$`HDI for 2015` <- hdi$`2015`[a] 


key = c("Bolivia" = "Bolivia (Plurinational State of)",
"Cote d'Ivoire" = "Côte d'Ivoire",
"Democratic Republic of the Congo" = "Congo (Democratic Republic of the)",
"Cape Verde" = "Cabo Verde",
"Micronesia, Federated States of" = "Micronesia (Federated States of)",
"Korea, Republic of" = "Korea (Republic of)",
"Libyan Arab Jamahiriya" = "Libya",
"Monaco" = NA,
"Republic of Moldova" = "Moldova (Republic of)",
"Marshall Islands" = NA,
"Nauro" = NA,
"Korea, Democratic People's Republic of" = NA,
"Russia" = "Russian Federation",
"San Marino" = NA,
"Somalia" = NA,
"Tuvalu" = NA,
"United Republic of Tanzania" = "Tanzania (United Republic of)",
"Venezuela" = "Venezuela (Bolivarian Republic of)"
)

d$`HDI for 2015` <- ifelse(is.na(d$`HDI for 2015`),hdi$`2015`[match( key[match(d$`Country name`,names(key))],hdi$ctry)], d$`HDI for 2015`)


# add GDP

d$`GDP per Capita PPP for 2015` <- gdppp[, '2015'][match(d$`Country code`, gdppp$`Country Code`)]
d$`Total GDP, PPP, for 2015` <- gdpt[, '2015'][match(d$`Country code`, gdpt$`Country Code`)]


# add Trade

tr$c <- gsub('^ .*?\\b', '',tr$YEAR)

a <- amatch(tr$c,d$`Country name` )
tr$country <- d$`Country name`[a]

key2 = c("Bolivia" = "Bolivia (Plurinational State of)",
"Cote d'Ivoire" = "Côte d'Ivoire",
"Democratic Republic of the Congo" = "Dem. Rep. of the Congo",
"Cape Verde" = "Cabo Verde",
"Micronesia, Federated States of" = "Micronesia (Federated States of)",
"Korea, Republic of" = "Korea (Republic of)",
"Libyan Arab Jamahiriya" = "Libya",
"Lao People's Democratic Republic" = "Lao People's Dem. Rep.",
"Republic of Moldova" = "Moldova (Republic of)",
"Marshall Islands" = NA,
"Nauro" = "Nauru",
"TFYR of Macedonia" = "The former Yugoslav Republic of Macedonia",
"Russia" = "Russian Federation",
"Korea, Democratic People's Republic of", "Korea, Dem. People's Rep. of",
"United Republic of Tanzania" = "Tanzania (United Republic of)",
"Venezuela" = "Venezuela (Bolivarian Republic of)"
)
keyx <- names(key2)
names(keyx) <- key2

tr$country <- ifelse(is.na(tr$country),keyx[tr$c], tr$country)

d$`Trade Open-ness for 2013` <- tr$`2013`[match(d$`Country name`, tr$country)]

d$`Trade Open-ness for 2013` <- ifelse( d$`Trade Open-ness for 2013` == '-', tr$`2012`[match(d$`Country name`, tr$country)], d$`Trade Open-ness for 2013`)

d$`Trade Open-ness for 2013` <- ifelse( d$`Trade Open-ness for 2013` == '-', tr$`2011`[match(d$`Country name`, tr$country)], d$`Trade Open-ness for 2013`)


# Add regulatory effectiveness / governance


key <- c(
'ADO' = 'AND',
'ROM' = 'ROU', # Romania
'TMP' = 'TLS', # Timor - leste
'ZAR' = 'COG') # DRC

gov$WBCode[which(gov$WBCode == 'COG')] <- 'COD'
reg$WBCode[which(reg$WBCode == 'COG')] <- 'COD'


i <- match(names(key),gov$WBCode)
gov$`WBCode`[i] <- key

i <- match(names(key),reg$WBCode)
reg$`WBCode`[i] <- key


d$RegulatoryQuality <- reg$`Estimate;2015` [ match(d$`Country code`, reg$WBCode)] 

d$GovernmentEffectiveness <- gov$`Estimate;2015` [ match(d$`Country code`, gov$WBCode)] 

d$RegulatoryQuality <- as.numeric( d$RegulatoryQuality)
d$GovernmentEffectiveness <- as.numeric( d$GovernmentEffectiveness)


# write
 write.csv(d, file.path(outdir, 'data.csv'), row.names=F)
