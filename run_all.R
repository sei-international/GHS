# TCI index and analyses
#
#  setwd('path/to/GHS')

# outputs directory (creates one per day!)
now <- Sys.time()
dname <- paste0( 'run_', strftime(now, format = "%Y%m%d"))
outdir <- file.path( 'outputs', dname )
dir.create(outdir)
dir.create(file.path(outdir,'tables'))
dir.create(file.path(outdir,'figures'))

# view assumptions / parameters
print(ls.str())

# 1. compile data and calculate TCI scores

source('code/00_add_data.R')

# 2. summary maps and figures

source('code/map.R')
source('code/GDP_plots.R')
source('code/HDI_plot.R')
source('code/KOF_HDI.R')
source('code/KOF_plots.R')
source('code/Trade_plot.R')
source('code/regulation_government_plot.R')

# 3. Analyses 


require(rmarkdown) # include pandoc in Path system variable 

rmarkdown::render('code/stats.Rmd')
file.rename('code/stats.docx', 
    file.path(outdir, 'stats.docx'))

rmarkdown::render('code/logistic_regression.Rmd')
file.rename('code/logistic_regression.docx', 
    file.path(outdir, 'logistic_regression.docx'))
