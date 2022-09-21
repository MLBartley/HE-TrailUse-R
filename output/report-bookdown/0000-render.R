## typically wd is set at the Project level, but needs to be changed to render bookdown documents
library(here)

#not sure why these has trouble loading but loading again here to help render
# library(mgcv) 
# library(gratia)
# source("scripts/01.02-LoadFunctions.R")

## render_book looks for "index.Rmd" in wd so we need to reset briefly
setwd(here("output/report"))

bookdown::render_book("index.Rmd", "bookdown::gitbook")
bookdown::render_book("index.Rmd", "bookdown::html_document2") ##sharable
bookdown::render_book("index.Rmd", "bookdown::pdf_book")

###Gitbook: open `~/output/report/_book/index.html` in web browser by right clicking file
###PDF_book: open `.pdf`. 
###

# return to project level working directory
setwd(here())
  