## typically wd is set at the Project level, but needs to be changed to render bookdown documents
library(here)

setwd(here("output/report"))

bookdown::render_book("index.Rmd", "bookdown::gitbook")
bookdown::render_book("index.Rmd", "bookdown::html_document2") ##sharable
bookdown::render_book("index.Rmd", "bookdown::pdf_book")

###Gitbook: open `~/output/report/_book/index.html` in web browser by right clicking file
###PDF_book: open `.pdf`. Doesn't look as nice (tables, etc).
###

# return to project level working directory
setwd(here())
  