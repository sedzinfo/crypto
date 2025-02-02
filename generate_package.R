##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check crypto
# R CMD Rd2pdf crypto
# R CMD build crypto --resave-data
library(devtools)
library(roxygen2)
directory<-paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/")
setwd(directory)
# usethis::create_package("crypto")
document()
install()
