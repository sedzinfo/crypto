##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check crypto
# R CMD Rd2pdf crypto
# R CMD build crypto --resave-data
library(devtools)
library(roxygen2)
rm(list = ls())
graphics.off()
cat("\014")
##########################################################################################
# FUNCTIONS PACKAGE
##########################################################################################
directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(directory)
# usethis::create_package("crypto")
document()
if ("crypto" %in% loadedNamespaces()) unloadNamespace("crypto") # an old version still loaded would make the reinstalled one look corrupt
if ("crypto" %in% rownames(installed.packages())) remove.packages("crypto")
install()
library(crypto)

pkgdown::build_site(getwd())
system("R CMD Rd2pdf . --force --no-preview --output=crypto.pdf")
if (.Platform$OS.type == "windows") shell.exec("crypto.pdf") else system("evince crypto.pdf")
system("R CMD build . --resave-data")
