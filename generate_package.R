##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check golemcrypto
# R CMD Rd2pdf golemcrypto
# R CMD build golemcrypto --resave-data
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
# usethis::create_package("golemcrypto")
document()
if ("golemcrypto" %in% loadedNamespaces()) unloadNamespace("golemcrypto") # an old version still loaded would make the reinstalled one look corrupt
if ("golemcrypto" %in% rownames(installed.packages())) remove.packages("golemcrypto")
install()
library(golemcrypto)

pkgdown::build_site(getwd())
system("R CMD Rd2pdf . --force --no-preview --output=golemcrypto.pdf")
if (.Platform$OS.type == "windows") shell.exec("golemcrypto.pdf") else system("evince golemcrypto.pdf")
system("R CMD build . --resave-data")

readRenviron("~/GitHub/golemcrypto/.Renviron")
Sys.getenv("COINMARKETCAP_APIKEY")
crypto()


