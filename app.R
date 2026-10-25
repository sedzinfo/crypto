# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options(crypto.cache_dir = file.path(tempdir(), "crypto-cache"))   # the server's disk is temporary anyway
crypto(analytics = TRUE)   # Google Analytics, after the visitor accepts the cookie banner
