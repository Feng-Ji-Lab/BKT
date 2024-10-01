# Test
unloadNamespace("BKT")
rm(list = ls())
devtools::load_all("./")
devtools::test()

# check
unloadNamespace("BKT")
rm(list = ls())
devtools::load_all("./")
devtools::check()

# document
unloadNamespace("BKT")
rm(list = ls())
devtools::load_all("./")
library(roxygen2)
roxygen2::roxygenise()
unloadNamespace("BKT")
devtools::load_all("./")
help("bkt")
help("fit")
