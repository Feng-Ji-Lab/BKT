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
devtools::build_manual()

# build
devtools::build()

# install local
devtools::install_local("../BKT_0.0.1.tar.gz")

# install local and test
devtools::install_local("../BKT_0.0.1.tar.gz")
library(BKT)
