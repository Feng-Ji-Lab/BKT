# Test
unloadNamespace("BKT")
rm(list = ls())
devtools::load_all("./")
devtools::test()

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

# install local and load
devtools::install_local("../BKT_0.0.1.tar.gz")
library(BKT)

# install online and load
devtools::install_github("Feng-Ji-Lab/bkt")
library(BKT)

# check
unloadNamespace("BKT")
devtools::load_all("./")
library(roxygen2)
roxygen2::roxygenise()
unloadNamespace("BKT")
devtools::load_all("./")
devtools::build_manual()
rm(list = ls())
devtools::load_all("./")
devtools::check()

# check cran
# 0. 改版本号
# 1. build
devtools::build()
# 2. bash run
R CMD check --as-cran "C:/Users/zby15/Desktop/ss/BKT_0.0.1.tar.gz"
R CMD check --as-cran "C:/Users/zby15/Documents/GitHub/BKT_0.0.1.tar.gz"