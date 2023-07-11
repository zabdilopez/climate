library(renv)

init()

snapshot()

.libPaths()

find.package("ggplot2")
find.package("dplyr")

install.packages("wesanderson")

find.package("wesanderson")

renv::snapshot()

library(wesanderson)
source("scripts/dummy.R")
