# install.packages("renv")
renv::init()
install.packages(c("shiny","tidyverse","shinyjs","ggforce","cowplot","DT","later"))
renv::snapshot()
