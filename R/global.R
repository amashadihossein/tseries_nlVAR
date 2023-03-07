library(simcausal)
library(dplyr)
library(ggplot2)
library(plotly)
library(conflicted)

# pkg conflict preference
#------------------------
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

source("./R/util.R")
y_spline <- yspline_get()