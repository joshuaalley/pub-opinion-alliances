# Joshua Alley
# set up project environment



# load packages
library(tidyverse)
library(stringr)
library(cregg)
library(conflicted)
library(sjlabelled)
library(gridExtra)
library(ggcarly)
library(factorEx)
library(GJRM)
library(wesanderson)

# manage conflicts
conflict_scout()
conflict_prefer("lag", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("combine", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("expand", "tidyr")
conflict_prefer("pack", "tidyr")
conflict_prefer("unpack", "tidyr")
conflict_prefer("as_data_frame", "dplyr")
conflict_prefer("compose", "purrr")
conflict_prefer("crossing", "tidyr")
conflict_prefer("groups", "dplyr")
conflict_prefer("simplify", "purrr")
conflict_prefer("as_factor", "forcats")
conflict_prefer("chol2inv", "Matrix")
conflict_prefer("decompose", "igraph")
conflict_prefer("Position", "ggplot2")
conflict_prefer("rcond", "Matrix")
conflict_prefer("spectrum", "igraph")
conflict_prefer("mm", "cregg")
conflict_prefer("collapse", "dplyr")

# set seed
set.seed(12)

# set ggplot theme
theme_set(theme_bw())


# set filter functions for cregg objects
# general
filter.cregg <- function(x){
  filter(x, feature == "Democrat Senators" |
           feature == "Republican Senators" |
           feature == "The Joint Chiefs of Staff" |
           feature == "The Secretary of State" |
           feature == "Region" |
           feature == "Political Regime" |
           feature == "Trade Ties" |
           feature == "Financial Cost" |
           feature == "Related Cooperation")
}


# elite cues 
filter.cregg.el <- function(x){
  filter(x, feature == "Democrat Senators" |
           feature == "Republican Senators" |
           feature == "The Joint Chiefs of Staff" |
           feature == "The Secretary of State")
}


# alliance characteristics 
filter.cregg.char <- function(x){
  filter(x, feature == "Region" |
           feature == "Political Regime" |
           feature == "Trade Ties" |
           feature == "Financial Cost" |
           feature == "Related Cooperation")
}
