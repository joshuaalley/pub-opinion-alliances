# Joshua Alley
# set up project environment



# load packages
library(tidyverse)
library(stringr)
library(cregg)
library(conflicted)
library(sjlabelled)
library(gridExtra)
library(FindIt)
library(ggcarly)
library(factorEx)
library(rstan)
# set stan options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

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
conflict_prefer("extract", "rstan")
conflict_prefer("Position", "ggplot2")
conflict_prefer("rcond", "Matrix")
conflict_prefer("spectrum", "igraph")
conflict_prefer("traceplot", "rstan")

# set seed
set.seed(12)

# set ggplot theme
theme_set(theme_bw())
