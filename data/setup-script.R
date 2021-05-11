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

# set seed
set.seed(12)

# set ggplot theme
theme_set(theme_bw())
