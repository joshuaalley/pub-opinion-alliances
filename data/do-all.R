# Joshua Alley
# run all scripts in proper order

# set up project
source("data/setup-script.R", echo = TRUE)
# clean data
source("data/data-cleaning.R", echo = TRUE)
# clean and analyze open questions
source("data/analysis-open-questions.R", echo = TRUE)
# AMCE and marginal means from conjoints 
source("data/analysis.R", echo = TRUE)
# present results
source("data/present-results.R", echo = TRUE)
