# Joshua Alley
# Clean data and prep for analysis



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


### load and clean sectoral exports and imports data
# from 2019: goods and services (no 2020 data yet at https://data.wto.org/)
trade.data <- read.csv("data/wto-us-trade.csv")
glimpse(trade.data)

# select key columns 
trade.data <- select(trade.data, Product.Sector, Product.Sector.Code,
                     Value, Indicator)

trade.data$flow <- str_extract(trade.data$Indicator, "imports")
trade.data$flow[is.na(trade.data$flow)] <- "exports"

# pivot wider
trade.data.wide <- pivot_wider(trade.data, id_cols = c(Product.Sector.Code, flow),
                               names_from = flow, values_from = Value) %>%
  left_join(select(trade.data, Product.Sector, Product.Sector.Code))
trade.data.wide <- unique(trade.data.wide)
colnames(trade.data.wide) <- c("sector.code", "imports", "exports", "sector")
trade.data.wide$exports[is.na(trade.data.wide$exports)] <- 0
trade.data.wide$imports[is.na(trade.data.wide$imports)] <- 0
trade.data.wide$net <- trade.data.wide$exports - trade.data.wide$imports


# clean sectors
trade.data.wide$consol.sector <- NA
# accomodation and food services as a zero
# administration (subtract professional services)
trade.data.wide$consol.sector[trade.data.wide$sector.code == "SJ"] <- "Administration, support, and waste management"
# Agriculture
trade.data.wide$consol.sector[trade.data.wide$sector.code == "AG"] <- "Agriculture"
# arts and entertainment 
trade.data.wide$consol.sector[trade.data.wide$sector.code == "SK"] <- "Arts, entertainment & recreation"
# construction
trade.data.wide$consol.sector[trade.data.wide$sector.code == "SE"] <- "Construction"
# Educational
trade.data.wide$consol.sector[trade.data.wide$sector.code == "SDB2" | 
                                trade.data.wide$sector.code == "SK22"]  <- "Educational Services"
# health care and social assistance (pharmaceuticals under chemicals)
trade.data.wide$consol.sector[trade.data.wide$sector.code == "SDB1"] <- "Health care and social assistance"
# telecommunications
trade.data.wide$consol.sector[trade.data.wide$sector.code == "MAMTOTTL" | 
                                trade.data.wide$sector.code == "SI"]  <- "Information and Telecommunications"
# Manufacturing - food, textile, apparel (functionally textile)
trade.data.wide$consol.sector[trade.data.wide$sector.code == "MACL" | 
                                trade.data.wide$sector.code == "MATE"]  <- "Manufacturing - food, textile, apparel"
# Manufacturing - wood, chemicals, plastics etc. (iron and steel here)
trade.data.wide$consol.sector[trade.data.wide$sector.code == "MACH" | 
                           trade.data.wide$sector.code == "MAIS"]  <- "Manufacturing - wood, chemicals, plastics etc."
# Manufacturing - machinery, electronics etc.
trade.data.wide$consol.sector[trade.data.wide$sector.code == "MAMT"]  <- "Manufacturing - machinery, electronics etc."
# Oil and Gas
trade.data.wide$consol.sector[trade.data.wide$sector.code == "MIFU"]  <- "Oil and Gas"
# Other Mining TODO(JOSH): subtract oil and gas from this
trade.data.wide$consol.sector[trade.data.wide$sector.code == "MI"]  <- "Other Mining"
# Professional, scientific, and technical services
trade.data.wide$consol.sector[trade.data.wide$sector.code == "SJ2" | 
                            trade.data.wide$sector.code == "SJ3"]  <- "Professional, scientific, and technical services"
# Real Estate (none, add to data) as 0 
# Retail trade
trade.data.wide$consol.sector[trade.data.wide$sector.code == "SJ34"]  <- "Retail Trade"
# wholesale Trade (none, add to data)
# transportation
trade.data.wide$consol.sector[trade.data.wide$sector.code == "SC"]  <- "Transportation & Warehousing"
# Utilities (none, add to data)

# select trade data with observed sector
trade.data.clean <- trade.data.wide[!is.na(trade.data.wide$consol.sector), ] %>%
                    select(net, consol.sector)
trade.data.clean <- rbind.data.frame(
  trade.data.clean,
  c(0, "Accommodation and food services"),
  c(0, "Wholesale Trade"),
  c(0, "Real Estate")
)
trade.data.clean$net <- as.numeric(trade.data.clean$net)
# summarize
trade.data.clean <- trade.data.clean %>%
                    group_by(consol.sector) %>%
                    summarize(
                      net = sum(net),
                      .groups = "keep"
                    )
# subtract subcategories
# oil and gas
trade.data.clean$net[trade.data.clean$consol.sector == "Other Mining"] <- 
  trade.data.clean$net[trade.data.clean$consol.sector == "Other Mining"] - 
  trade.data.clean$net[trade.data.clean$consol.sector == "Oil and Gas"]
# professional services
trade.data.clean$net[trade.data.clean$consol.sector == "Administration, support, and waste management"] <- 
  trade.data.clean$net[trade.data.clean$consol.sector == "Administration, support, and waste management"] - 
  trade.data.clean$net[trade.data.clean$consol.sector == "Professional, scientific, and technical services"]
# rename colums
colnames(trade.data.clean) <- c("occupation", "net.exports")
summary(trade.data.clean$net.exports)




### Clean survey data
# qualtrics API: maintenance data
maintenance.data <- read.csv("data/maintenance-lucid-theorem.csv")
maintenance.data <- maintenance.data %>%
  filter(Finished == TRUE)

# formation data
formation.data <- read.csv("data/formation-lucid-theorem.csv")
formation.data <- formation.data %>%
  filter(Finished == TRUE) 




# feature variables are doable, albeit with some ugly code
# to split tasks
# as a function
feature.clean <- function(data, ntask){

# Lucid theorem rather not answer is -3105
data[data == -3105] <- NA  


# rename key variables 
# task variables
data <- rename(data,
                choice_1 = Q21,
                choice_2 = Q23,
                choice_3 = Q25,
                choice_4 = Q27,
                choice_5 = Q29 )

data <- rename(data,
               rating_1 = Q22_1,
               rating_2 = Q24_1,
               rating_3 = Q26_1,
               rating_4 = Q28_1,
               rating_5 = Q30_1)


# clean up outcome data
rating.data <- select(data, ResponseId, 
                   starts_with("rating")) %>%
                  pivot_longer(cols = starts_with("rating"),
                               names_to = c("task"),
                               names_transform = list(task = readr::parse_number),
                               values_to = "rating")

choice.data <- select(data, ResponseId, 
                      starts_with("choice")) %>%
  pivot_longer(cols = starts_with("choice"),
               names_to = c("task"),
               names_transform = list(task = readr::parse_number),
               values_to = "choice")

# get covariates
covar.data <- select(data, ResponseId, 
                age, gender, hhi, ethnicity,
                hispanic, education, political_party,	
                region,	zip, Q11,
                Q18, Q1, Q6, Q3, Q7, Q4, Q5) %>%
  rename(
    us.region = region,
    occupation = Q11,
    mil.serv = Q18,
    peace.str = Q1,
    force.worse = Q6,
    war.unf = Q3,
    isolation = Q7,
    us.sup = Q4,
    us.shame = Q5
  ) %>%
  left_join(trade.data.clean)

# loop over data 
task.list <- vector(mode = "list", length = ntask)
for(i in 1:ntask){
task <- select(data, starts_with(paste0("F.", i)))
task <- as.data.frame(mapply(c,
               task[, 1:2],
               task[, 3:4],
               task[, 5:6],
               task[, 7:8],
               task[, 9:10],
               task[, 11:12],
               task[, 13:14],
               task[, 15:16],
               task[, 17:18],
               task[, 19:20],
               task[, 21:22],
               task[, 23:24],
               task[, 25:26],
               task[, 27:28]
               ))
colnames(task) <- c("attribute", "value")
task$ResponseId <- rep(data$ResponseId, 14)
task <- pivot_wider(task, names_from = "attribute",
                     id_cols = "ResponseId",
                     values_from = "value")
task.list[[i]] <- task
task.list[[i]] <- left_join(task.list[[i]], # merge respondent-level factors
                      covar.data, by = "ResponseId") %>%
                  mutate(
                    task = i,
                    profile = "A"
                     )
}

# unlist task data and add to cjoint data
cjoint.data <- bind_rows(task.list) %>%
                 left_join(choice.data) %>%
                  left_join(rating.data) 

# Further data cleaning
# recode choice variable
table(cjoint.data$choice)
print(table(cjoint.data$choice))
cjoint.data$choice <- as.numeric(factor(cjoint.data$choice))
print(table(cjoint.data$choice))
cjoint.data$choice[cjoint.data$choice == 1] <- NA
cjoint.data$choice <- recode(cjoint.data$choice, `2` = 0, `3` = 1)
print(table(cjoint.data$choice))

# Other variables
# party
# sort the leaners
cjoint.data$party.id <- ifelse(cjoint.data$political_party >= 8 |
                         cjoint.data$political_party == 5,
                        "Republican",
                          ifelse(cjoint.data$political_party <= 3 |
                            cjoint.data$political_party == 6,
                          "Democrat", "Independent"))

# factor and split by republican/not 
cjoint.data$party.id <- factor(cjoint.data$party.id,
                              levels = c("Independent", "Democrat", "Republican"))
print(table(cjoint.data$party.id))
cjoint.data$republican <- factor(ifelse(cjoint.data$party.id == "Republican", "Repub.", "Democ./Indep."))
print(table(cjoint.data$republican))


# gender and military service
cjoint.data$gender.fac <- as.factor(recode(cjoint.data$gender, 
                                           `1` = "Male",
                                           `2` = "Female"
                                         ))
print(table(cjoint.data$gender))
print(table(cjoint.data$gender.fac))
cjoint.data$mil.serv <- as.factor(cjoint.data$mil.serv)

# mapping for agree/disagree questions
mapping <- c("Strongly disagree" = 1, "Somewhat disagree" = 2,
             "Neither agree nor disagree" = 3,
             "Somewhat agree" = 4, "Strongly agree" = 5)

# isolationism
cjoint.data$isolation.num <- mapping[cjoint.data$isolation]
cjoint.data$isolation.fac <- factor(ifelse(cjoint.data$isolation.num > 3, "Agree", 
                                   #  ifelse(cjoint.data$isolation.num == 3, "Neutral",      
                                          "Disagree/Neutral"),
                                    levels = c("Disagree/Neutral", "Agree"))
print("Isolationism")
print(table(cjoint.data$isolation.fac))

# militant internationalism
cjoint.data$peace.str.num <- mapping[cjoint.data$peace.str]
cjoint.data$force.worse.num <- mapping[cjoint.data$force.worse]
cjoint.data$war.unf.num <- mapping[cjoint.data$war.unf]
# index of militant internationalism
cjoint.data$mil.inter <- cjoint.data$peace.str.num + cjoint.data$war.unf.num -
  cjoint.data$force.worse.num
cjoint.data$mil.inter.fac <- cut(cjoint.data$mil.inter, 2)
print("Mil. Inter.")
print(table(cjoint.data$mil.inter.fac))
# split exports
cjoint.data$exports.fac <- factor(ifelse(cjoint.data$net.exports >= 0, "Positive",
                                 #   ifelse(cjoint.data$net.exports == 0, "Null",
                                           "Negative"),
                                  levels = c("Negative", "Positive"))
print("Exports")
print(table(cjoint.data$exports.fac))




# label and factor treatments
labels <- colnames(cjoint.data)[2:15]
# turn labels to useful variable names
cjoint.data <- rename(cjoint.data,
                      shared.threat = `Shared Threat`,
                      support.cond = `Military Support Conditions`,
                      region = `Region`,
                      defense.coop = `Defense Cooperation`,
                      dem.supp = `Democrat Senators`,
                      rep.supp = `Republican Senators`,
                      regime = `Political Regime`,
                      trade = `Trade Ties`,
                      cost = `Financial Cost`,
                      jcs.supp = `The Joint Chiefs of Staff`,
                      issue.link = `Related Cooperation`,
                      state.supp = `The Secretary of State`,
                      cap = `Military Capability`,
                      mil.coop = `Recent Military Cooperation`
)


# recode support vars to differentiate them
cjoint.data$dem.supp <- recode(cjoint.data$dem.supp,
                             "Oppose an alliance with this country." = "Dem. Senate Oppose" ,
                             "Support an alliance with this country." = "Dem. Senate Support"
)
cjoint.data$rep.supp <- recode(cjoint.data$rep.supp,
                             "Oppose an alliance with this country." = "Rep. Senate Oppose",
                             "Support an alliance with this country." =  "Rep. Senate Support"
)
cjoint.data$jcs.supp <- recode(cjoint.data$jcs.supp,
                             "Oppose an alliance with this country." = "JCS Oppose",
                             "Support an alliance with this country." = "JCS Support"
)
cjoint.data$state.supp <- recode(cjoint.data$state.supp,
                               "Opposes an alliance with this country." = "State Oppose",
                               "Supports an alliance with this country." = "State Support"
)

# recode other factors to make plots more legible
# regime
cjoint.data$regime <- recode(cjoint.data$regime,
         "This country is not a democracy, and shows no sign of becoming a democracy. " = "Not a Democracy",
         "This country is a democracy, but shows signs that it may not remain a democracy." = "Weak Democracy",
         "This country is a democracy, and shows every sign that it will remain a democracy." = "Democracy"
         )
# Shared threat
cjoint.data$shared.threat <- recode(cjoint.data$shared.threat,
                            "The United States and this country face minimal common threats." = "Minimal Threat",
                            "The United States and this country face mild common threats." = "Mild Threat",
                            "The United States and this country face serious common threats." = "Serious Threat")
# trade ties
cjoint.data$trade <- recode(cjoint.data$trade,
                       "The United States has extensive trade ties with this country." = "Extensive Trade",
                       "The United States has modest trade ties with this country." = "Modest Trade",
                       "The United States has minimal trade ties with this country." = "Minimal Trade")
# conditions on support
cjoint.data$support.cond <- recode(cjoint.data$support.cond,
            "The alliance treaty promises military support in any conflict." = "Unconditional",
    "The alliance treaty promises military support only if this country did not provoke the conflict." =
       "Nonprovocation",
    "The alliance treaty promises military support only it the conflict takes place in this country's region." =
      "Specific Region")
# defense cooperation
cjoint.data$defense.coop <- recode(cjoint.data$defense.coop,
                      "None." = "No Defense Cooperation",
               "The alliance includes a shared military command." = "Shared Command",
            "The alliance includes an international organization to coordinate defense policies." =
              "International Organization",
            "The alliance provides basing rights for U.S. troops." = "Basing Rights"
            )
# issue linkages
cjoint.data$issue.link <- recode(cjoint.data$issue.link,
                                 "None." = "No Issue Linkage.",
       "The alliance is linked to greater support for the United States in the United Nations." =
         "UN Voting",
       "The alliance is linked to greater trade and investment with the United States." =
          "Trade and Investment")
# Military Capability
cjoint.data$cap <- recode(cjoint.data$cap,
                  "10,000 soldiers and spends 1% of GDP on their military." = "Low Capability",
                  "80,000 soldiers and spends 2% of their GDP on the military." = "Moderate Capability",
                  "250,000 soldiers and spends 3% of their GDP on the military." = "High Capability")
# recent cooperation
cjoint.data$mil.coop <- recode(cjoint.data$mil.coop,
                 "This country has not participated in recent U.S. military operations." = "No Recent Cooperation",
                 "This country recently fought with the United States in a war." = "At War Together",
                 "This country recently supported U.S. counterinsurgency operations." = "Support Counterinsurgency",
                 "This country recently supported U.S. airstrikes against terrorists." = "Support Airstrikes")

# financial cost 
cjoint.data$cost <- recode(cjoint.data$cost,
          "This alliance requires $5 billion in annual U.S. defense spending." = "$5 billion",
          "This alliance requires $10 billion in annual U.S. defense spending." = "$10 billion",
          "This alliance requires $15 billion in annual U.S. defense spending." = "$15 billion"
)



# as factors
cjoint.data[, 2:15] <- lapply(cjoint.data[, 2:15], factor)

# set base categories
cjoint.data$regime <- relevel(cjoint.data$regime,
                    "Not a Democracy")
cjoint.data$defense.coop <- relevel(cjoint.data$defense.coop,
                              "No Defense Cooperation")
cjoint.data$trade <- relevel(cjoint.data$trade,
                             "Minimal Trade")
cjoint.data$cap <- relevel(cjoint.data$cap,
                             "Low Capability")
cjoint.data$mil.coop <- relevel(cjoint.data$mil.coop,
                           "No Recent Cooperation")
cjoint.data$shared.threat <- relevel(cjoint.data$shared.threat,
                                "Minimal Threat")
cjoint.data$support.cond <- relevel(cjoint.data$support.cond,
                                     "Unconditional")
cjoint.data$cost <- relevel(cjoint.data$cost,
                                    "$5 billion")

# label key variables
cjoint.data[, 2:15] <- set_label(cjoint.data[, 2:15], labels)
cjoint.data$gender <- set_label(cjoint.data$gender, "Gender")
cjoint.data$republican <- set_label(cjoint.data$republican, "Republican")
cjoint.data$party.id <- set_label(cjoint.data$party.id, "Party ID")
cjoint.data$mil.serv <- set_label(cjoint.data$mil.serv, "Military Service")
cjoint.data$isolation.fac <- set_label(cjoint.data$isolation.fac, "Isolationism")
cjoint.data$mil.inter.fac <- set_label(cjoint.data$mil.inter.fac, "Mil. Internationalism")
cjoint.data$exports.fac <- set_label(cjoint.data$exports.fac, "Sectoral Exports")

# final output
data <- cjoint.data

}


# formation data
form.data <- feature.clean(data = formation.data, 
                           ntask = 5)

# pull data: maintenance
main.data <- feature.clean(data = maintenance.data, 
              ntask = 5)




