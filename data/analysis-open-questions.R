# Joshua Alley
# Analyze open-ended questions


# load data 
form.open <- read.csv("data/formation-open-question.csv")
main.open <- read.csv("data/maintenance-open-question.csv")


# clean data
open.clean <- function(data){
  
  # address blanks: NA to 0
  data[, 3:19][is.na(data[3:19])] <- 0
  # address -3105 missing codes
  data[data == -3105] <- NA
  
  # clean up other covariates
  covar.data <- rename(data,
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
  
  # sort the leaners
  covar.data$party.id <- ifelse(covar.data$political_party >= 8 |
                                   covar.data$political_party == 5,
                                 "Republican",
                                 ifelse(covar.data$political_party <= 3 |
                                          covar.data$political_party == 6,
                                        "Democrat", "Independent"))
  
  # factor and split by republican/not 
  covar.data$party.id <- factor(covar.data$party.id,
                                 levels = c("Republican", "Independent", "Democrat"))
  print(table(covar.data$party.id))
  covar.data$republican <- factor(ifelse(covar.data$party.id == "Republican", "Repub.", "Democ./Indep."))
  print(table(covar.data$republican))
  
  # strength of party attachment
  covar.data$partisan.str <- ifelse(covar.data$political_party == 1 |
                                      covar.data$political_party == 10,
                                    3, # strong
                              ifelse(covar.data$political_party == 2 |
                                       covar.data$political_party == 9,
                                     2, # not very strong
                               ifelse(covar.data$political_party == 3 |
                                      covar.data$political_party == 6|
                                        covar.data$political_party == 8 |
                                        covar.data$political_party == 5,
                                      # indep/other w/ lean
                                      1, 0))) 
  
  
  # gender and military service
  covar.data$gender.fac <- as.factor(recode(covar.data$gender, 
                                             `1` = "Male",
                                             `2` = "Female"
  ))
  print(table(covar.data$gender))
  print(table(covar.data$gender.fac))
  covar.data$mil.serv <- as.factor(covar.data$mil.serv)
  
  # mapping for agree/disagree questions
  mapping <- c("Strongly disagree" = 1, "Somewhat disagree" = 2,
               "Neither agree nor disagree" = 3,
               "Somewhat agree" = 4, "Strongly agree" = 5)
  
  # isolationism
  covar.data$isolation.num <- mapping[covar.data$isolation]
  covar.data$isolation.fac <- factor(ifelse(covar.data$isolation.num > 3, "Agree", 
                                             #  ifelse(covar.data$isolation.num == 3, "Neutral",      
                                             "Disagree/Neutral"),
                                      levels = c("Disagree/Neutral", "Agree"))
  covar.data$isolation.fac <- recode(covar.data$isolation.fac,
                                      "Disagree/Neutral" = "International",
                                      "Agree" = "Isolation")
  print("Isolationism")
  print(table(covar.data$isolation.fac))
  
  # militant internationalism
  covar.data$peace.str.num <- mapping[covar.data$peace.str]
  covar.data$force.worse.num <- mapping[covar.data$force.worse]
  covar.data$war.unf.num <- mapping[covar.data$war.unf]
  # index of militant internationalism
  covar.data$mil.inter <- covar.data$peace.str.num + covar.data$war.unf.num -
    covar.data$force.worse.num
  covar.data$mil.inter.fac <- cut(covar.data$mil.inter, 2)
  covar.data$mil.inter.fac <- recode(covar.data$mil.inter.fac,
                                      "(-3.01,3]" = "Dove",
                                      "(3,9.01]" = "Hawk")
  print("Mil. Inter.")
  print(table(covar.data$mil.inter.fac))
  # split exports
  covar.data$exports.fac <- factor(ifelse(covar.data$net.exports >= 0, "Positive",
                                           #   ifelse(covar.data$net.exports == 0, "Null",
                                           "Negative"),
                                    levels = c("Negative", "Positive"))
  print("Exports")
  print(table(covar.data$exports.fac))
  
  
  # generate summary variables of open responses
  covar.data <- covar.data %>%
     mutate(
       trust = as.numeric(stringr::str_detect(open.question, "trust")),
       benefits = as.numeric(stringr::str_detect(open.question, "benefit")),
       elite.supp = ifelse(rep.supp == 1 | dem.supp == 1 |
                             elite == 1 | bipart == 1 | 
                             jcs == 1 | state == 1, 
                           1, 0),
       partner.attr = ifelse(regime == 1 | trade == 1 |
                            shared.threat == 1 | region == 1 |
                            mil.coop == 1 | cap == 1,
                            1, 0),
       alliance.attr = ifelse(defense.coop == 1 | conditions == 1 |
                                cost == 1 | issue.link == 1,
                              1, 0),
       partisan.cues = ifelse(dem.supp == 1 | rep.supp == 1 |
                                bipart == 1, 1, 0)
     )
  
  # final output
  clean.data <- covar.data
} # end cleaning function

# apply cleaning function
# formation
openq.form <- open.clean(form.open)
glimpse(openq.form)

# maintenance
openq.main <- open.clean(main.open)
glimpse(openq.main)

# formation data
lapply(openq.form[, 3:19], function(x) sum(x, na.rm = TRUE))
table(openq.form$elite.supp)
table(openq.form$partisan.cues)
table(openq.form$alliance.attr)
table(openq.form$partner.attr)
# trust and benefits
table(openq.form$trust)
table(openq.form$benefits)


# maintenance data
lapply(openq.main[, 3:19], function(x) sum(x, na.rm = TRUE))
table(openq.main$elite.supp)
table(openq.main$partisan.cues)
table(openq.main$alliance.attr)
table(openq.main$partner.attr)
# trust and benefits
table(openq.main$trust)
table(openq.main$benefits)



### look over raw data 

# tabulate foreign policy dispositions by party attachment
# formation 
table(openq.form$party.id, openq.form$mil.inter)
table(openq.form$party.id, openq.form$mil.inter.fac)
table(openq.form$party.id, openq.form$isolation.fac)

# maintenance 
table(openq.main$party.id, openq.main$mil.inter)
table(openq.main$party.id, openq.main$mil.inter.fac)
table(openq.main$party.id, openq.main$isolation.fac)

#isolation and hawkishness
ggplot(openq.form, aes(x = mil.inter, y = isolation.num)) +
    facet_wrap(~ party.id) +
    geom_count(aes(size = after_stat(prop))) +
  geom_smooth()

ggplot(openq.main, aes(x = mil.inter, y = isolation.num)) +
  facet_wrap(~ party.id) +
  geom_count(aes(size = after_stat(prop))) +
  geom_smooth()


ggplot(openq.main, aes(x = mil.inter, y = isolation.num,
                       color = party.id)) +
  geom_jitter(alpha = .5)

# numbers of each
openq.main$party.dispo <- interaction(openq.main$party.id, 
                                      openq.main$mil.inter.fac,
                                      openq.main$isolation.fac,
                                      sep = "_")
table(openq.main$party.dispo)

# economic interests and parties
table(openq.form$party.id, openq.form$exports.fac)
table(openq.main$party.id, openq.main$exports.fac)

# economic interests and dispositions
table(openq.form$exports.fac, openq.form$mil.inter.fac)
table(openq.form$exports.fac, openq.form$isolation.fac)


# plot estimated support by partisan str
partisan.str.form <- openq.form %>%
                      group_by(partisan.str) %>%
                       summarize(
                         n = n(),
                         prop.none = sum(none, na.rm = T) / n,
                         prop.elite = sum(elite.supp, na.rm = T) / n,
                         prop.all = sum(alliance.attr, na.rm = T) / n,
                         prop.part = sum(partner.attr, na.rm = T) / n,
                         .groups = "keep"
                       ) %>% 
                      pivot_longer(cols = c("prop.elite", "prop.none",
                                            "prop.all", "prop.part"),
                                   names_to = "emphasis") 
ggplot(partisan.str.form, aes(x = partisan.str, y = value,
                              color = emphasis)) +
  geom_point() + geom_line() + theme_bw()


# by maintenance
partisan.str.main <- openq.main %>%
  group_by(partisan.str) %>%
  summarize(
    n = n(),
    prop.none = sum(none, na.rm = T) / n,
    prop.elite = sum(elite.supp, na.rm = T) / n,
    prop.all = sum(alliance.attr, na.rm = T) / n,
    prop.part = sum(partner.attr, na.rm = T) / n,
    .groups = "keep"
  ) %>% 
  pivot_longer(cols = c("prop.elite", "prop.none",
                        "prop.all", "prop.part"),
               names_to = "emphasis") 
ggplot(partisan.str.main, aes(x = partisan.str, y = value,
                              color = emphasis)) +
  geom_point() + geom_line() + theme_bw()
