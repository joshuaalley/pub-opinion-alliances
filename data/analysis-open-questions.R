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
      intl.trust = Q4,
      war.unf = Q3,
      isolation = Q7,
      us.sup = Q4.1,
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
  
  # hispanic
  covar.data$hispanic <- ifelse(covar.data$hispanic > 1, 1, 0)
  covar.data$white <- ifelse(covar.data$ethnicity == 1, 1, 0)
  
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
  
  
  # intl trust
  covar.data$intl.trust.dum <- ifelse(covar.data$intl.trust ==
                                         "Can trust other nations.",
                                       1, 0)
  print("Intl. Trust")
  print(table(covar.data$intl.trust))
  print(table(covar.data$intl.trust.dum))
  
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
table(openq.form$party.id, openq.form$intl.trust)

# maintenance 
table(openq.main$party.id, openq.main$mil.inter)
table(openq.main$party.id, openq.main$mil.inter.fac)
table(openq.main$party.id, openq.main$isolation.fac)
table(openq.main$party.id, openq.main$intl.trust)


# count mentions of isolation
sum(str_detect(openq.form$open.question, "isolationist"))
sum(str_detect(openq.main$open.question, "isolation"))

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

# tabulate number of respondents in each disposition
# numbers of each: maintenance
openq.main$party.dispo <- interaction(openq.main$party.id, 
                                      openq.main$mil.inter.fac,
                                      openq.main$isolation.fac,
                                      sep = "-")
table(openq.main$party.dispo)
xtable::xtable(table(openq.main$party.dispo),
               label = "tab:party-dispo-main",
               caption = "Number of respondents in each group of partisanship 
               and foreign policy disposition for the alliance maintenance experiment.")

# formation
openq.form$party.dispo <- interaction(openq.form$party.id, 
                                      openq.form$mil.inter.fac,
                                      openq.form$isolation.fac,
                                      sep = "-")
table(openq.form$party.dispo)
xtable::xtable(table(openq.form$party.dispo),
               label = "tab:party-dispo-form",
               caption = "Number of respondents in each group of partisanship 
               and foreign policy disposition for the alliance formation experiment.")

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



### multivariate probit analysis 


# use GJRM w/ T copula for errors

# elite formula
elite.formula <- elite.supp ~ partisan.str + isolation.num + mil.inter + 
                                age + gender + white +
                                hhi + education + net.exports

# alliance attr formula
all.formula <- alliance.attr ~ partisan.str + isolation.num + mil.inter + 
  age + gender + white +
  hhi + education + net.exports

# partner attr formula
part.formula <- partner.attr ~ partisan.str + isolation.num + mil.inter + 
  age + gender + white +
  hhi + education + net.exports



### formation results

# clean data to get variables on same scale
# rescale continuous by 2sd
# formation
openq.form.clean <- select(openq.form,
                           elite.supp, alliance.attr, partner.attr,
                           partisan.str, isolation.num, mil.inter,
                             age, gender, white,
                             hhi, education, net.exports)
openq.form.clean[, 4:ncol(openq.form.clean)] <- apply(
  openq.form.clean[, 4:ncol(openq.form.clean)], 2, 
  function(x) rescale(x, binary.inputs = "0/1")
)
# maintenance
openq.main.clean <- select(openq.main,
                           elite.supp, alliance.attr, partner.attr,
                           partisan.str, isolation.num, mil.inter,
                           age, gender, white,
                           hhi, education, net.exports)
openq.main.clean[, 4:ncol(openq.main.clean)] <- apply(
  openq.main.clean[, 4:ncol(openq.main.clean)], 2, 
  function(x) rescale(x, binary.inputs = "0/1")
)

# start with univariate models
# elite 
form.elite.glm <- glm(elite.formula,
                      family = binomial(link = "probit"),
                      data = openq.form.clean)
summary(form.elite.glm)

# alliance attr
form.all.glm <- glm(all.formula,
                      family = binomial(link = "probit"),
                      data = openq.form.clean)
summary(form.all.glm)

# partner attr
form.part.glm <- glm(part.formula,
                    family = binomial(link = "probit"),
                    data = openq.form.clean)
summary(form.part.glm)


# GJRM: fit trivariate probit 
gjrm.form <- gjrm(list(elite.formula, all.formula, part.formula),
                  data = openq.form.clean,
                  margins = c("probit", "probit", "probit"),
                  Model = "T", 
                  BivD = "T")
# No difference in AIC or convergence across copulas
conv.check(gjrm.form)
AIC(gjrm.form)
summary(gjrm.form)



### maintenance results

# start with univariate models
# elite 
main.elite.glm <- glm(elite.formula,
                      family = binomial(link = "probit"),
                      data = openq.main.clean)
summary(main.elite.glm)

# alliance attr
main.all.glm <- glm(all.formula,
                    family = binomial(link = "probit"),
                    data = openq.main.clean)
summary(main.all.glm)

# partner attr
main.part.glm <- glm(part.formula,
                     family = binomial(link = "probit"),
                     data = openq.main.clean)
summary(main.part.glm)


# GJRM: fit trivariate probit 
gjrm.main <- gjrm(list(elite.formula, all.formula, part.formula),
                  data = openq.main.clean,
                  margins = c("probit", "probit", "probit"),
                  Model = "T", 
                  BivD = "T")
# No difference in AIC or convergence across copulas
conv.check(gjrm.main)
AIC(gjrm.main)
summary(gjrm.main)



# plot results 

gjrm.sum <- function(model){
  summary.gjrm <- summary(model)
  
  variables <-  c("(Intercept)",  
                  "Partisan Strength",
                  "Isolationism", "Militant Assertiveness",
                  "Age", "Female", "White",
                  "Income", "Education", "Export Orientation")


# tabulate the results 
print(summary.gjrm[["tableP1"]]) # elite
print(summary.gjrm[["tableP2"]]) # alliance
print(summary.gjrm[["tableP3"]]) # partner
 
# Combine all the results in a single table. 
# Start with elite table
elite.tab <- as.data.frame(summary.gjrm[["tableP1"]][, 1:2])
elite.tab$variable <- factor(variables,
                                ordered = TRUE,
                                levels = rev(variables))

# table for alliance attr
all.tab <- as.data.frame(summary.gjrm[["tableP2"]][, 1:2])
all.tab$variable <- factor(variables,
                               ordered = TRUE,
                               levels = rev(variables))
# table for partner attr
part.tab <- as.data.frame(summary.gjrm[["tableP2"]][, 1:2])
part.tab$variable <- factor(variables,
                                ordered = TRUE,
                                levels = rev(variables))

joint.tab <- rbind.data.frame(elite.tab, all.tab, part.tab)
joint.tab$equation <- c(rep("Elite Support", 10),
                     rep("Alliance Attribute", 10),
                     rep("Partner Attribute", 10))
joint.tab

}

# output and combine
form.gjrm.res <- gjrm.sum(gjrm.form)
form.gjrm.res$Experiment <- "Formation"
main.gjrm.res <- gjrm.sum(gjrm.main)
main.gjrm.res$Experiment <- "Maintenance"

full.gjrm.res <- bind_rows(form.gjrm.res, main.gjrm.res) %>%
                   filter(variable != "(Intercept)")

# plot probit coefs 
ggplot(full.gjrm.res, aes(x = Estimate, y = variable,
                          group = Experiment,
                          color = Experiment)) +
  facet_wrap(~ equation) +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = Estimate - 1.96*`Std. Error`,
                      xmax = Estimate + 1.96*`Std. Error`),
                  position = position_dodge(width = 0.5)) +
  labs(title = "Trivariate Probit Models of Open-Ended Question Content",
       y = "Variable")
ggsave("appendix/open-questions-res.png", height = 6, width = 8)
  




