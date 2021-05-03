# Joshua Alley
# estimate population AMCE 
# using model-based exploratory analysis


# w/ FactorEx package

# atop dyadic data
atop.dyad <- read.csv("data/atop5_0dy.csv")
# No US in mem2, so filter as follows for US commitments
us.ally <- filter(atop.dyad, year == 2018 & mem1 == 2 &
                    defense == 1) %>%
             select(mem2, atopid1, atopid2, atopid3, atopid4) %>%
             rename(ccode = mem2,
               atopid = atopid1) 

# load ATOP alliance-level data 
# filter atop to defense pacts and select key variables
atop.alliance <- read.csv("data/atop5-alliance.csv") %>%
  filter(defense == 1 &
          atopid %in% us.ally$atopid) %>%
  select(atopid, 
         defcon, thirdcom, 
         organ1, ecaid, base, milcon, intcom) %>%
  mutate(
    # basing rights for US
    defense.coop = ifelse(base >= 2, "Basing Rights", 
                          ifelse(milcon == 1, "Shared Command",
                                 ifelse(organ1 != 0, "International Organization",
                                        "No Defense Cooperation")
                          )),
    # issue linkages 
    issue.link = ifelse(ecaid > 0, "Trade and Investment",
                      ifelse(thirdcom == 1, "UN Voting",
                             "No Issue Linkage.")),
    support.cond = ifelse(atopid == 3075 | atopid == 3150 |
                            atopid == 3180 | atopid == 3215 |
                            atopid == 3260 | atopid == 3240,
                            "Specific Region",
                    ifelse(atopid == 3375 |
                           atopid == 3215 | atopid == 3210, "Nonprovocation",
                          "Unconditional"))
    
  )
coop.freq <- table(atop.alliance$defense.coop)
coop.freq
linkage.freq <- table(atop.alliance$issue.link)
linkage.freq
table(atop.alliance$support.cond)

# clean up US ally data
us.ally <- us.ally %>%
  left_join(select(atop.alliance, atopid,
                   defense.coop, issue.link,
                   support.cond))
# tricky thing w/ overlapping obligations in some

# ipe data resource
load("data/Graham_Tucker_IPE_v4.rdata") 
ipe_v4 <- ipe_v4 %>%
        filter(year == 2017 & ccode != 2) %>%
         select(ccode, year, milex_SI, gdp_WDI_PW,
                cinc_MC, milper_MC,
                polity2_P4, v2x_libdem_VDEM, v2x_polyarchy_VDEM,
                trade_WDI, eco_glob_KOF,
                fdiflows_UNCTAD, fdistocks_UNCTAD,
                ind_total_OFS) %>%                           
          mutate(
                region = ifelse(ccode < 200, "The Americas.", # Americas 
                  ifelse(ccode %in% 200:400, "Europe", # Europe
                  ifelse(ccode > 400 & ccode < 600, "Africa", # subs Africa
                  ifelse(ccode >= 600 & ccode < 700, "The Middle East", # MENA
                  ifelse(ccode > 700, "Asia", 0))))), # Asia
                # political regime
                regime = ifelse(polity2_P4 >= 6, "Democracy",
                            ifelse(polity2_P4 < 6 & polity2_P4 >= -6, "Weak Democracy",
                                   "Not a Democracy"))
                )
ipe_v4$milex_SI <- as.numeric(ipe_v4$milex_SI)
# join the two datasets
us.ally.full <- left_join(us.ally, ipe_v4) %>%
          mutate(
             # trade ties 
             trade = ifelse(fdiflows_UNCTAD >= 7579.5, "Extensive Trade",
               ifelse( fdiflows_UNCTAD < 7579.5 & fdiflows_UNCTAD > 360.6,
                       "Modest Trade", "Minimal Trade")),
               # capability
               cap = ifelse(milex_SI >= 1.957989e+04, # turkey or higher
                               "High Capability", 
                      ifelse(milex_SI < 1.957989e+04 & milex_SI > 5.164968e+03,
                             "Moderate Capability", "Low Capability")),
               # alliance cost
               cost = ifelse(atopid == 3180 | atopid == 3375 | atopid == 3240,
                             "$15 billion", # NATO, Japan, Korea
                        ifelse(ccode == 840 | ccode == 900 | # phillipines, australia
                                ccode == 770, "$10 billion", # pakistan
                               "$5 billion" 
                                 )),
              # military cooperation
              mil.coop = ifelse(atopid == 3180, 
                                sample(c("At War Together", "Support Airstrikes",
                                "Support Counterinsurgency")),
                          ifelse(ccode == 900, "Support Counterinsurgency", # Australia
                              "No Recent Cooperation")),
              shared.threat = ifelse(region == "Asia",
                                      "Serious Threat",
                                ifelse(region == "Europe" | region == "The Middle East", "Mild Threat",
                                       "Minimal Threat")
                                   ),
              # democratic senator support
              dem.supp = ifelse(ccode == 640 | ccode == 800 |
                                  ccode == 770, # Turkey, thailand and pakistan
                  "Dem. Senate Oppose", "Dem. Senate Support"),
             # republican senator support
              rep.supp = ifelse(ccode == 640 | ccode == 366 | ccode == 367 |
                                  ccode == 368 | # turkey and baltics
                                  ccode == 770 | # pakistan
                                  ccode == 339 | ccode == 341, # fmr. yugoslavia
                  "Rep. Senate Oppose", "Rep. Senate Support"),
             # joint chiefs support
            jcs.supp = ifelse(ccode == 800,
                      "JCS Oppose", "JCS Support"),
            # state department
            state.supp = ifelse(ccode == 800,
              "State Oppose", "State Support")
                   )
us.ally.full$cap[is.na(us.ally.full$cap)] <- "Low Capability"
us.ally.full$regime[is.na(us.ally.full$regime)] <- "Democracy"
us.ally.full$regime[us.ally.full$ccode == 800] <- "Not a Democracy" # Thailand
us.ally.full$issue.link[us.ally.full$atopid == 3180] <- "UN Voting"



### establish list of characteristics
# Political leaders
rep.sen <- c(.4, .6)
names(rep.sen) <- levels(main.data$rep.supp)
rep.sen

dem.sen <- c(.2, .8)
names(dem.sen) <- levels(main.data$dem.supp)
dem.sen
# military and diplomatic
mil.lead <- c(.1, .9)
names(mil.lead) <- levels(main.data$jcs.supp)
mil.lead

dip.lead <- c(.1, .9)
names(dip.lead) <- levels(main.data$state.supp)
dip.lead

# trade data
trade.marg <- c(.5, .25, .25)
names(trade.marg) <- levels(main.data$trade)
trade.marg

# regime marg
regime.marg <- c(.1, .7, .2)
names(regime.marg) <- levels(main.data$regime)
regime.marg

# military capability
cap.marg <- c(.75, .05, .2)
names(cap.marg) <- levels(main.data$cap)
cap.marg

# shared threat
threat.marg <- c(.5, .35, .15)
names(threat.marg) <- levels(main.data$shared.threat)
threat.marg

# recent cooperation
milc.marg <- c(.46, .18, .18, .18)
names(milc.marg) <- levels(main.data$mil.coop)
milc.marg

# financial cost TODO: add levels
cost.marg <- c(.6, .3, .1)
names(cost.marg) <- levels(main.data$cost)
cost.marg

# support conditions
cond.marg <- c(.5, .25, .25)
names(cond.marg) <- levels(main.data$support.cond)
cond.marg

# defense cooperation
coop.marg <- c(.56, .21, .17, .07)
names(coop.marg) <- levels(main.data$defense.coop)
coop.marg

# issue linkages
linkage.marg <- c(.86, .07, .07)
names(linkage.marg) <- levels(main.data$issue.link)
linkage.marg

# region
region.marg <- c(.03, .1, .4, .4, .07)
names(region.marg) <- levels(main.data$region)
region.marg


# create a list
marginal.attr <- list(dem.sen, rep.sen, mil.lead, dip.lead, 
                       threat.marg,  region.marg,
                       regime.marg, trade.marg, cond.marg,
                       cost.marg, 
                       coop.marg, linkage.marg, cap.marg,  
                       milc.marg)
names(marginal.attr) <- c("dem.supp", "rep.supp", "jcs.supp", "state.supp",
                          "shared.threat", "region",
                          "regime", "trade", "support.cond", "cost",
                          "defense.coop", "issue.link", "cap",
                           "mil.coop")

# load up key data for pre-reg
us.ally.key <- select(us.ally.full, ccode, atopid,
                      names(marginal.attr))

# manually pull region- no Africa creates target dist problems 
us.ally.key <- select(us.ally.key, -c(region, ccode, atopid))
class(us.ally.key) <- "data.frame"


# model: formation 
# get complete cases
form.data.comp <- form.data %>%
                select(ResponseId, choice, names(marginal.attr))
form.data.comp <- form.data.comp[complete.cases(form.data.comp), ]
class(form.data.comp) <- "data.frame"
# fit model
form.pop <- model_pAMCE(formula = choice ~ dem.supp + rep.supp + jcs.supp + state.supp +
                          shared.threat + region +
                          regime + trade + support.cond + cost +
                          defense.coop + issue.link + cap + mil.coop,
                        data = form.data.comp,
                        reg = TRUE, cross_int = TRUE,
                        pair = FALSE,
                        cluster_id = form.data.comp$ResponseId,
                        target_dist = marginal.attr,
                        target_type = "marginal",
                        boot = 1000, seed = 12)
summary(form.pop, sample = TRUE)
plot(form.pop, diagnose = TRUE)


# model: maintenance
# Use observed US alliances as the target distribution
# get complete cases 
main.data.comp <- main.data %>%
  select(ResponseId, choice, names(marginal.attr))
main.data.comp <- main.data.comp[complete.cases(main.data.comp), ]
class(main.data.comp) <- "data.frame"
# fit model
main.pop.marg <- model_pAMCE(formula = choice ~ dem.supp + rep.supp + jcs.supp + state.supp +
                          shared.threat + region +
                          regime + trade + support.cond + cost +
                          defense.coop + issue.link + cap + mil.coop,
                        data = main.data.comp,
                        reg = TRUE, cross_int = TRUE,
                        pair = FALSE,
                        cluster_id = main.data.comp$ResponseId,
                        target_dist = marginal.attr,
                        target_type = "marginal",
                        boot = 1000, seed = 12)
summary(main.pop.marg, sample = TRUE)
plot(main.pop.marg, diagnose = TRUE)



# Use observed US alliances as the target distribution
main.pop <- model_pAMCE(formula = choice ~ dem.supp + rep.supp + jcs.supp + state.supp +
                            shared.threat +
                            regime + trade + support.cond + cost +
                            defense.coop + issue.link + cap + mil.coop,
                         data = main.data.comp,
                         reg = TRUE, cross_int = TRUE,
                         pair = FALSE,
                         cluster_id = main.data.comp$ResponseId,
                         target_dist = list(us.ally.key),
                         target_type = "target_data",
                         boot = 1000, seed = 12)
summary(main.pop, sample = TRUE)
plot(main.pop, diagnose = TRUE)
