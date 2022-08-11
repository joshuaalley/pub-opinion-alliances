# Joshua Alley
# Public Opinion towards military alliances 

# estimate AMCE of different alliance components
# and analyze subgroups

### NATO example data
nato.top <- read.csv("data/nato-toplines.csv")
glimpse(nato.top)
   # reshape
nato.top.long <- nato.top %>%
                  filter(elite == 0 |
                        Notes != "Essential/Not.") %>% # drop elite surveys and essential q 
                  select(year, perc.supp, perc.opp,
                         perc.none) %>%
                pivot_longer(-year)
# factor for color order
nato.top.long$name <- factor(nato.top.long$name, levels = unique(nato.top.long$name))
# plot
ggplot(nato.top.long, aes(x = year, y = value, 
                      group = name, color = name)) +
   geom_point() +
   geom_smooth(method = "loess") +
   scale_color_grey(labels = c("Support", "Oppose",
                               "None/Neutral")) +
   labs(x = "Year", y = "Percentage of Respondents",
        color = "Attitude", 
        subtitle = "Data from Roper iPoll") +
  ggtitle("US Public Support for NATO: 1974-2020") +
  theme_bw()
ggsave("figures/nato-op-time.png", height = 6, width = 8)


### analysis
# set formulas
choice.formula <- formula(choice ~ dem.supp + rep.supp + jcs.supp + state.supp +
                              shared.threat + region +
                              regime + trade + support.cond + cost +
                              defense.coop + issue.link + cap + mil.coop) #gender)

rate.formula <- formula(rating ~ dem.supp + rep.supp + jcs.supp + state.supp +
                       shared.threat + region +
                       regime + trade + support.cond + cost +
                       defense.coop + issue.link + cap + mil.coop) #gender)



# choice
plot(mm(main.data, choice.formula,
        id = ~ ResponseId))

amce.main.choice <- amce(main.data, choice.formula,
                         id = ~ ResponseId)
main.choice.plot <- plot(amce.main.choice) + 
  theme(legend.position = "none", axis.text.y = element_text(size = 8)) +
  xlim(-.07, .13) +
  labs(title = "Maintain Alliance?")
main.choice.plot


# rating
plot(mm(main.data, rate.formula,
        id = ~ ResponseId))

amce.main.rate <- amce(main.data, rate.formula,
                       id = ~ ResponseId)
main.rate.plot <- plot(amce.main.rate) + theme(legend.position = "none", axis.text.y = element_text(size = 8)) +
  xlim(-4, 6.5) +
  labs(title = "Existing Alliance Rating")
main.rate.plot


# combine maintenance choice and rating plots 
grid.arrange(main.choice.plot, main.rate.plot, ncol= 2)
maintenance.plots <- arrangeGrob(main.choice.plot, main.rate.plot, ncol = 2)
ggsave("appendix/maintenance-plots.png", maintenance.plots, width = 10, height = 8)


# partisan subgroups
mm.main.part <- cj(main.data, choice.formula,
                       estimate = "mm",
                       id = ~ ResponseId, by = ~ party.id)
mmplot.main.part <- plot(filter.cregg(mm.main.part), group = "party.id") +
                      facet_wrap(~BY, ncol = 3L) + 
                    theme_grey() + 
                      theme(legend.position = "none", 
                           axis.text.y = element_text(size = 8)) +
  ggtitle("Party ID and Alliance Maintenance")
mmplot.main.part

# differences in marginal means
mmd.main.part <- cj(main.data, choice.formula,
                   estimate = "mm_differences",
                   id = ~ ResponseId, by = ~ party.id)
mmdiff.main.part <- plot(filter.cregg(mmd.main.part)) +
                     facet_wrap(~BY, ncol = 3L) + 
                     theme_grey() +
                     theme(legend.position = "none", 
                          axis.text.y = element_text(size = 8))
mmdiff.main.part
# plot marginal means and differences
grid.arrange(mmplot.main.part, mmdiff.main.part)
part.main.plot <- arrangeGrob(mmplot.main.part, mmdiff.main.part)


main.part.plot <- plot(rbind(mm.main.part, mmd.main.part)) + 
  theme(legend.position = "none", axis.text.y = element_text(size = 8)) +
  facet_wrap(~BY, ncol = 3L, scales = "free_x") + 
  theme_grey() +
  ggtitle("Party ID and Alliance Maintenance")
main.part.plot
# f-test shows clear differences
cj_anova(main.data, choice.formula,
         id = ~ ResponseId, by = ~ party.id)


# split by export interests 
mm.main.econ <- cj(main.data, choice.formula,
                     estimate = "mm",
                     id = ~ ResponseId, by = ~ exports.fac)
plot(mm.main.econ, group = "exports.fac")
# differences in marginal means
mmd.main.econ <- cj(main.data, choice.formula,
                               estimate = "mm_differences",
                             id = ~ ResponseId, by = ~ exports.fac)
plot(mmd.main.econ, group = "exports.fac")

exports.main <- plot(rbind(mm.main.econ, mmd.main.econ)) + 
                  facet_wrap(~BY, ncol = 3L) + theme(legend.position = "none", axis.text.y = element_text(size = 8)) +
                  ggtitle("Net Exports and Alliance Maintenance")
exports.main

# f-test shows clear differences
cj_anova(main.data[!is.na(main.data$exports.fac), ], choice.formula,
         id = ~ ResponseId, by = ~ exports.fac)


# split by isolationism
mm.main.isol <- cj(main.data, choice.formula,
                     estimate = "mm",
                     id = ~ ResponseId, by = ~ isolation.fac)
plot(mm.main.isol, group = "isolation.fac")
# differeneces
mmd.main.isol  <- cj(main.data, choice.formula,
                       estimate = "mm_differences",
                       id = ~ ResponseId, by = ~ isolation.fac)
plot(mmd.main.isol, group = "isolation.fac")
isol.main <- plot(rbind(filter.cregg(mm.main.isol),
                        filter.cregg(mmd.main.isol))) + 
              facet_wrap(~BY, ncol = 3L) + 
              theme_grey() +
              theme(legend.position = "none", 
                    axis.text.y = element_text(size = 6)) +
              ggtitle("Isolationism and Alliance Maintenance")
isol.main
# f-test shows clear differences
cj_anova(main.data[!is.na(main.data$isolation.fac), ], choice.formula,
         id = ~ ResponseId, by = ~ isolation.fac)


# split by militant assertiveness 
mm.main.milint <- cj(main.data, choice.formula,
                     estimate = "mm",
                     id = ~ ResponseId, by = ~ mil.inter.fac)
plot(mm.main.milint, group = "mil.inter.fac")
# clear differences,
mmd.main.milint  <- cj(main.data, choice.formula,
                       estimate = "mm_differences",
                       id = ~ ResponseId, by = ~ mil.inter.fac)
plot(mmd.main.milint, group = "mil.inter.fac")
hawk.main <- plot(rbind(filter.cregg(mm.main.milint),
                        filter.cregg(mmd.main.milint))) + 
               facet_wrap(~BY, ncol = 3L) + 
               theme_grey() +
               theme(legend.position = "none", 
                    axis.text.y = element_text(size = 6)) +
               ggtitle("Militant Assertiveness and Alliance Maintenance")
hawk.main
# f-test shows clear difference
cj_anova(main.data[!is.na(main.data$mil.inter.fac), ], choice.formula,
         id = ~ ResponseId, by = ~ mil.inter.fac)





### formation analysis ### 


# choice
plot(mm(form.data, choice.formula,
        id = ~ ResponseId))

amce.form.choice <- amce(form.data, choice.formula,
                         id = ~ ResponseId)
form.choice.plot <- plot(amce.form.choice) +
                   theme(legend.position = "none", 
                    axis.text.y = element_text(size = 8)) +
                     xlim(-.07, .13) +
                     labs(title = "Form Alliance?")
form.choice.plot
ggsave("appendix/formation-plot.png", form.choice.plot, width = 8, height = 8)


# rating
plot(mm(form.data, rate.formula,
        id = ~ ResponseId))

amce.form.rate <- amce(form.data, rate.formula,
                       id = ~ ResponseId)
form.rate.plot <- plot(amce.form.rate) + theme(legend.position = "none", axis.text.y = element_text(size = 8)) +
                     xlim(-3, 6) +
                     labs(title = "New Alliance Rating")
form.rate.plot

# combine formation choice and rating plots 
grid.arrange(form.choice.plot, form.rate.plot, ncol= 2)
formation.plots <- arrangeGrob(form.choice.plot, form.rate.plot, ncol = 2)
ggsave("appendix/formation-plots.png", formation.plots, width = 10, height = 8)


# partisan subgroups
mm.form.part <- cj(form.data, choice.formula,
                   estimate = "mm",
                   id = ~ ResponseId, by = ~ party.id)
mmplot.form.part <- plot(filter.cregg(mm.form.part), group = "party.id") +
  facet_wrap(~BY, ncol = 3L) + 
  theme_grey() +
  theme(legend.position = "none", axis.text.y = element_text(size = 8)) +
  ggtitle("Party ID and Alliance Formation")
mmplot.form.part

# differences in marginal means
mmd.form.part <- cj(form.data, choice.formula,
                    estimate = "mm_differences",
                    id = ~ ResponseId, by = ~ party.id)
mmdiff.form.part <- plot(filter.cregg(mmd.form.part)) +
  facet_wrap(~BY, ncol = 3L) + 
  theme_grey() +
  theme(legend.position = "none", axis.text.y = element_text(size = 8))
mmdiff.form.part
# plot marginal means and differences
grid.arrange(mmplot.form.part, mmdiff.form.part)
part.form.plot <- arrangeGrob(mmplot.form.part, mmdiff.form.part)


# combine formation and maintenance partisanship plots
grid.arrange(part.form.plot, part.main.plot, ncol= 2)
joint.part.plots <- arrangeGrob(part.form.plot, part.main.plot, ncol= 2)
ggsave("appendix/joint-part-plots.png", joint.part.plots, width = 12, height = 10)



# f-test shows clear differences
cj_anova(form.data, choice.formula,
         id = ~ ResponseId, by = ~ party.id)
# amce difference
plot(amce_diffs(form.data, choice.formula,
                id = ~ ResponseId, by = ~ party.id)) +
      facet_wrap(~ BY, ncol = 3L)



# split by export interests 
mm.form.econ <- cj(form.data, choice.formula,
                   estimate = "mm",
                   id = ~ ResponseId, by = ~ exports.fac)
plot(mm.form.econ, group = "exports.fac")
# clear differences
mmd.form.econ <- cj(form.data, choice.formula,
                    estimate = "mm_differences",
                    id = ~ ResponseId, by = ~ exports.fac)
plot(mmd.form.econ, group = "exports.fac")

exports.form <- plot(rbind(mm.form.econ, mmd.form.econ)) + 
                  facet_wrap(~BY, ncol = 3L) + theme(legend.position = "none", axis.text.y = element_text(size = 8)) +
                  ggtitle("Net Exports and Alliance Formation")
exports.form

# f-test shows no clear differences
cj_anova(form.data[!is.na(form.data$exports.fac), ], choice.formula,
         id = ~ ResponseId, by = ~ exports.fac)


# split by isolationism
mm.form.isol <- cj(form.data, choice.formula,
                   estimate = "mm",
                   id = ~ ResponseId, by = ~ isolation.fac)
plot(mm.form.isol, group = "isolation.fac")
# clear differences
mmd.form.isol <- cj(form.data, choice.formula,
                              estimate = "mm_differences",
                             id = ~ ResponseId, by = ~ isolation.fac)
plot(mmd.form.isol, group = "isolation.fac")
isol.form <- plot(rbind(filter.cregg(mm.form.isol),
                        filter.cregg(mmd.form.isol))) + 
              facet_wrap(~BY, ncol = 3L) + 
              theme_grey() +
              theme(legend.position = "none", 
                    axis.text.y = element_text(size = 6)) +
              ggtitle("Isolationism and Alliance Formation")
isol.form
# f-test: clear difference
cj_anova(form.data[!is.na(form.data$isolation.fac), ], choice.formula,
         id = ~ ResponseId, by = ~ isolation.fac)



# split by militant assertiveness 
mm.form.milint <- cj(form.data, choice.formula,
                     estimate = "mm",
                     id = ~ ResponseId, by = ~ mil.inter.fac)
plot(mm.form.milint, group = "mil.inter.fac")
# clear differences,
mmd.form.milint  <- cj(form.data, choice.formula,
                       estimate = "mm_differences",
                       id = ~ ResponseId, by = ~ mil.inter.fac)
plot(mmd.form.milint, group = "mil.inter.fac")
hawk.form <- plot(rbind(filter.cregg(mm.form.milint),
                        filter.cregg(mmd.form.milint))) + 
              facet_wrap(~BY, ncol = 3L) + 
              theme_grey() +
              theme(legend.position = "none",
                    axis.text.y = element_text(size = 6)) +
              ggtitle("Militant Assertiveness and Alliance Formation")
hawk.form
# f-test shows clear differences
cj_anova(form.data[!is.na(form.data$mil.inter.fac), ], choice.formula,
         id = ~ ResponseId, by = ~ mil.inter.fac)



# combine plots for isolationism
grid.arrange(isol.form, isol.main, ncol= 1)
isolation.plots <- arrangeGrob(isol.form, isol.main, ncol= 1)
ggsave("appendix/isolation-plots.png", isolation.plots, width = 10, height = 10)

# combine plots for hawkishness
grid.arrange(hawk.form, hawk.main, ncol= 1)
hawk.plots <- arrangeGrob(hawk.form, hawk.main, ncol= 1)
ggsave("appendix/hawk-plots.png", hawk.plots, width = 10, height = 10)

# all together: helpful, but illegible
grid.arrange(isolation.plots, hawk.plots)


### interactions of individual concerns and partisanship


# formation: combine militant assertiveness and hawkishness
form.data$isol.milint <- interaction(form.data$isolation.fac, 
                                      form.data$mil.inter.fac, sep = "/")
table(form.data$isol.milint)
isolhawk.mms.form <- cj(form.data, choice.formula, 
                           estimate = "mm",
                           id = ~ ResponseId,  by = ~ isol.milint)
plot(isolhawk.mms.form, group = "isol.milint", vline = 0.5) +
  facet_wrap(~ BY) + theme(legend.position = "none", 
                          axis.text.y = element_text(size = 7))

# maintenance: combine militant assertiveness and hawkishness
main.data$isol.milint <- interaction(main.data$isolation.fac, 
                                     main.data$mil.inter.fac, sep = "/")
table(main.data$isol.milint)
isolhawk.mms.main <- cj(main.data, choice.formula, 
                        estimate = "mm",
                        id = ~ ResponseId,  by = ~ isol.milint)
plot(isolhawk.mms.main, group = "isol.milint", vline = 0.5) +
  facet_wrap(~ BY) + theme(legend.position = "none", 
                          axis.text.y = element_text(size = 7))



# look at interaction of partisanship and isolationism: formation
# focus on elite cues
form.data$party.isol <- interaction(form.data$party.id, 
                                    form.data$isolation.fac, sep = "_")
table(form.data$party.isol)
partyisol.mms.form <- cj(form.data, choice.formula, 
                         estimate = "mm",
                         id = ~ ResponseId,  by = ~ party.isol)
plot(partyisol.mms.form, group = "party.isol", vline = 0.5) +
  facet_wrap(~ BY) + theme(legend.position = "none", axis.text.y = element_text(size = 8))


# look at interaction of partisanship and isolationism: maintenance
# focus on elite cues
main.data$party.isol <- interaction(main.data$party.id, 
                                    main.data$isolation.fac, sep = "_")
table(main.data$party.isol)
partyisol.mms.main <- cj(main.data, choice.formula, 
                         estimate = "mm",
                         id = ~ ResponseId,  by = ~ party.isol)
plot(partyisol.mms.main, group = "party.isol", vline = 0.5) +
  facet_wrap(~ BY) + theme(legend.position = "none", axis.text.y = element_text(size = 7))


# look at interaction of partisanship and hawkishness: formation
# focus on elite cues
form.data$party.milint <- interaction(form.data$party.id, 
                                    form.data$mil.inter.fac, sep = "_")
table(form.data$party.milint)
partyhawk.mms.form <- cj(form.data, rate.formula, 
                           estimate = "mm",
                           id = ~ ResponseId,  by = ~ party.milint)
plot(partyhawk.mms.form, group = "party.milint", vline = 50) +
  facet_wrap(~ BY) + theme(legend.position = "none", axis.text.y = element_text(size = 7))


# look at interaction of partisanship and hawkishness: maintenance
# focus on elite cues
main.data$party.milint <- interaction(main.data$party.id, 
                                    main.data$mil.inter.fac, sep = "_")
table(main.data$party.milint)
partyhawk.mms.main <- cj(main.data, rate.formula, 
                           estimate = "mm",
                           id = ~ ResponseId,  by = ~ party.milint)
plot(partyhawk.mms.main, group = "party.milint", vline = 50) +
  facet_wrap(~ BY) + theme(legend.position = "none", axis.text.y = element_text(size = 7))


# three way inter: party and both dispositions
# maintenance first
main.data$party.dispo <- interaction(main.data$party.id, 
                                      main.data$mil.inter.fac,
                                     main.data$isolation.fac,
                                     sep = "-")
table(main.data$party.dispo)


# calculate overall mean by group
party.dispo.mean.main <- main.data %>%
  group_by(party.dispo) %>%
  summarize(
    mean.choice = mean(choice, na.rm = TRUE),
    sd.choice = sd(choice, na.rm = TRUE),
    mean.rate = mean(rating, na.rm = TRUE),
    sd.rate = sd(rating, na.rm = TRUE),
    .groups = "keep"
  ) %>%
  drop_na()


partydispo.mms.main <- cj(main.data, choice.formula, 
                         estimate = "mm",
                         id = ~ ResponseId,  by = ~ party.dispo) %>%
                     left_join(party.dispo.mean.main) # overall means

plot(filter(partydispo.mms.main, !str_detect(BY, "Independent")), 
            group = "party.dispo", vline = .5) +
  facet_wrap(~ BY, ncol = 4L) + 
  geom_vline(aes(xintercept = mean.choice),
             linetype = "dashed") +
  theme(legend.position = "none", 
         axis.text.y = element_text(size = 7)) +
  scale_color_manual(values = rep("black", 9)) +
  ggtitle("Partisanship, FP Dispositions, and Alliance Maintenance")

# formation 
form.data$party.dispo <- interaction(form.data$party.id, 
                                     form.data$mil.inter.fac,
                                     form.data$isolation.fac,
                                     sep = "-")

# calculate overall mean by group
party.dispo.mean.form <- form.data %>%
  group_by(party.dispo) %>%
  summarize(
    mean.choice = mean(choice, na.rm = TRUE),
    sd.choice = sd(choice, na.rm = TRUE),
    mean.rate = mean(rating, na.rm = TRUE),
    sd.rate = sd(rating, na.rm = TRUE),
    .groups = "keep"
  ) %>%
  drop_na()


table(form.data$party.dispo)

# calculate marginal means
partydispo.mms.form <- cj(form.data, choice.formula, 
                          estimate = "mm",
                          id = ~ ResponseId,  by = ~ party.dispo) %>% 
                        left_join(party.dispo.mean.form)
plot(filter(partydispo.mms.form, !str_detect(BY, "Independent")), 
     group = "party.dispo", vline = .5) +
  facet_wrap(~ BY, ncol = 4L) + 
  geom_vline(aes(xintercept = mean.choice),
             linetype = "dashed") +
  theme(legend.position = "none",
          axis.text.y = element_text(size = 7)) +
  scale_color_manual(values = rep("black", 9)) +
  ggtitle("Partisanship, FP Dispositions, and Alliance Formation")


# include independents
# formation 
plot(partydispo.mms.form, 
     group = "party.dispo", vline = .5) +
  facet_wrap(~ BY) + 
  geom_vline(aes(xintercept = mean.choice),
             linetype = "dashed") +
  theme(legend.position = "none",
          axis.text.y = element_text(size = 4)) +
  scale_color_manual(values = rep("black", 12)) +
  ggtitle("Partisanship, FP Dispositions, and Alliance Formation")

# maintenance
plot(partydispo.mms.main, 
     group = "party.dispo", vline = .5) +
  facet_wrap(~ BY) + 
  geom_vline(aes(xintercept = mean.choice),
             linetype = "dashed") +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 4)) +
  scale_color_manual(values = rep("black", 12)) +
  ggtitle("Partisanship, FP Dispositions, and Alliance Maintenance")





# rating results for appendix
# formation
partydispo.rate.form <- cj(form.data, rate.formula, 
                          estimate = "mm",
                          id = ~ ResponseId,  by = ~ party.dispo) 
plot(filter(filter.cregg.el(partydispo.rate.form), !str_detect(BY, "Independent")), 
     group = "party.dispo", vline = 50) +
  facet_wrap(~ BY, ncol = 4L) + 
  theme(legend.position = "none",
          axis.text.y = element_text(size = 7)) +
  scale_color_manual(values = rep("black", 9)) +
  ggtitle("Rating: Partisanship, FP Dispositions, and Alliance Formation")
ggsave("appendix/party-dispo-formapp.png", height = 12, width = 12)

# maintenance
partydispo.rate.main <- cj(main.data, rate.formula, 
                           estimate = "mm",
                           id = ~ ResponseId,  by = ~ party.dispo)
plot(filter(filter.cregg.el(partydispo.rate.main), !str_detect(BY, "Independent")), 
     group = "party.dispo", vline = 50) +
  facet_wrap(~ BY, ncol = 4L) + theme(legend.position = "none",
                                      axis.text.y = element_text(size = 7)) +
  scale_color_manual(values = rep("black", 9)) +
  ggtitle("Rating: Partisanship, FP Dispositions, and Alliance Maintenance")
ggsave("appendix/party-dispo-mainapp.png", height = 12, width = 12)





### check differences by task 
# maintenance
main.data$task <- as.factor(main.data$task)
# check differences by task
cj_anova(main.data, choice.formula,
         id = ~ ResponseId, by = ~ task)
# slight difference driven by high support on task 1. 
plot(cj(main.data, choice.formula, 
        id = ~ ResponseId, estimate = "mm", by = ~ task),
     group = "task", vline = 0.5)

# formation
form.data$task <- as.factor(form.data$task)
cj_anova(form.data, choice.formula,
         id = ~ ResponseId, by = ~ task)
# no clear difference
plot(cj(form.data, choice.formula, 
        id = ~ ResponseId, estimate = "mm", by = ~ task),
     group = "task", vline = 0.5)


# check if dropping task 1 changes main results
main.data.sub <- filter(main.data, task != 1)
form.data.sub <- filter(form.data, task != 1)


# look to unconditonal AMCE: very similar results
# choice of maintenance
plot(mm(main.data.sub, choice.formula,
        id = ~ ResponseId))

amce.main.choice.sub <- amce(main.data.sub, choice.formula,
                         id = ~ ResponseId)
main.choice.plot.sub <- plot(amce.main.choice.sub) + 
  theme(legend.position = "none", axis.text.y = element_text(size = 8)) +
  xlim(-.07, .14) +
  labs(title = "Maintain Alliance?: Subset Data")
main.choice.plot.sub


# choice of formation
plot(mm(form.data.sub, choice.formula,
        id = ~ ResponseId))

amce.form.choice.sub <- amce(form.data.sub, choice.formula,
                             id = ~ ResponseId)
form.choice.plot.sub <- plot(amce.form.choice.sub) + 
  theme(legend.position = "none", axis.text.y = element_text(size = 8)) +
  xlim(-.07, .14) +
  labs(title = "Form Alliance?: Subset Data")
form.choice.plot.sub
