# Joshua Alley
# Public Opinion towards military alliances 

# estimate AMCE of different alliance components

# do not need to load design: no profile restrictions


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


# maintenance
main.data$task <- factor(main.data$task)
# check differences by task
cj_anova(main.data, choice.formula,
     id = ~ ResponseId, by = ~ task)
# no clear difference: plot is illegible
mm.task <- cj(main.data, choice.formula, 
            id = ~ ResponseId, estimate = "mm", by = ~ task)
plot(mm.task, group = "task", vline = 0.5)

# choice
plot(mm(main.data, choice.formula,
        id = ~ ResponseId))

amce.main.choice <- amce(main.data, choice.formula,
                         id = ~ ResponseId)
main.choice.plot <- plot(amce.main.choice) + theme(legend.position = "none") +
                 labs(title = "Maintain Alliance?")
main.choice.plot
ggsave("figures/maintain-plot.png", main.choice.plot, width = 8, height = 8)


# rating
plot(mm(main.data, rate.formula,
        id = ~ ResponseId))

amce.main.rate <- amce(main.data, rate.formula,
                       id = ~ ResponseId)
main.rate.plot <- plot(amce.main.rate) + theme(legend.position = "none") +
  labs(title = "Existing Alliance Rating")
main.rate.plot

# partisan subgroups
mm.main.part <- cj(main.data, choice.formula,
                       estimate = "mm",
                       id = ~ ResponseId, by = ~ republican)
plot(mm.main.part, group = "republican")
mmd.main.part <- cj(main.data, choice.formula,
                   estimate = "mm_differences",
                   id = ~ ResponseId, by = ~ republican)
# plot marginal means and differences
main.part.plot <- plot(rbind(mm.main.part, mmd.main.part)) + 
  facet_wrap(~BY, ncol = 3L) + theme(legend.position = "none") +
  ggtitle("Party ID and Alliance Maintenance")
main.part.plot
ggsave("figures/partisan-main.png", width = 10, height = 10)
# f-test shows clear differences
cj_anova(main.data, choice.formula,
         id = ~ ResponseId, by = ~ republican)


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
                  facet_wrap(~BY, ncol = 3L) + theme(legend.position = "none") +
                  ggtitle("Net Exports and Alliance Maintenance")
exports.main

# f-test shows no clear differences
cj_anova(main.data, choice.formula,
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
plot(rbind(mm.main.isol, mmd.main.isol)) + 
  facet_wrap(~BY, ncol = 3L) + theme(legend.position = "none") +
  ggtitle("Isolationism and Alliance Maintenance")
# f-test shows no clear differences
cj_anova(main.data, choice.formula,
         id = ~ ResponseId, by = ~ isolation.fac)


# split by militant internationalism 
mm.main.milint <- cj(main.data, choice.formula,
                     estimate = "mm",
                     id = ~ ResponseId, by = ~ mil.inter.fac)
plot(mm.main.milint, group = "mil.inter.fac")
# clear differences,
mmd.main.milint  <- cj(main.data, choice.formula,
                       estimate = "mm_differences",
                       id = ~ ResponseId, by = ~ mil.inter.fac)
plot(mmd.main.milint, group = "mil.inter.fac")
plot(rbind(mm.main.milint, mmd.main.milint)) + 
  facet_wrap(~BY, ncol = 3L) + theme(legend.position = "none") +
  ggtitle("Militant Internationalism and Alliance Maintenance")
# f-test shows no clear difference
cj_anova(main.data, choice.formula,
         id = ~ ResponseId, by = ~ mil.inter.fac)





### formation analysis ### 

# check differences by task
cj_anova(form.data, choice.formula,
         id = ~ ResponseId, by = ~ task)
# no clear difference

# choice
plot(mm(form.data, choice.formula,
        id = ~ ResponseId))

amce.form.choice <- amce(form.data, choice.formula,
                         id = ~ ResponseId)
form.choice.plot <- plot(amce.form.choice) + theme(legend.position = "none") +
                     labs(title = "Form Alliance?")
form.choice.plot
ggsave("figures/formation-plot.png", form.choice.plot, width = 8, height = 8)


# rating
plot(mm(form.data, rate.formula,
        id = ~ ResponseId))

amce.form.rate <- amce(form.data, rate.formula,
                       id = ~ ResponseId)
form.rate.plot <- plot(amce.form.rate) + theme(legend.position = "none") +
                     labs(title = "New Alliance Rating")
form.rate.plot

# combine formation choice and rating plots 
grid.arrange(form.choice.plot, form.rate.plot, ncol= 2)
formation.plots <- arrangeGrob(form.choice.plot, form.rate.plot, ncol = 2)
ggsave("figures/formation-plots.png", formation.plots, width = 10, height = 8)


# combine formation and maintenance choice plots
grid.arrange(form.choice.plot, main.choice.plot, ncol= 2)
joint.amce.plots <- arrangeGrob(form.choice.plot, main.choice.plot, ncol = 2)
ggsave("figures/joint-amce-plots.png", joint.amce.plots, width = 10, height = 8)



# partisan subgroups
mm.form.part <- cj(form.data, choice.formula,
                   estimate = "mm",
                   id = ~ ResponseId, by = ~ republican)
plot(mm.form.part, group = "republican")
mmd.form.part <- cj(form.data, choice.formula,
                    estimate = "mm_differences",
                    id = ~ ResponseId, by = ~ republican)
# plot marginal means and differences
form.part.plot <- plot(rbind(mm.form.part, mmd.form.part)) + 
                    facet_wrap(~BY, ncol = 3L) + theme(legend.position = "none") +
                    ggtitle("Party ID and Alliance Formation")
form.part.plot
ggsave("figures/partisan-form.png", height = 8, width = 10)
# f-test shows clear differences
cj_anova(form.data, choice.formula,
         id = ~ ResponseId, by = ~ republican)
# amce difference
plot(amce_diffs(form.data, choice.formula,
                id = ~ ResponseId, by = ~ republican))

# combine formation and maintenance partisanship plots
grid.arrange(form.part.plot, main.part.plot, ncol= 2)
joint.part.plots <- arrangeGrob(form.part.plot, main.part.plot, ncol= 2)
ggsave("figures/joint-part-plots.png", joint.part.plots, width = 12, height = 8)

# split by export interests 
mm.form.econ <- cj(form.data, choice.formula,
                   estimate = "mm",
                   id = ~ ResponseId, by = ~ exports.fac)
plot(mm.form.econ, group = "exports.fac")
# clear differences, but few respondents in one group of exports
mmd.form.econ <- cj(form.data, choice.formula,
                    estimate = "mm_differences",
                    id = ~ ResponseId, by = ~ exports.fac)
plot(mmd.form.econ, group = "exports.fac")

exports.form <- plot(rbind(mm.form.econ, mmd.form.econ)) + 
                  facet_wrap(~BY, ncol = 3L) + theme(legend.position = "none") +
                  ggtitle("Net Exports and Alliance Formation")
exports.form

# f-test shows potential differences
cj_anova(form.data, choice.formula,
         id = ~ ResponseId, by = ~ exports.fac)

# combine formation and maintenance choice plots
grid.arrange(exports.form, exports.main, ncol= 2)
joint.econ.plots <- arrangeGrob(exports.form, exports.main, ncol= 2)
ggsave("figures/joint-econ-plots.png", joint.econ.plots, width = 12, height = 8)


# split by isolationism
mm.form.isol <- cj(form.data, choice.formula,
                   estimate = "mm",
                   id = ~ ResponseId, by = ~ isolation.fac)
plot(mm.form.isol, group = "isolation.fac")
# no clear differences
mmd.form.isol <- cj(form.data, choice.formula,
                              estimate = "mm_differences",
                             id = ~ ResponseId, by = ~ isolation.fac)
plot(mmd.form.isol, group = "isolation.fac")
plot(rbind(mm.form.isol, mmd.form.isol)) + 
  facet_wrap(~BY, ncol = 3L) + theme(legend.position = "none") +
  ggtitle("Isolationism and Alliance Formation")
# f-test 
cj_anova(form.data, choice.formula,
         id = ~ ResponseId, by = ~ isolation.fac)



# split by militant internationalism 
mm.form.milint <- cj(form.data, choice.formula,
                     estimate = "mm",
                     id = ~ ResponseId, by = ~ mil.inter.fac)
plot(mm.form.milint, group = "mil.inter.fac")
# clear differences,
mmd.form.milint  <- cj(form.data, choice.formula,
                       estimate = "mm_differences",
                       id = ~ ResponseId, by = ~ mil.inter.fac)
plot(mmd.form.milint, group = "mil.inter.fac")
plot(rbind(mm.form.milint, mmd.form.milint)) + 
  facet_wrap(~BY, ncol = 3L) + theme(legend.position = "none") +
  ggtitle("Militant Internationalism and Alliance Formation")
# f-test shows potential
cj_anova(form.data, choice.formula,
         id = ~ ResponseId, by = ~ mil.inter.fac)




# look at interaction of partisanship and isolationism: formation
# focus on elite cues
form.data$party.isol <- interaction(form.data$republican, 
                                       form.data$isolation.fac, sep = "_")
table(form.data$party.isol)
interaction.mms.form <- cj(form.data, choice ~ region + dem.supp + rep.supp +
                        regime + trade + jcs.supp + state.supp , 
                      estimate = "mm_differences",
                     id = ~ ResponseId,  by = ~ party.isol)
plot(interaction.mms.form, group = "party.isol", vline = 0.0)


# look at interaction of partisanship and isolationism: maintenance
# focus on elite cues
main.data$party.isol <- interaction(main.data$republican, 
                                    main.data$isolation.fac, sep = "_")
table(main.data$party.isol)
interaction.mms.main <- cj(main.data, choice ~ region + dem.supp + rep.supp +
                        regime + trade + jcs.supp + state.supp , 
                      estimate = "mm_differences",
                      id = ~ ResponseId,  by = ~ party.isol)
plot(interaction.mms.main, group = "party.isol", vline = 0.0)



# look at interaction of partisanship and hawkishness: formation
# focus on elite cues
form.data$party.milint <- interaction(form.data$republican, 
                                    form.data$mil.inter.fac, sep = "_")
table(form.data$party.milint)
interaction.mms.form <- cj(form.data, choice ~ region + dem.supp + rep.supp +
                             regime + trade + jcs.supp + state.supp , 
                           estimate = "mm_differences",
                           id = ~ ResponseId,  by = ~ party.milint)
plot(interaction.mms.form, group = "party.milint", vline = 0.0)


# look at interaction of partisanship and hawkishness: maintenance
# focus on elite cues
main.data$party.milint <- interaction(main.data$republican, 
                                    main.data$mil.inter.fac, sep = "_")
table(main.data$party.milint)
interaction.mms.main <- cj(main.data, choice ~ region + dem.supp + rep.supp +
                             regime + trade + jcs.supp + state.supp , 
                           estimate = "mm_differences",
                           id = ~ ResponseId,  by = ~ party.milint)
plot(interaction.mms.main, group = "party.milint", vline = 0.0)




## look at open-ended questions
form.open <- read.csv("data/formation-open-question.csv")
main.open <- read.csv("data/maintenance-open-question.csv")

# formation data
lapply(form.open[, 2:8], function(x) sum(x, na.rm = TRUE))
table(form.open$other)
# maintenance data
lapply(main.open[, 2:8], function(x) sum(x, na.rm = TRUE))
table(main.open$other)




