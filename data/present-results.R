# Joshua Alley
# Results for the manuscript

# given number of attributes and no color, need more legible figures

# choice for alliance maintenance
main.choice.plot <- plot(filter.cregg(amce.main.choice)) + 
  theme(legend.position = "none", axis.text.y = element_text(size = 10)) +
  xlim(-.07, .13) +
  scale_color_manual(values = rep("black", 10)) +
  labs(title = "Maintain Alliance?")
main.choice.plot


# choice for alliance formation 
form.choice.plot <- plot(filter.cregg(amce.form.choice)) + 
  theme(legend.position = "none", axis.text.y = element_text(size = 10)) +
  xlim(-.07, .13) +
  scale_color_manual(values = rep("black", 10)) +
  labs(title = "Form Alliance?")
form.choice.plot



# combine formation and maintenance choice plots
grid.arrange(form.choice.plot, main.choice.plot, ncol= 2)
joint.amce.plots <- arrangeGrob(form.choice.plot, main.choice.plot, ncol = 2)
ggsave("figures/joint-amce-plots.png", joint.amce.plots, width = 10, height = 8)



# partisanship
mmplot.form.part <- plot(filter.cregg(mm.form.part), group = "party.id") +
  facet_wrap(~BY, ncol = 3L) + 
  theme_grey() +
  theme(legend.position = "none", axis.text.y = element_text(size = 8)) +
  ggtitle("Party ID and Alliance Formation")
mmplot.form.part





### three way inter: party and both dispositions

# need to split to make legible
# split by treatment


# maintenance first
plot(filter(filter.cregg.el(partydispo.mms.main), 
        !str_detect(BY, "Independent")), 
     group = "party.dispo", vline = .5) +
  facet_wrap(~ BY, ncol = 4L) + 
  geom_vline(aes(xintercept = mean.choice),
             linetype = "dashed") +
  theme(legend.position = "none", 
         axis.text.y = element_text(size = 11)) +
  geom_pointrange(aes(xmin = lower, xmax = upper),
                  fatten = 4) +
  scale_color_manual(values = rep("black", 9)) +
  ggtitle("Alliance Maintenance: Elite Cues, Partisanship and FP Dispositions")
ggsave("figures/party-dispo-main-el.png", height = 11, width = 11)

# formation 
plot(filter(filter.cregg.el(partydispo.mms.form), !str_detect(BY, "Independent")), 
     group = "party.dispo", vline = .5) +
  geom_pointrange(aes(xmin = lower, xmax = upper),
                  fatten = 4) +
  facet_wrap(~ BY, ncol = 4L) + 
  geom_vline(aes(xintercept = mean.choice),
             linetype = "dashed") +
  theme(legend.position = "none",
            axis.text.y = element_text(size = 11)) +
  scale_color_manual(values = rep("black", 9)) +
  ggtitle("Alliance Formation: Elite Cues, Partisanship and FP Dispositions")
ggsave("figures/party-dispo-form-el.png", height = 11, width = 11)



# alliance characteristics

# maintenance first
plot(filter(filter.cregg.char(partydispo.mms.main), 
            !str_detect(BY, "Independent")), 
     group = "party.dispo", vline = .5) +
  facet_wrap(~ BY, ncol = 4L) + 
  geom_vline(aes(xintercept = mean.choice),
             linetype = "dashed") +
  theme(legend.position = "none", 
          axis.text.y = element_text(size = 11)) +
  geom_pointrange(aes(xmin = lower, xmax = upper),
                  fatten = 4) +
  scale_color_manual(values = rep("black", 9)) +
  ggtitle("Alliance Maintenance: Alliance Characteristics, Partisanship and FP Dispositions")
ggsave("appendix/party-dispo-main-char.png", height = 11, width = 11)

# formation 
plot(filter(filter.cregg.char(partydispo.mms.form), !str_detect(BY, "Independent")), 
     group = "party.dispo", vline = .5) +
  geom_pointrange(aes(xmin = lower, xmax = upper),
                  fatten = 4) +
  facet_wrap(~ BY, ncol = 4L) + 
  geom_vline(aes(xintercept = mean.choice),
             linetype = "dashed") +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 11)) +
  scale_color_manual(values = rep("black", 9)) +
  ggtitle("Alliance Formation: Alliance Characteristics, Partisanship and FP Dispositions")
ggsave("appendix/party-dispo-form-char.png", height = 11, width = 11)
