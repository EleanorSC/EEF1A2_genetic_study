## ---------------------------
##
## Notes: Initial exploratory analysis of EEF1A2 survey data
##        Takes excel document with survey response data and creates plots of participant responses 
##        IMPACT VS SEVERITY PLOT
##        
##   
##        This plot examines the relative severity of different symptoms of EEF1A2 against impact on caregiver
##        For the individual subdomains of:
##
##        (1) medical symptoms (seizures, incontinence, sleep issues, gross motor control, fine motor control)
##        (2) cognitive symptoms (memory, verbal comprehension, verbal fluency, ability to learn new things, engangement)
##        (3) behavioural symptoms (emotional regulation, hyperactivity, aggression, staying focused and self-injury)
##        
##        Total severity of symptoms was associated with overall impact perceived (R = 0.84, p = 7.5 x 10-7)
##
## Packages: install.packages("ggplot2")
##           
##   
##   
##           
## ---------------------------

#severity medical scores 
impact <- read.csv(file = "impact_plot.csv")

impact$symptom <- as.factor(as.character(impact$symptom))

impact$age <- factor(impact$age, levels = c("infant",
                                            "preschool", 
                                            "school", 
                                            "adult"))

impact$type <- factor(impact$type, levels = c("medical",
                                              "cognitive", 
                                              "behavioural"))

impact$symptom <- factor(impact$symptom, 
                         levels = c("seziures",
                                    "incontinence",
                                    "sleep_issues",
                                    "gross_motor_control",
                                    "fine_motor_control",
                                    "memory",
                                    "verbal_comprehension",
                                    "verbal_fluency",
                                    "learning_new_things",
                                    "engagement",
                                    "emotional_regulation",
                                    "hyperactivity",
                                    "agression",
                                    "staying_focused",
                                    "self_injury"))


####Impact vs Severity graph
ggplot(impact, aes(x = severity, y = impact, color = type)) +
  geom_point(size =0.3, alpha = 0.7) +
  geom_jitter(size =0.3) +
  geom_smooth(aes(color = type,
                  fill = type),
              method = lm) +
  facet_wrap( ~ symptom, ncol = 5) +
  xlim(0, 6) +
  ylim(0, 6) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_colour_manual(values = c("violetred4", "indianred1", "royalblue1")) +
  scale_fill_manual(values = c("deeppink3", "indianred1", "cornflowerblue")) 



####Impact v SWLS graph
ggplot(impact, aes(x=severity, y=SWLS, color=type)) + 
  geom_point(alpha=0.7)+
  geom_smooth(aes(color = type, 
                  fill = type),
              method=lm) +
  facet_wrap(~symptom, ncol=5) +
  theme_bw() +
  xlim(1,5) +
  xlab("Symptom severity") +
  theme(axis.title.y = element_text(size=11, face="bold"),
        axis.title.x = element_text(size=11, face="bold")) +
  scale_colour_manual(values = c("violetred4", "indianred1", "royalblue1")) +
  scale_fill_manual(values = c("deeppink3", "indianred1", "cornflowerblue")) 
#####

