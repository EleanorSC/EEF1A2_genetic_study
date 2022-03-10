## ---------------------------
##
## Notes: Initial exploratory analysis of EEF1A2 survey data
##        Takes excel document with survey response data per day and creates plots of participant responses 
##        
##   
##        setwd("/Users/eleanorc_worklaptop/repos/EEF1A2_genetic_study")
##
## Packages: install.packages("ggplot2")
##           install.packages("ggpubr")
##   
##   
##           
## ---------------------------

install.packages("ggplot2")
install.packages("ggpubr")

library(ggplot2)
library(ggpubr)

#severity medical scores 
regressions<- read.csv(file = "regressions_dec.csv")

shapiro.test(regressions$Seizures_impact)
#W = 0.8574, p-value = 0.004602 - output, not normal.

shapiro.test(regressions$Toileting_issues_impact)
#W = 0.8574, p-value = 0.004602 - output, not normal.

res <- cor.test(regressions$Seizures_impact, regressions$Seizures_severity, 
                method = "spearman")
res

ggscatter(regressions, x = "Toileting_issues_severity", y = "Total_impact", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "severity", ylab = "impact") 

summary(lm(scale(Pro.science) ~ 
             scale(Total_impact) + 
             scale(Total_severity) + 
             scale(Parent_age) + 
             scale(age_diagnosis) +
             scale(SWLS) +
             sex +
             scale(patient_age), data=regressions))

### What's the biggest driver of overall impact of EEF1A2 ?

summary(lm(scale(Total_severity) ~ 
             scale(Toileting_issues_severity) + 
             scale(Sleep_issues_severity) + 
             scale(Seizures_severity) + 
             scale(speaking_severity) +
             scale(Staying_focused_severity) +
             scale(Self_injury_severity) +
             scale(Memory_severity) +
             scale(Aggression_severity) +
             scale(Learning_new_things_severity) +
             scale(hyperactivity_severity) +
             scale(gross_motor_control_severity)+
             scale(fine_motor_control_severity) +
             scale(engagement_severity) +
             scale(emotional_regulation_severity) +
          #   scale(Parent_age) + 
          #   scale(age_diagnosis) +
          #   scale(SWLS) +
             sex +
             scale(patient_age), 
          data=regressions))


summary(lm(scale(Toileting_issues_severity) ~
             scale(Toileting_issues_impact) +
             scale(patient_age) + 
             sex, 
           data=regressions))

summary(lm(cure_EEF1A2 ~ 
             scale(Total_impact) +
             scale(Total_severity), data=regressions))

regressions$sex <- as.factor(regressions$sex)

x <- ggplot(regressions,
            aes(x=Pro.genetics,
                y=Pro.science,
                colour=Pro.science)) +
  
  geom_point(size = 2, alpha =0.7) +
  geom_smooth(method= "lm",se=FALSE) +
  
  geom_hline(yintercept = 0, linetype = "dotted") +
  
  theme_bw() + 
  xlab("Total impact of EEF1A2") + 
  ylab("Attitude to genetic research") +
  theme(#axis.text.x = element_blank(),
        #axis.text.y = element_blank(),
    axis.title.y = element_text(size=11, face="bold"),
        axis.title.x = element_text(size=11, face="bold")) +
  ylim(-0.5, 1.0) +
  xlim(-0.5, 1.0)
  
  x +
   scale_colour_brewer(palette = "Greens") 
 
#########
  
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


nb.cols <- 3
mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(nb.cols)

####Impact v Severity graph
ggplot(impact, aes(x=SWLS, y=impact, color=type)) + 
  geom_point(alpha=0.7)+
  geom_smooth(aes(color = type, 
                  fill = type),
              method=lm) +
  geom_jitter()+
  facet_wrap(~symptom, ncol=5) +
  xlim(0,6) +
  ylim(0,6) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  scale_colour_manual(values = c("violetred4", "indianred1", "royalblue1")) +
  scale_fill_manual(values = c("deeppink3", "indianred1", "cornflowerblue")) 


####Impact v SWLS graph
ggplot(impact, aes(x=severity, y=SWLS, color=type)) + 
  geom_point(alpha=0.7)+
  geom_smooth(aes(color = type, 
                  fill = type),
              method=lm) +
  geom_jitter()+
  facet_wrap(~symptom, ncol=5) +
  theme_bw() +
  xlim(1,5) +
  xlab("Symptom severity") +
  theme(axis.title.y = element_text(size=11, face="bold"),
        axis.title.x = element_text(size=11, face="bold")) +
  scale_colour_manual(values = c("violetred4", "indianred1", "royalblue1")) +
  scale_fill_manual(values = c("deeppink3", "indianred1", "cornflowerblue")) 
#####



#### Age v Impact
ggplot(impact, aes(x=Parent_age, y=impact, color=type)) + 
  geom_point(alpha=0.8)+
  geom_smooth(aes(color = type, fill = type),method=lm) +
  geom_jitter()+
  facet_wrap(~symptom, ncol=5) +
  #xlim(0,6) +
  ylim(0,6) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

#### correlation

corr <- read.csv(file = "correlation.csv")

severity <- subset(corr,select=c(
                                #medical
                                Seizures_severity,
                                 Toileting_issues_severity, 
                                 Sleep_issues_severity, 
                                 Infections_severity, 
                                 Fever_severity, 
                                 gross_motor_control_severity, 
                                 fine_motor_control_severity,
                                    #cognitive
                                    speaking_severity,
                                    verbal_comprehension_severity,
                                    Learning_new_things_severity,
                                    Memory_severity,
                                    engagement_severity,
                                      #behaviour
                                      emotional_regulation_severity,
                                      hyperactivtiy_severity,
                                      Self_injury_severity,
                                      Aggression_severity,
                                      Staying_focused_severity))



impact <- subset(corr,select=c(
  #medical
  Seizures_impact,
  Toileting_issues_impact, 
  Sleep_issues_impact, 
  Infections_impact, 
  Fever_impact, 
  gross_motor_control_impact, 
  fine_motor_control_impact,
  #cognitive
  speaking_impact,
  verbal_comprehension_impact,
  Learning_new_things_impact,
  Memory_impact,
  engagement_impact,
  #behaviour
  emotional_regulation_impact,
  hyperactivity_impact,
  Self_injury_impact,
  Aggression_impact,
  Staying_focused_impact))



M <-cor(impact, use = "pairwise.complete.obs")
library(corrplot)
library(RColorBrewer)
corrplot(M, method = "color", outline = T, 
         #addgrid.col = "lightgray", order="FPC", 
         addrect = 4, rect.col = "black", rect.lwd = 1,
         cl.pos = "b", tl.col = "black", 
         tl.cex = 0.6, cl.cex = 0.6, 
         addCoef.col = "black", number.digits = 2, 
         number.cex = 0.5,
         #col = wes_palette("Zissou1", 10, type = "continuous"))
         col = colorRampPalette(c("#7179ba", "white", "#e35f6a"))(10)
)


plot <- read.csv(file = "regressions_final_edit.csv")

plot$symptom <- factor(plot$symptom, levels = c("seizures",
                                                "incontinence",
                                                "sleep issues",
                                                "gross motor control",
                                                "fine motor control",
                                                "hyperactivity",
                                                "self injury",
                                                "aggression",
                                                
                                                "staying focused",
                                                "emotional regulation",
                                                "memory",
                                                "verbal fluency (speaking)",
                                                "learning new things",
                                                "engagement",
                                                "verbal comprehension (understanding others)"
                                                                    ))

plot <- plot

plot <- plot %>% filter(model == "SWLS")

x <- ggplot(plot,
            aes(x=symptom,
                y=beta,
                colour = category,
                shape = type
                #alpha = type
            )) +
  
  geom_point(position=position_dodge(width=0.9), size = 3) +
  
  geom_errorbar(aes(ymin=beta-se, ymax=beta+se), 
                position = position_dodge(0.9),
                width=0.3,
                colour="darkgrey", alpha=0.9, size=0.8) +
  
  geom_hline(yintercept = 0, linetype = "dotted") +
  
  coord_flip() +
  
  theme_bw() + 
  xlab("Symptom") + 
  ylab("Standardised effect size") +
  theme(axis.title.y = element_text(size=11, face="bold"),
        axis.title.x = element_text(size=11, face="bold")) +
  scale_colour_manual(values = c("violetred4", "indianred1", "royalblue1")) +
  facet_wrap(vars(model), ncol =3) 


x

