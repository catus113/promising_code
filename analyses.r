continuous <- c("Obligated", "Expect", "Permission", "Stakes", 
    "Promise_Con", "Answerability", "Wrong", "Blame", "Demand_Con")
binary <- c("Promise", "Demands")
initial_continuous <- c("Obligated", "Expect", "Permission", "Stakes", 
    "Promise_Con")
follow_continuous <- c("Answerability", "Wrong", "Blame", "Demand_Con")

#Start with an overall ANOVA test, no breakdown between conditions.
for (c in continuous) {
   fit <- aov(as.formula(paste0(c, " ~  factor(Condition)")), data = data_xp)
   cat(c,":","\n")
   print(summary(fit))
}

for (c in initial_continuous) {
    fit <- aov(as.formula(paste0(c, " ~  factor(cond_group)")), data = data_xp)
   cat(c,":","\n")
   print(summary(fit))
   print(TukeyHSD(fit))
}