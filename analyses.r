continuous <- c("Obligated", "Expect", "Permission", "Stakes", 
    "Promise_Con", "Answerability", "Wrong", "Blame", "Demand_Con")
binary <- c("Promise", "Demands")
initial_continuous <- c("Obligated", "Expect", "Permission", "Stakes", 
    "Promise_Con")
follow_continuous <- c("Answerability", "Wrong", "Blame", "Demand_Con")

#Start with an overall ANOVA test, no breakdown between conditions.
for (c in continuous) {
   fit <- aov(as.formula(paste0(c, " ~  factor(Condition)")), data = data_xp)
   cat(c,":\n")
   print(summary(fit))
}

#This tests our initial scenarios, so 1, 2, and 3.
for (c in initial_continuous) {
    fit <- aov(as.formula(paste0(c, " ~  factor(cond_group)")), data = data_xp)
   cat(c,":\n")
   print(summary(fit))
   print(TukeyHSD(fit))
}

#This is our test for within the follow up, so 1A, 2A, 3A
followup_sets <- list(A = c(1,4,7), B = c(2,5,8), C = c(3,6,9))

for (c in follow_continuous) {
  for (f in names(followup_sets)) {
    sub <- filter(data_xp, Condition %in% followup_sets[[f]])
    fit <- aov(as.formula(paste0(c, " ~ factor(Condition)")), data = sub)
    cat(c, "- follow-up", f, ":\n")
    print(summary(fit))
    print(TukeyHSD(fit))
  }
}

#This is our test for the within initial, so 1A, 1B, 1C
for (c in follow_continuous) {
  for (g in 1:3) {
    sub <- filter(data_xp, cond_group == g)
    fit <- aov(as.formula(paste0(c, " ~ factor(Condition)")), data = sub)
    cat(c, "- initial", g, ":\n")
    print(summary(fit))
    print(TukeyHSD(fit))
  }
}

#Now these are for the binary choice sections
#All 9 conditions
for (b in binary) {
  tab <- table(data_xp$Condition, data_xp[[b]])
  cat(b, ":\n")
  print(tab)
  print(chisq.test(tab))
}

#1 vs 2 vs 3
  tab_1 <- table(data_xp$cond_group, data_xp$Promise)
  print(tab_1)
  print(chisq.test(tab_1))
  print(pairwise.prop.test(tab_1))

#1A vs 2A vs 3A
  for (f in names(followup_sets)) {
    sub <- filter(data_xp, Condition %in% followup_sets[[f]])
    tab <- table(sub$Condition, sub$Demands)
    cat("Demands - follow-up", f, ":\n")
    print(tab)
    print(chisq.test(tab))
    print(pairwise.prop.test(tab))
  }

#1A vs 1B vs 1C
  for (g in 1:3) {
    sub <- filter(data_xp, cond_group == g)
    tab <- table(sub$Condition, sub$Demands)
    cat( "Demands- initial", g, ":\n")
    print(tab)
    print(chisq.test(tab))
    print(pairwise.prop.test(tab))
  }
