install.packages("gtsummary")
library(gtsummary)

desc <- data_xp |>
  group_by(Condition) |>
  summarise(
    n = n(),
    across(all_of(vars),
           list(mean   = \(x) mean(x, na.rm = TRUE),
                sd     = \(x) sd(x, na.rm = TRUE),
                median = \(x) median(x, na.rm = TRUE)))
  )
desc

data_xp <- data_xp |>
    mutate(cond_group = case_when(
        Condition <= 3 ~ 1,
        Condition <= 6 ~ 2,
        Condition <= 9 ~ 3,
    ))

desc2 <- data_xp |>
  group_by(cond_group) |>
  summarise(
    n = n(),
    across(all_of(section1),
           list(mean   = \(x) mean(x, na.rm = TRUE),
                sd     = \(x) sd(x, na.rm = TRUE),
                median = \(x) median(x, na.rm = TRUE)))
  )
desc2

tbl_summary(data_xp, by = Condition, include = all_of(vars), statistic = all_continuous() ~ "{mean} ({sd})")

tbl_summary(data_xp, by = cond_group, include = all_of(section1), statistic = all_continuous() ~ "{mean} ({sd})")