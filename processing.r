install.packages("tidyverse")
library(tidyverse)

data_OG <- read.csv("")

data_xp <- data.frame(data_OG)

rm(data_OG)

data_xp <- data_xp[-c(1,2),]

#anonymizing data
id_col <- "Pro_ID"

ids <- unique(na.omit(data_xp[[id_col]]))
width <- max(3, nchar(length(ids)))
codes <- sprintf(paste0("P%0", width, "d"), sample(length(ids)))

data_xp <- data_xp |>
    mutate(participant = codes[match(.data[[id_col]], ids)], .before = 1)

drop_cols <- c(id_col, "IPAddress", "LocationLatitude", "LocationLongitude",
               "RecipientEmail", "RecipientFirstName", "RecipientLastName",
               "ExternalReference")
data_xp <- data_xp |> select(-any_of(drop_cols))

#Making measures easy to work with
vars <- c("Obligated", "Expect", "Permission", "Stakes", "Promise", 
    "Promise_Con", "Answerability", "Wrong", "Blame", "Demands", "Demand_Con")
pattern <- paste0("^(", paste(vars, collapse = "|"), ")_(\\d+)_1$")
matched <- str_subset(names(data_xp), pattern)
length(matched)

section1 <- c("Obligated", "Expect", "Permission", "Stakes", "Promise", 
    "Promise_Con")

data_xp <- data_xp |>
    mutate(across(all_of(matched), as.numeric))

cols_for <- function(v) str_subset(names(data_xp), paste0("^", v, "_\\d+_1$"))

for (v in vars) {
  data_xp[[v]] <- reduce(data_xp[cols_for(v)], coalesce)
}

data_xp <- data_xp |>
    select(-all_of(matched))

#Need to remove or change for test data
data_xp_filtered_IC <- data_xp %>% filter(CONSENT == 2)
data_xp <- setdiff(data_xp, data_xp_filtered_IC)

#Need to remove or change for test data
data_xp_filtered_Bot_Attention <- data_xp %>% filter(BOT_TEST_x != 3 | Attention_x != 4)
data_xp <- setdiff(data_xp, data_xp_filtered_Bot_Attention)

data_xp$Gender_z <- factor(data_xp$Gender_z,
             levels = c(0, 1, 2, 3, 4),
             labels = c("No answer",
                     "Man",
                     "Woman",
                     "Nonbinary",
                     "None"))
data_xp$RACE_z <- factor(data_xp$RACE_z,
             levels = c(0, 1, 2, 3, 4, 5, 6),
             labels = c("No answer",
                     "White",
                     "Hispanic",
                     "Black",
                     "Asian",
                     "Mixed",
                     "Native American"
             ))