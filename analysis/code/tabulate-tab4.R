rm(list = ls())

library(tidyverse)
library(nonnest2)
library(stargazer)
source("analysis/function/truncate.R")

data <- read_rds("analysis/input/fundq-ibes.rds")
daily_return <- read_rds("analysis/input/crsp-daily-return.rds")

car <- daily_return %>%
  filter(datefromevent <= 61) %>%
  mutate(ar = ret - vwretd) %>%
  group_by(permno, rdq) %>%
  summarise(car = sum(ar)) %>%
  drop_na(car)

data2 <- data %>%
  filter(epsop != street) %>%
  mutate(group = case_when((epsop == gaap) & (gaap != street) ~ "Group 1",
                           (epsop != gaap) & (gaap != street) ~ "Group 2",
                           (street == gaap) & (gaap != epsop) ~ "Group 3",
                           TRUE ~ NA_character_)) %>%
  inner_join(car, by = c("permno", "rdq")) %>%
  mutate(uopinc_street = (street - meanest)/lag1_prccq,
         uopinc_epsop = (epsop - meanest)/lag1_prccq) %>%
  mutate(across(c(starts_with("uopinc")), ~truncate(.x, .025, .975))) %>%
  drop_na(uopinc_street, uopinc_epsop)

gen_summary <- function(data){
  model_street <- lm(car ~ uopinc_street, data)
  model_epsop <- lm(car ~ uopinc_epsop, data)
  
  b1 <- model_street$coefficients[-1]
  se <- summary(model_street)$coefficients["uopinc_street", "Std. Error"]
  b2 <- model_epsop$coefficients[-1]
  tstat <- round((b1[1] - b2[1]) / se, 2)
  
  tstat <- if (tstat >= 2.65) {
    paste0(tstat, "*")
  } else if (tstat >= 2.00) {
    paste0(tstat, "**")
  } else if (tstat >= 1.65) {
    paste0(tstat, "***")
  } else {
    tstat %>%
      as.character()
  }
  
  vuong <- vuongtest(model_street, model_epsop)
  vuong_stat <- vuong$LRTstat
  vuong_p <- vuong$p_LRT$A
  
  vuong_stat <- if (vuong_p <= .01) {
    paste0(round(vuong_stat, 2), "*")
  } else if (vuong_p <= .05) {
    paste0(round(vuong_stat, 2), "**")
  } else if (vuong_p <= .1) {
    paste0(round(vuong_stat, 2) , "***")
  } else {
    round(vuong_stat, 2) %>%
      as.character()
  }
  
  b1 <- round(b1, 2) %>%
    as.character()
  
  b2 <- round(b2, 2) %>%
    as.character()
  
  rsq1 <- round(100*summary(model_street)$adj.r.squared, 2) %>%
    as.character()
  
  rsq2 <- round(100*summary(model_epsop)$adj.r.squared, 2) %>%
    as.character()
  
  tribble(
    ~"rownames", ~"N", ~"ERC", ~"ADJ R2",
    "STREET", nrow(data), b1, rsq1,
    "EPSOP", nrow(data), b2, rsq2,
    "t test", NA, tstat, NA,
    "Vuong Z", NA, NA, vuong_stat
  )
}


# Group 1 ------
df_group1 <- data2 %>%
  filter(group == "Group 1")

gen_summary(df_group1) %>%
  as.data.frame() %>%
  stargazer(summary = FALSE, type = "text", out = "analysis/output/tabs/tab4a.tex",
            digits = 2, float = FALSE, rownames = FALSE)

# Group 2 ------
df_group2 <- data2 %>%
  filter(group == "Group 2")

gen_summary(df_group2) %>%
  as.data.frame() %>%
  stargazer(summary = FALSE, type = "text", out = "analysis/output/tabs/tab4b.tex",
            digits = 2, float = FALSE, rownames = FALSE)

# Group 3 ------
df_group3 <- data2 %>%
  filter(group == "Group 3")

gen_summary(df_group3) %>%
  as.data.frame() %>%
  stargazer(summary = FALSE, type = "text", out = "analysis/output/tabs/tab4c.tex",
            digits = 2, float = FALSE, rownames = FALSE)

# All
gen_summary(data2) %>%
  as.data.frame() %>%
  stargazer(summary = FALSE, type = "text", out = "analysis/output/tabs/tab4d.tex",
            digits = 2, float = FALSE, rownames = FALSE)

