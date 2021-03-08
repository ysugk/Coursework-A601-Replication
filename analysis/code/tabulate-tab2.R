rm(list = ls())

library(tidyverse)
library(stargazer)
library(nonnest2)
source("analysis/function/truncate.R")

data <- read_rds("analysis/input/fundq-ibes.rds")

data2 <- data %>%
  filter(epsop != street) %>%
  drop_na(lead1_prccq, ceqq, cshprq, street, gaap, epsop, epspiq) %>%
  mutate(group = case_when((epsop == gaap) & (gaap != street) ~ "Group 1",
                           (epsop != gaap) & (gaap != street) ~ "Group 2",
                           (street == gaap) & (gaap != epsop) ~ "Group 3",
                           TRUE ~ NA_character_)) %>%
  mutate(bv = ceqq / cshprq,
         ni_street = epspiq - street,
         ni_epsop = epspiq - epsop) %>%
  mutate(across(c("bv", "street", "epsop", "ni_street", "ni_epsop"), ~truncate(.x, 0.025, 0.975))) %>%
  drop_na(lead1_prccq, bv, street, epsop, ni_street, ni_epsop)

gen_summary <- function(data){
  model_street <- lm(lead1_prccq ~ bv + street + I(epspiq - street), data)
  model_epsop <- lm(lead1_prccq ~ bv + epsop + I(epspiq - epsop), data)
  
  b1 <- model_street$coefficients[-1]
  se <- summary(model_street)$coefficients["street", "Std. Error"]
  b2 <- model_epsop$coefficients[-1]
  tstat <- round((b1[2] - b2[2]) / se, 2)
  
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
    ~"rownames", ~"N", ~"BV", ~"OPINC", ~"NI-OPINC", ~"ADJ R2",
    "STREET", nrow(data), b1[1], b1[2], b1[3], rsq1,
    "EPSOP", nrow(data), b2[1], b2[2], b2[3], rsq2,
    "t test", NA, NA, tstat, NA, NA,
    "Vuong Z", NA, NA, NA, NA, vuong_stat
  )
}

# Group 1 ------
df_group1 <- data2 %>%
  filter(group == "Group 1")

gen_summary(df_group1) %>%
  as.data.frame() %>%
  stargazer(summary = FALSE, type = "text", out = "analysis/output/tabs/tab2a.tex",
            digits = 2, float = FALSE, rownames = FALSE)

# Group 2 ------
df_group2 <- data2 %>%
  filter(group == "Group 2")

gen_summary(df_group2) %>%
  as.data.frame() %>%
  stargazer(summary = FALSE, type = "text", out = "analysis/output/tabs/tab2b.tex",
            digits = 2, float = FALSE, rownames = FALSE)

# Group 3 ------
df_group3 <- data2 %>%
  filter(group == "Group 3")

gen_summary(df_group3) %>%
  as.data.frame() %>%
  stargazer(summary = FALSE, type = "text", out = "analysis/output/tabs/tab2c.tex",
            digits = 2, float = FALSE, rownames = FALSE)

# All ------

gen_summary(data2) %>%
  as.data.frame() %>%
  stargazer(summary = FALSE, type = "text", out = "analysis/output/tabs/tab2d.tex",
            digits = 2, float = FALSE, rownames = FALSE)

