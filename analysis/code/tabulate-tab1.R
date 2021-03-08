rm(list = ls())

library(tidyverse)
library(stargazer)
library(ystools)
source("analysis/function/truncate.R")

data <- read_rds("analysis/input/fundq-ibes.rds")

data2 <- data %>%
  filter(lag4_epsop != lag4_street) %>%
  mutate(group = case_when((lag4_epsop == lag4_gaap) & (lag4_gaap != lag4_street) ~ "Group 1",
                           (lag4_epsop != lag4_gaap) & (lag4_gaap != lag4_street) ~ "Group 2",
                           (lag4_street == lag4_gaap) & (lag4_gaap != lag4_epsop) ~ "Group 3",
                           TRUE ~ NA_character_)) %>%
  mutate(street_error = abs(street - lag4_street)/lag1_prccq,
         epsop_error = abs(epsop - lag4_epsop)/lag1_prccq) %>%
  mutate(param =  epsop_error - street_error,
         nonparam = 1*(epsop_error > street_error)) %>%
  mutate(across(c("param"), ~truncate(.x, .025, .975))) %>%
  drop_na(param, nonparam)

# Tabulate Panel A ------
panelA_all <- data2 %>%
  filter(epsop == street) %>%
  mutate(group = "All") %>%
  group_by(group) %>%
  summarise(N = n(),
            param_stat = t.test(param)$statistic,
            param_pvalue = t.test(param)$p.value,
            nonparam_stat = mean(nonparam),
            nonparam_pvalue = t.test(nonparam - 0.5)$p.value)

panelA_subsample <- data2 %>%
  filter(epsop == street) %>%
  group_by(group) %>% 
  summarise(N = n(),
            param_stat = t.test(param)$statistic,
            param_pvalue = t.test(param)$p.value,
            nonparam_stat = mean(nonparam),
            nonparam_pvalue = t.test(nonparam - 0.5)$p.value)

panelA <- bind_rows(panelA_subsample, panelA_all) %>%
  mutate(across(ends_with("stat"), ~round(.x, 2))) %>%
  mutate(param_stat = case_when(param_pvalue <= .01 ~ paste0(param_stat, "*"),
                                param_pvalue <= .05 ~ paste0(param_stat, "**"),
                                param_pvalue <= .1 ~ paste0(param_stat, "**"),
                                TRUE ~ as.character(param_stat)),
         nonparam_stat = case_when(nonparam_pvalue <= .01 ~ paste0(nonparam_stat, "*"),
                                   nonparam_pvalue <= .05 ~ paste0(nonparam_stat, "**"),
                                   nonparam_pvalue <= .1 ~ paste0(nonparam_stat, "**"),
                                   TRUE ~ as.character(nonparam_stat))) %>%
  select(-ends_with("pvalue")) %>%
  as.data.frame()

stargazer(panelA, summary = FALSE, type = "text", out = "analysis/output/tabs/tab1a.tex",
          digits = 2, float = FALSE, rownames = FALSE)

# Tabulate Panel B ------
panelB_all <- data2 %>%
  filter(epsop != street) %>%
  mutate(group = "All") %>%
  group_by(group) %>%
  summarise(N = n(),
            param_stat = t.test(param)$statistic,
            param_pvalue = t.test(param)$p.value,
            nonparam_stat = mean(nonparam),
            nonparam_pvalue = t.test(nonparam - 0.5)$p.value)

panelB_subsample <- data2 %>%
  filter(epsop != street) %>%
  group_by(group) %>% 
  summarise(N = n(),
            param_stat = t.test(param)$statistic,
            param_pvalue = t.test(param)$p.value,
            nonparam_stat = mean(nonparam),
            nonparam_pvalue = t.test(nonparam - 0.5)$p.value)

panelB <- bind_rows(panelB_subsample, panelB_all) %>%
  mutate(across(ends_with("stat"), ~round(.x, 2))) %>%
  mutate(param_stat = case_when(param_pvalue <= .01 ~ paste0(param_stat, "*"),
                                param_pvalue <= .05 ~ paste0(param_stat, "**"),
                                param_pvalue <= .1 ~ paste0(param_stat, "**"),
                                TRUE ~ as.character(param_stat)),
         nonparam_stat = case_when(nonparam_pvalue <= .01 ~ paste0(nonparam_stat, "*"),
                                   nonparam_pvalue <= .05 ~ paste0(nonparam_stat, "**"),
                                   nonparam_pvalue <= .1 ~ paste0(nonparam_stat, "**"),
                                   TRUE ~ as.character(nonparam_stat))) %>%
  select(-ends_with("pvalue")) %>%
  as.data.frame()

stargazer(panelB, summary = FALSE, type = "text", out = "analysis/output/tabs/tab1b.tex",
          digits = 2, float = FALSE, rownames = FALSE)

