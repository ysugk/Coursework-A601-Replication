rm(list = ls())

library(tidyverse)
library(lubridate)

iclink <- read_delim("build/input/ibes-crsp-linktable.txt", delim = "\t", col_types = "cccccn") %>%
  mutate(across(ends_with("date"), dmy)) %>%
  set_names(str_to_lower)

ccmlink <- read_rds("build/input/ccmxpf_lnkhist.rds") %>%
  filter(linkprim %in% c("P", "C"),
         linktype %in% c("LC", "LU")) %>%
  mutate(linkprim = factor(linkprim, levels = c("P", "C")),
         linktype = factor(linktype, levels = c("LC", "LU")))

fundq <- read_rds("build/input/comp-fundq.rds") %>%
  rename(epsop = opepsq,
         gaap = epspxq)

statsumu <- read_rds("build/input/ibes-statsumu-epsus.rds") %>%
  select(cusip, statpers, meanest, fpedats)

actu <- read_rds("build/input/ibes-actu-epsus.rds") %>%
  select(cusip, pends, anndats, actdats, street = value) %>%
  mutate(street = round(street, 2)) %>%
  semi_join(statsumu, by = c("cusip", "pends" = "fpedats"))

data <- fundq %>%
  left_join(ccmlink, by = "gvkey") %>%
  filter(datadate >= linkdt & (is.na(linkenddt) | datadate <= linkenddt)) %>%
  arrange(gvkey, datadate, linkprim, linktype) %>%
  distinct(gvkey, datadate, .keep_all = TRUE) %>%
  mutate(permno = as.character(lpermno)) %>%
  left_join(iclink, by = "permno") %>%
  filter(datadate >= sdate & (is.na(edate) | datadate <= edate)) %>%
  arrange(gvkey, datadate, score) %>%
  distinct(gvkey, datadate, .keep_all = TRUE) %>%
  inner_join(actu, by = c("ncusip" = "cusip", "datadate" = "pends")) %>%
  arrange(gvkey, fyearq, fqtr) %>%
  distinct(gvkey, fyearq, fqtr, .keep_all = TRUE)

tidy_statsmu <- statsumu %>%
  inner_join(select(data, ncusip, datadate, rdq), by = c("cusip" = "ncusip", "fpedats" = "datadate")) %>%
  filter(statpers < rdq) %>%
  arrange(cusip, fpedats, desc(statpers)) %>%
  distinct(cusip, fpedats, .keep_all = TRUE)

lead1_data <- data %>%
  select(gvkey, fyearq, fqtr, prccq) %>%
  mutate(fyearq = if_else(fqtr == 1, fyearq - 1, fyearq),
         fqtr = if_else(fqtr == 1, 4, fqtr - 1)) %>%
  rename(lead1_prccq = prccq)

lag1_data <- data %>%
  select(gvkey, fyearq, fqtr, prccq) %>%
  mutate(fyearq = if_else(fqtr == 4, fyearq + 1 ,fyearq),
         fqtr = if_else(fqtr == 4, 1, fqtr + 1)) %>%
  rename(lag1_prccq = prccq)

lag4_data <- data %>%
  select(gvkey, fyearq, fqtr, epsop, gaap, street) %>%
  mutate(fyearq = fyearq + 1) %>%
  rename(lag4_epsop = epsop,
         lag4_gaap = gaap,
         lag4_street = street)

data2 <- data %>%
  left_join(lead1_data, by = c("gvkey", "fyearq", "fqtr")) %>%
  left_join(lag1_data, by = c("gvkey", "fyearq", "fqtr")) %>%
  left_join(lag4_data, by = c("gvkey", "fyearq", "fqtr")) %>%
  left_join(tidy_statsmu, by = c("ncusip" = "cusip", "datadate" = "fpedats", "rdq")) %>%
  drop_na(street, lag4_street, epsop, lag4_epsop) %>%
  filter(score == 1) %>%
  filter(linkprim == "P") %>%
  filter(fyearq >= 1989, fyearq <= 1997)

write_rds(data2, "build/output/fundq-ibes.rds", compress = "gz")
  
