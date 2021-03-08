rm(list = ls())

library(tidyverse)

dsf <- read_rds("build/input/crsp-dsf.rds")
event_window <- read_rds("build/temp/event-window.rds")

daily_return <- dsf %>%
  mutate(permno = as.character(permno)) %>%
  inner_join(event_window, by = c("date", "permno"))

write_rds(daily_return, "build/output/crsp-daily-return.rds", compress = "gz")
