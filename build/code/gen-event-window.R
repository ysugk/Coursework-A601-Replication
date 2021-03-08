rm(list = ls())
library(tidyverse)

dsi <- read_rds("build/input/crsp-dsi.rds")
fundq_ibes <- read_rds("build/output/fundq-ibes.rds")

rdq_crwk <- fundq_ibes %>%
  filter(epsop != street) %>%
  drop_na(rdq, meanest) %>%
  select(datadate, fyearq, fqtr, rdq, permno, gvkey) %>%
  semi_join(dsi, by = c("rdq" = "date"))

construct_window <- function(firmcode, anndate, window = c(-1, 61)){
  # global: dsf, dsi
  
  pre <- window[1]
  post <- window[2]
  
  pre_dsi <- dsi %>%
    filter(date < anndate) %>%
    arrange(desc(date)) %>%
    mutate(datefromevent = -(1:n())) %>%
    filter(datefromevent >= pre)
  
  post_dsi <- dsi %>%
    filter(date >= anndate) %>%
    arrange(date) %>%
    mutate(datefromevent = (1:n()-1)) %>%
    filter(datefromevent <= post)
  
  bind_rows(pre_dsi, post_dsi) %>%
    mutate(permno = firmcode,
           rdq = anndate)
}

event_window <- map2_dfr(rdq_crwk$permno, rdq_crwk$rdq, construct_window)

write_rds(event_window, "build/temp/event-window.rds", compress = "gz")
