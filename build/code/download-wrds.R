rm(list = ls())

library(RPostgres)
library(tidyverse)
library(labelled)

# set-up ------
wrds <- dbConnect(Postgres(),
                  host = 'wrds-pgdata.wharton.upenn.edu',
                  port = 9737,
                  dbname = 'wrds',
                  sslmode = 'require',
                  user = 'ysugk')

# look into schema ------
res <- dbSendQuery(wrds, "select distinct table_schema
                   from information_schema.tables
                   where table_type ='VIEW'
                   or table_type ='FOREIGN TABLE'
                   order by table_schema")

table_schema <- dbFetch(res, n = -1)
dbClearResult(res)

# download ibes ------
res <- dbSendQuery(wrds, "select distinct table_name
                   from information_schema.columns
                   where table_schema='ibes'
                   order by table_name")
ibes_table <- dbFetch(res, n = -1)
dbClearResult(res)

## download ibes.statsumu_epsus (Stat Summary Unadjusted US EPS) ------
res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='ibes'
                   and table_name='statsumu_epsus'
                   order by ordinal_position")
statsumu <- dbFetch(res, n = -1)
dbClearResult(res)

queue <- paste("SELECT *",
               "FROM ibes.statsumu_epsus",
               "WHERE fiscalp = 'QTR' AND fpedats >= '1988-01-01' AND fpedats <= '1999-12-31' AND fpi = '6'",
               sep = " ")

res <- dbSendQuery(wrds, queue)
data <- dbFetch(res, n = -1)
dbClearResult(res)

label <- tibble::tribble(
  ~name, ~type,                                              ~desc, ~help,
                      "CNAME",     "CHAR",                                                    "Company Name",    NA,
                    "CURCODE",     "CHAR",                                                   "Currency Code",    NA,
                      "CUSIP",     "CHAR",                                                     "CUSIP/SEDOL",    NA,
                    "ESTFLAG",     "CHAR",                                                   "Estimate Flag",    NA,
                    "FISCALP",     "CHAR",                                                     "Periodicity",    NA,
                    "FPEDATS",      "NUM",                                        "Forecast Period End Date",    NA,
                    "HIGHEST",      "NUM",                                                   "High Estimate",    NA,
                     "LOWEST",      "NUM",                                                    "Low Estimate",    NA,
                    "MEANEST",      "NUM",                                                   "Mean Estimate",    NA,
                     "MEDEST",      "NUM",                                                 "Median Estimate",    NA,
                    "NUMDOWN",      "NUM",                                                     "Number Down",    NA,
                     "NUMEST",      "NUM",                                             "Number of Estimates",    NA,
                      "NUMUP",      "NUM",                                                       "Number Up",    NA,
                      "OFTIC",     "CHAR",                                                 "Official Ticker",    NA,
                   "STATPERS",      "NUM",                                         "IBES Statistical Period",    NA,
                      "STDEV",      "NUM",                                              "Standard Deviation",    NA,
                     "TICKER",     "CHAR",                                                  "I/B/E/S Ticker",    NA,
                     "USFIRM",      "NUM", "U.S. Firm (USFIRM=0 if from .INT and USFIRM=1 if from .US file)",    NA
               ) %>%
  mutate(name_lower = str_to_lower(name))

var_label(data) <- split(label$desc, label$name_lower)

write_rds(data, "build/input/ibes-statsumu-epsus.rds", compress = "gz")

## download ibes.actu_epsus (Actual Unadjusted US EPS) ------
res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='ibes'
                   and table_name='actu_epsus'
                   order by ordinal_position")
actu_epsus <- dbFetch(res, n = -1)
dbClearResult(res)

queue <- paste("SELECT *",
               "FROM ibes.actu_epsus",
               "WHERE pdicity = 'QTR' AND pends >= '1988-01-01' AND pends <= '1999-12-31'",
               sep = " ")

res <- dbSendQuery(wrds, queue)
data <- dbFetch(res, n = -1)
dbClearResult(res)

label <-  tibble::tribble(
  ~name, ~type,                                              ~desc, ~help,
                    "ACTDATS",     "DATE",                                     "Activation Date, SAS Format",    NA,
                    "ACTTIMS",      "NUM",                                     "Activation Time, SAS Format",    NA,
                    "ANNDATS",     "DATE",                                       "Announce Date, SAS Format",    NA,
                    "ANNTIMS",      "NUM",                                       "Announce Time, SAS Format",    NA,
                      "CNAME",     "CHAR",                                                    "Company Name",    NA,
                   "CURR_ACT",     "CHAR",                                                        "Currency",    NA,
                      "CUSIP",     "CHAR",                                                     "CUSIP/SEDOL",    NA,
                      "OFTIC",     "CHAR",                                                 "Official Ticker",    NA,
                      "PENDS",     "DATE",                                     "Period End Date, SAS Format",    NA,
                     "TICKER",     "CHAR",                                                  "I/B/E/S Ticker",    NA,
                     "USFIRM",      "NUM", "U.S. Firm (USFIRM=0 if from .INT and USFIRM=1 if from .US file)",    NA,
                      "value",      "NUM",                                                           "Value",    NA
               ) %>%
  mutate(name_lower = str_to_lower(name))

var_label(data) <- split(label$desc, label$name_lower)

write_rds(data, "build/input/ibes-actu-epsus.rds", compress = "gz")

# download comp -----
res <- dbSendQuery(wrds, "select distinct table_name
                   from information_schema.columns
                   where table_schema='comp'
                   order by table_name")
comp_table <- dbFetch(res, n = -1)
dbClearResult(res)

## download comp.fundq -----
res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='comp'
                   and table_name='fundq'
                   order by ordinal_position")
fundq <- dbFetch(res, n = -1)
dbClearResult(res)

select_condition <- c("gvkey", "datadate", "indfmt", "datafmt", "popsrc", "consol", "curcdq", "costat",
                      "fyearq", "fqtr", "fyr", "tic", "cusip", "rdq",
                      "OPEPSQ", "EPSPXQ", "PRCCQ", "CEQQ", "CSHPRQ", "EPSPIQ") %>%
  paste0(collapse = ", ")

where_condition <- c("(fyearq between 1986 and 2000)", "AND",
                     "indfmt = 'INDL'", "AND",
                     "datafmt = 'STD'", "AND",
                     "popsrc = 'D'", "AND",
                     "consol = 'C'") %>%
  paste0(collapse = " ")

from_condition <- "comp.fundq"

queue <- paste("SELECT", select_condition, "FROM", from_condition, "WHERE", where_condition, sep = " ")

res <- dbSendQuery(wrds, queue)
data <- dbFetch(res, n = -1)
dbClearResult(res)

write_rds(data, "build/input/comp-fundq.rds", compress = "gz")

# download crsp -----
res <- dbSendQuery(wrds, "select distinct table_name
                   from information_schema.columns
                   where table_schema='crsp'
                   order by table_name")
crsp_table <- dbFetch(res, n = -1)
dbClearResult(res)

## download crsp.dsf (daily stock return) ------
res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='crsp'
                   and table_name='dsf'
                   order by ordinal_position")
dsf <- dbFetch(res, n = -1)
dbClearResult(res)

res <- dbSendQuery(wrds, "SELECT date, cusip, permno, ret, retx
                   FROM crsp.dsf
                   WHERE date >= '1988-01-01' AND date <= '1998-12-31'")
data <- dbFetch(res, n = -1)
dbClearResult(res)

write_rds(data, "build/input/crsp-dsf.rds", compress = "gz")

## download crsp.dsi (daily stock index) ------
res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='crsp'
                   and table_name='dsi'
                   order by ordinal_position")
dsi <- dbFetch(res, n = -1)
dbClearResult(res)

res <- dbSendQuery(wrds, "SELECT *
                   FROM crsp.dsi
                   WHERE date >= '1988-01-01' AND date <= '1998-12-31'")
data <- dbFetch(res, n = -1)
dbClearResult(res)

write_rds(data, "build/input/crsp-dsi.rds", compress = "gz")

## download crsp.ccmxpf_lnkhist(Compustat-CRSP link table) ------
res <- dbSendQuery(wrds, "SELECT *
                   FROM crsp.ccmxpf_lnkhist")
data <- dbFetch(res, n = -1)
dbClearResult(res)

write_rds(data, "build/input/ccmxpf_lnkhist.rds", compress = "gz")
