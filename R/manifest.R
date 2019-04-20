options("scipen"=100, "digits"=6)
library(tidyverse)
library(rlang)
library(kableExtra)
library(knitr)
library(pander)
library(magrittr)
library(doParallel)
library(mstate)
library(flexsurv)
library(eha)
library(heemod)
library(here)
library(conflicted)
library(cluster)
conflict_prefer("filter", "dplyr")
conflict_prefer("set_names", "rlang")
conflict_prefer("lag", "dplyr")

source(here::here("R/prepare-multistate-data.R"))
source(here::here("R/fit-multistate-model.R"))
source(here::here("R/get-cumulative-hazard.R"))
source(here::here("R/add-binary-indicators.R"))

insurance_sipp_lut <- c(
  "cat_1" = "01_esi",
  # "cat_2" = "02_esi_dep",
  "cat_2" = "02_priv_oth",
  "cat_3" = "03_public",
  "cat_4" = "04_uninsured"
)
