other_expansion_dates <- 
  c("AK" = lubridate::as_date(zoo::as.yearmon("09/2014","%m/%Y")),
    "IN" = lubridate::as_date(zoo::as.yearmon("02/2015","%m/%Y")),
    "MI" = lubridate::as_date(zoo::as.yearmon("04/2014","%m/%Y")),
    "MT" = lubridate::as_date(zoo::as.yearmon("01/2016","%m/%Y")),
    "NH" = lubridate::as_date(zoo::as.yearmon("08/2014","%m/%Y")),
    "PA" = lubridate::as_date(zoo::as.yearmon("01/2015","%m/%Y")),
    "CA" = lubridate::as_date(zoo::as.yearmon("01/2010","%m/%Y")),
    "CT" = lubridate::as_date(zoo::as.yearmon("01/2010","%m/%Y")),
    "HI" = lubridate::as_date(zoo::as.yearmon("01/1994","%m/%Y")),
    "MN" = lubridate::as_date(zoo::as.yearmon("01/2010","%m/%Y")),
    "WI" = lubridate::as_date(zoo::as.yearmon("01/2009","%m/%Y")),
    "DE" = lubridate::as_date(zoo::as.yearmon("01/1996","%m/%Y")),
    "DC" = lubridate::as_date(zoo::as.yearmon("01/2010","%m/%Y")),
    "MA" = lubridate::as_date(zoo::as.yearmon("01/2006","%m/%Y")),
    "NY" = lubridate::as_date(zoo::as.yearmon("01/2001","%m/%Y")),
    "VT" = lubridate::as_date(zoo::as.yearmon("01/1996","%m/%Y")),
    "LA" = lubridate::as_date(zoo::as.yearmon("01/2016","%m/%Y")))



state_xw <- read.csv(here::here("../../../box/sccs-r01/transitions-dd/input/state_xwalk.csv")) %>% 
  mutate(full_expansion_state = as.integer(
    state %in%   c("AK","AZ","AR","CO","IL","IN","KY",
                   "MD","MI","MT","NV","NH","NJ","NM",
                   "ND","OH","OR","PA","RI","WA","WV")
  ), 
  substantial_expansion_state = as.integer(
    state %in% c("CA","CT","HI","MN","WI") 
  ),
  mild_expansion_state = as.integer(
    state %in% c("DE","DC","MA","NY","VT")
  ),
  non_expansion_state = as.integer(
    state %in% c("AL","GA","FL","ID","KS","LA","ME",
                 "MS","MO","NE","NC","OK","SC","SD",
                 "TN","TX","UT","VA","WY")
  )) %>% 
  mutate(expansion_date = ifelse(non_expansion_state ==0 , 
                                 lubridate::as_date(zoo::as.yearmon("01/2014","%m/%Y")),
                                 NA)) %>% 
  mutate(expansion_date = ifelse(state %in% names(other_expansion_dates), 
                                 other_expansion_dates[as.character(state)],expansion_date)) %>% 
  mutate(expansion_date = lubridate::as_date(expansion_date)) %>% 
  mutate(expansion_year = lubridate::year(expansion_date))

df_sipp_full <-
  #data.table::fread("./data/sipp/sip_waves_one_two.csv") %>%
  #data.table::fread(here::here("data/sipp/sip_waves_one_two (1).csv")) %>% 
  data.table::fread("./data/sipp/sip_waves_one_two_three_v2.csv") %>% 
  janitor::clean_names() %>% tbl_df()  %>% 
  rename(fips = tehc_st) %>% 
  inner_join(state_xw,"fips") %>% 
  mutate(himth = as.integer(rhimth==1 | rcdmth ==1 | ecrmth ==1),
         privhimth = as.integer(rhimth == 1),
         medcarmth = as.integer(ecrmth == 1),
         medcaidmth = as.integer(rcdmth == 1), 
         milmth = as.integer(emlmth == 1) 
  ) %>% 
  #EHEMPLY: 1=Employer or job, 2=Former Employer, 3=Union or Association, 
  # 4=Bought it directly, 5=School, or 6=something else)
  mutate(ephi_t = as.integer(ehemply1 %in% c(1,2,3) | ehemply2 %in% c(1,2,3))) %>% 
  mutate(own_cov = as.integer(rhiowner==1),
         oth_cov = as.integer(rhiowner==2),
         both_cov = as.integer(rhiowner==3))  %>% #  plan, someone else's, both or neither. 
  mutate(ownboth = as.integer(own_cov==1 | both_cov==1)) %>% 
  mutate(re_ephi = as.integer(ephi_t==1 & ownboth==1)) %>% 
  mutate(year_in_survey = swave)  %>% 
  mutate(id = paste0(ssuid,"-",pnum)) %>% 
  mutate(sex = as.factor(esex),
         race = as.factor(erace),
         state = as.factor(state),
         age = tage) %>% 
  mutate(inc_pov = pmin(50,pmax(0,tftotinc / rfpov)))

#write_rds(df_sipp_full, path = here("input/sipp/01_sipp-tidy_v1-0.rds"))
write_rds(df_sipp_full, path = here("input/sipp/01_sipp-tidy_v2-0.rds"))


set.seed(123) 
sample_ids <- df_sipp_full %>% 
  filter(age>18 & age<63) %>% 
  pull(id) %>% 
  unique() %>% 
  sample(1000)

df_sipp_full_sample <- 
  df_sipp_full %>% filter(id %in% sample_ids)

#write_rds(df_sipp_full_sample, path = here("input/sipp/01_sample_sipp-tidy_v1-0.rds"))
write_rds(df_sipp_full_sample, path = here("input/sipp/01_sample_sipp-tidy_v2-0.rds"))
