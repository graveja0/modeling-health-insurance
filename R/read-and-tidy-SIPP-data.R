state_xw <- read.csv(here::here("input/state_xwalk.csv"))
df_sipp_full <-
  #data.table::fread("./data/sipp/sip_waves_one_two.csv") %>%
  data.table::fread(here::here("data/sipp/sip_waves_one_two (1).csv")) %>% 
  janitor::clean_names() %>% tbl_df()  %>% 
  rename(fips = tehc_st) %>% 
  inner_join(state_xw,"fips") %>% 
  mutate(expansion_state = 1 - as.integer(
    # Note these need to be nonexpansion states as of *2015*
    state %in% c(
      "TN",
      "WY",
      "SD",
      "WI",
      "TX",
      "KS",
      "OK",
      "MO",
      "MS",
      "AL",
      "GA",
      "FL",
      "SC",
      "NC",
      "ID",
      "UT",
      "VA",
      "LA",
      "AK",
      "ME","NE","WY"
    )
  )) %>% 
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
  mutate(hicov = 
           ifelse((ephi_t == 1 | milmth==1) & own_cov==1,1,
                  ifelse((ephi_t ==1 | milmth==1) & own_cov==0,1,
                         ifelse(privhimth==1,2,
                                ifelse(medcaidmth==1 | medcarmth==1,3,
                                       ifelse(rhimth==2,4,-9)))))) %>% 
  mutate(hicov3 = 
           ifelse(hicov %in% 1:2,1, 
                  ifelse(hicov==3, 2, 
                         ifelse(hicov==4,3, NA)))) %>% 
  group_by(ssuid,pnum,monthcode) %>% 
  mutate(year_in_survey = row_number())  %>% 
  mutate(id = paste0(ssuid,"-",pnum)) %>% 
  mutate(sex = as.factor(esex),
         race = as.factor(erace),
         state = as.factor(state),
         age = tage) %>%
  ungroup()

write_rds(df_sipp_full, path = here("input/sipp/01_sipp-tidy_v1-0.rds"))

set.seed(123) 
sample_ids <- df_sipp_full %>% 
  filter(age>18 & age<63) %>% 
  pull(id) %>% 
  unique() %>% 
  sample(1000)

df_sipp_full_sample <- 
  df_sipp_full %>% filter(id %in% sample_ids)

write_rds(df_sipp_full_sample, path = here("input/sipp/01_sample_sipp-tidy_v1-0.rds"))
