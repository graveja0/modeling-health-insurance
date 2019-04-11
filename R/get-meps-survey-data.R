##############################
##############################
# Get Survey (MEPS) Data
# December 10, 2018
# John A. Graves
##############################
##############################

#devtools::install_github("ajdamico/lodown")
# install.packages( "srvyr" , repos = "http://cran.rstudio.com/" )
library(lodown)
library(srvyr)
library(tidyverse)

# Only needs to be run once.
# meps_cat <-
#   get_catalog( "meps" ,
#                output_dir = file.path( "./input/meps/" ))
# # 2014-2016 only
# meps_cat <- subset( meps_cat , year %in%  2014:2016 )
# # download the microdata to local computer
# meps_cat <- lodown( "meps" , meps_cat )

df_meps <-
  list.files("./input/meps", recursive = TRUE, "consol") %>%
  map(~ (
    read_rds(paste0("./input/meps/", .x)) %>%
      tbl_df() %>% 
      janitor::clean_names() %>%
      mutate(file = .x)  #%>% 
    # filter(row_number() %in% sample(1:nrow(.),100))
  ))

month_lut <- 
  c("ja" = 1,
    "fe" = 2,
    "ma" = 3,
    "ap" = 4,
    "my" = 5,
    "ju" = 6,
    "jl" = 7,
    "au" = 8,
    "se" = 9,
    "oc" = 10,
    "no" = 11,
    "de" = 12)

key_vars <- c("dupersid","panel","age31x","region31","sex","racethx")
key_vars2 <- gsub("31|31x","",key_vars)
df_meps_long <- 
  df_meps %>% 
  map(~(.x %>% 
          select(key_vars,matches("^tri|^mcr|^mcd|^pub|^ins|^opa|^opb|^sta|^peg|^pdk|^png|^pog|^prs|^pou|^prx|^pri|^hpe|^hpd|^hpn|^hpo|^hps|^hpx|^hpr")) %>% 
          mutate(region = region31,
                 age = age31x) %>% 
          select(-contains("inscop"),-contains("insc"),-matches("42$|53$|31$")) %>% 
          select(key_vars2,matches("ja[1-9]|fe[1-9]|ma[1-9]|ap[1-9]|my[1-9]|ju[1-9]|jl[1-9]|au[1-9]|se[1-9]|oc[1-9]|no[1-9]|de[1-9]"))  %>% 
          gather(key,value,-key_vars2) %>% 
          mutate(key = str_remove(key,"14|15|16")) %>% 
          mutate(month = ifelse(grepl("x$",key),str_sub(key,-3,-2),str_sub(key,-2,-1))) %>% 
          mutate(type = ifelse(grepl("x$",key), 
                               str_replace(key,"jax$|fex$|max$|apx$|myx$|jux$|jlx$|aux$|sex$|ocx$|nox$|dex$","x"),
                               str_replace(key,"ja$|fe$|ma$|ap$|my$|ju$|jl$|au$|se$|oc$|no$|de$",""))) %>% 
          filter(!type %in% c("mcd","mcr","pub","tri")) %>% 
          select(key_vars2,month,type,value) %>% 
          mutate(month = month_lut[month]) %>% 
          arrange(dupersid,month) %>% 
          mutate(value = ifelse(value==2,0,value)) %>% 
          spread(type,value) %>% 
          mutate(insurance = case_when(
            hpe==1 | hpo==1 | hpd==1 | hps==1 ~ 1,
            peg==1 | pog==1  | pou ==1 | pdk==1  ~2 ,
            png==1 | prx ==1 ~ 3, 
            mcdx==1 | mcrx==1 | pubx==1 | opa==1 | opb==1 | sta==1 ~ 4,
            insx == 0 ~5
          ))  %>% 
          mutate(insurance = factor(insurance,labels = c("ESI-Own","ESI-Dependent","Non-Group","Public","Uninsured"))) %>% 
          select(key_vars2,month,insurance) %>% 
          mutate(panel2 = as.integer(panel==min(panel)))  %>% 
          mutate(month = month + 12 * panel2) %>% 
          select(-panel2) #%>% 
        #mutate(month2 = paste0("insurance_",month)) 
  )) %>%
  bind_rows() %>% 
  arrange(dupersid,month) 

write_rds(df_meps_long,path="./input/meps/df-meps-long-2014-to-2016.rds")
df_meps_long %>% filter(dupersid =="60001101") %>% data.frame()
# gen hicov = .
# replace hicov = 5 if ins==2
# replace hicov= 4 if mcd==1 | mcr==1 | pub==1 | opa==1 | opb==1 | sta==1
# replace hicov = 3 if png==1 | prx ==1
# replace hicov = 2 if peg==1 | pog==1  | pou ==1 | pdk==1   // note that originally pou was in non-group                                                                              
# replace hicov = 1 if hpe==1 | hpo==1 | hpd==1 | hps==1
