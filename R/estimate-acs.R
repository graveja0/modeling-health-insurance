library(tidycensus)
install.packages("ipumsr")
library(ipumsr)
library(tidyverse)


## HIU INCOME
# HIUFPGINC is constructed by the State Health Access Data Assistance Center (SHADAC) at the University of Minnesota. Federal Poverty Guidelines (FPG) are issued by the Department of Health and Human Services.
# 
# FPG is a modified version of the poverty thresholds provided by the Census Bureau. FPG is used for administrative purposes, for example determining eligibility for federal programs such as Medicaid. Poverty thresholds are used for calculating official poverty statistics.
# 
# FPG varies by family size. The 48 contiguous states and DC use the same FPG while Alaska and Hawaii each have their own FPG.
# 
# HIUFPGBASE is the FPG for the first person in the family and should be used in conjunction with HIUFPGINC for each additional person in the HIU (HIUNPERS). Poverty is calculated for family units, but there are different ways to define the family unit depending on the analysis. For example, all related individuals in a household can be considered a family or for the purposes of studying health insurance coverage, health insurance units (HIUs) can be used. The SHADAC defined HIU variable is HIUID.
# 
# SHADAC has also provided information and example STATA and SAS code, available on their website, for using the HIU and FPG variables and replicating estimates available from the SHADAC Data Center.
# 
# An example of how to calculate the poverty cutoff using FPG follows:
#   
#   FPG poverty guidelines = HIUFPGBASE+ HIUFPGINC*(HIUNPERS-1)
# 
# FPG poverty cutoff = (Sum of personal income by family) divided by (FPG poverty guidelines)

# FPG poverty guidelines = HIUFPGBASE+ HIUFPGINC*(HIUNPERS-1)
# 
# FPG poverty cutoff = (Sum of personal income by family) divided by (FPG poverty guidelines)