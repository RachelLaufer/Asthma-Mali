
# Mali Asthma Project

################################################################################
# Step 1: load in health outcomes data for base case and uncertainty analysis

# Step 2: load asthma params

# Step 3: create functions for asthma calcs

# Step 4: transform data to desired structures

# Step 5: apply asthma functions to obtain output

# Step 6: repeat for uncertainty analysis

# Step 7: obtain 95% credible intervals for all calculations
################################################################################
trials <- 10000

library(tidyverse)

# S.1
HO_df <- (read.csv("Health_Outcomes_Data.csv", header = TRUE, sep = ","))
HO_u_df <- readRDS(file = "Health_Outcomes_Uncertainty.rds")

# S.2
source("asthma_params.R")

# S.3
source("asthma_funcs.R")

# S.4
# Extract just LRTI episodes
LRTI_df <- HO_df%>%filter(metric == "B: LRTI episodes")

LRTI_no_df <- LRTI_df%>%filter(intervention == "status quo")
LRTI_llAb_df <- LRTI_df%>%filter(intervention == "mAb")
LRTI_llAb_pVax_df <- LRTI_df%>%filter(intervention == "mAb + pVax 10 & 14 wks")

# point estimates in 6-month age bins
sum(LRTI_no_df[1:6,4])
sum(LRTI_no_df[7:12,4])
sum(LRTI_no_df[13:18,4])
sum(LRTI_no_df[19:24,4])
sum(LRTI_no_df[25:30,4])
sum(LRTI_no_df[31:36,4])

sum(LRTI_llAb_df[1:6,4])
sum(LRTI_llAb_df[7:12,4])
sum(LRTI_llAb_df[13:18,4])
sum(LRTI_llAb_df[19:24,4])
sum(LRTI_llAb_df[25:30,4])
sum(LRTI_llAb_df[31:36,4])

sum(LRTI_llAb_pVax_df[1:6,4])
sum(LRTI_llAb_pVax_df[7:12,4])
sum(LRTI_llAb_pVax_df[13:18,4])
sum(LRTI_llAb_pVax_df[19:24,4])
sum(LRTI_llAb_pVax_df[25:30,4])
sum(LRTI_llAb_pVax_df[31:36,4])

# S.5
# adjust number of LRTI to account for all-cause mortality out to 6 years
# LRTI_df[,5] <- mapply(mort_adj_func, LRTI_df$value, MoreArgs = list(U5 = U5_mort, U9 = U9_mort))
# LRTI_df <- rename(LRTI_df, "mort_adj_LRTI" = V5)

tot_LRTI_no <- mort_adj_func(sum(LRTI_no_df$value), U5 = U5_mort, U9 = U9_mort)
tot_LRTI_llAb <- mort_adj_func(sum(LRTI_llAb_df$value), U5 = U5_mort, U9 = U9_mort)
tot_LRTI_llAb_pVax <-mort_adj_func(sum(LRTI_llAb_pVax_df$value), U5 = U5_mort, U9 = U9_mort)

# number of kids surviving to age 6 without RSV-LRTI for each strategy
# no intervention, llAb, and llAb + pVax
tot_wo_LRTI_no <- pop_tot - tot_LRTI_no
tot_wo_LRTI_llAb <- pop_tot - tot_LRTI_llAb
tot_wo_LRTI_llAb_pVax <- pop_tot - tot_LRTI_llAb_pVax

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv <- prev_no_rsv_func(prev_tot, pop_tot, rr_w, tot_LRTI_no, tot_wo_LRTI_no)

# number of asthma cases among those without RSV-LRTI
asth_wo_LRTI_no <- asth_no_rsv_func(tot_wo_LRTI_no, r_asth_norsv)
asth_wo_LRTI_llAb <- asth_no_rsv_func(tot_wo_LRTI_llAb, r_asth_norsv)
asth_wo_LRTI_llAb_pVax <- asth_no_rsv_func(tot_wo_LRTI_llAb_pVax, r_asth_norsv)

# number of asthma cases among those with RSV-LRTI
asth_LRTI_no <- asth_rsv_func(tot_LRTI_no, r_asth_norsv, rr_w)
asth_LRTI_llAb <- asth_rsv_func(tot_LRTI_llAb, r_asth_norsv, rr_w)
asth_LRTI_llAb_pVax <- asth_rsv_func(tot_LRTI_llAb_pVax, r_asth_norsv, rr_w)

# total with asthma
tot_asth_no <- tot_asth_func(asth_LRTI_no, asth_wo_LRTI_no)
tot_asth_llAb <- tot_asth_func(asth_LRTI_llAb, asth_wo_LRTI_llAb)
tot_asth_llAb_pVax <-tot_asth_func(asth_LRTI_llAb_pVax, asth_wo_LRTI_llAb_pVax)

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no <- asth_rsv_null_func(tot_LRTI_no, r_asth_norsv)
asth_null_llAb <- asth_rsv_null_func(tot_LRTI_llAb, r_asth_norsv)
asth_null_llAb_pVax <-asth_rsv_null_func(tot_LRTI_llAb_pVax, r_asth_norsv)
  
# RSV-LRTI attributable asthma
att_no <- asth_rsv_att_func(asth_LRTI_no, asth_null_no)
att_llAb <- asth_rsv_att_func(asth_LRTI_llAb, asth_null_llAb)
att_llAb_pVax <-asth_rsv_att_func(asth_LRTI_llAb_pVax, asth_null_llAb_pVax)

# total recurrent wheeze/asthma, all RSV LRTI prevented
asth_all_RSV_prev <- asth_null_no + asth_wo_LRTI_no

# S.6
source("uncertainty_analysis.R")

# S.7
source("hospitalizations.R")
source("deaths.R")
source("credible_intervals.R")

################################################################################