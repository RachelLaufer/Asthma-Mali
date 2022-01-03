
# Mali Asthma Project Uncertainty Analysis

################################################################################

# Extract just LRTI episodes
LRTI_u_df <- HO_u_df%>%filter(metric == "LRTI episodes")

LRTI_no_u_df <- LRTI_u_df%>%filter(strategy == "no intervention")
LRTI_llAb_u_df <- LRTI_u_df%>%filter(strategy == "llAb")
LRTI_llAb_pVax_u_df <- LRTI_u_df%>%filter(strategy == "llAb + pVax")

# Transform data structure to be rows = trials, columns = age in months
unique_ages <- unique(LRTI_u_df$age)

LRTI_age_no <- LRTI_no_u_df %>% 
  filter(age == unique_ages[1]) %>% 
  select(value)
names(LRTI_age_no) <- paste0(names(LRTI_age_no), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp <- LRTI_no_u_df %>% 
    filter(age == unique_ages[idx]) %>% 
    select(value)
  names(temp) <- paste0(names(temp), "_age", unique_ages[idx])
  LRTI_age_no <- cbind(LRTI_age_no, 
                    temp)
}


LRTI_age_llAb <- LRTI_llAb_u_df %>% 
  filter(age == unique_ages[1]) %>% 
  select(value)
names(LRTI_age_llAb) <- paste0(names(LRTI_age_llAb), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp2 <- LRTI_llAb_u_df %>% 
    filter(age == unique_ages[idx]) %>% 
    select(value)
  names(temp2) <- paste0(names(temp2), "_age", unique_ages[idx])
  LRTI_age_llAb <- cbind(LRTI_age_llAb, 
                       temp2)
}


LRTI_age_llAb_pVax <- LRTI_llAb_pVax_u_df %>% 
  filter(age == unique_ages[1]) %>% 
  select(value)
names(LRTI_age_llAb_pVax) <- paste0(names(LRTI_age_llAb_pVax), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp3 <- LRTI_llAb_pVax_u_df %>% 
    filter(age == unique_ages[idx]) %>% 
    select(value)
  names(temp3) <- paste0(names(temp3), "_age", unique_ages[idx])
  LRTI_age_llAb_pVax <- cbind(LRTI_age_llAb_pVax, 
                         temp3)
}

# Bin to age categories
age_cats <- c("0-<6", "6-<12", "12-<18", "18-<24", "24-<30", "30-<36")

LRTI_no_agebin <- cbind(rowSums(LRTI_age_no[, 1:6]), rowSums(LRTI_age_no[, 7:12]),
      rowSums(LRTI_age_no[, 13:18]), rowSums(LRTI_age_no[, 19:24]),
      rowSums(LRTI_age_no[, 25:30]), rowSums(LRTI_age_no[, 31:36]))
colnames(LRTI_no_agebin) <- age_cats

LRTI_llAb_agebin <- cbind(rowSums(LRTI_age_llAb[, 1:6]), rowSums(LRTI_age_llAb[, 7:12]),
                        rowSums(LRTI_age_llAb[, 13:18]), rowSums(LRTI_age_llAb[, 19:24]),
                        rowSums(LRTI_age_llAb[, 25:30]), rowSums(LRTI_age_llAb[, 31:36]))
colnames(LRTI_llAb_agebin) <- age_cats

LRTI_llAb_pVax_agebin <- cbind(rowSums(LRTI_age_llAb_pVax[, 1:6]), rowSums(LRTI_age_llAb_pVax[, 7:12]),
                          rowSums(LRTI_age_llAb_pVax[, 13:18]), rowSums(LRTI_age_llAb_pVax[, 19:24]),
                          rowSums(LRTI_age_llAb_pVax[, 25:30]), rowSums(LRTI_age_llAb_pVax[, 31:36]))
colnames(LRTI_llAb_pVax_agebin) <- age_cats


# Total percent decrease from status quo
LRTI_pd_tot_llAb <- (rowSums(LRTI_no_agebin) - rowSums(LRTI_llAb_agebin)) / rowSums(LRTI_no_agebin) * 100
LRTI_pd_tot_llAb_pVax <-(rowSums(LRTI_no_agebin) - rowSums(LRTI_llAb_pVax_agebin)) / rowSums(LRTI_no_agebin) * 100
  
# Percent decrease from status quo, by age bin
LRTI_pd_llAb <- (LRTI_no_agebin - LRTI_llAb_agebin) / LRTI_no_agebin * 100
LRTI_pd_llAb_pVax <- (LRTI_no_agebin - LRTI_llAb_pVax_agebin) / LRTI_no_agebin * 100

# adjust number of LRTI to account for all-cause mortality out to 6 years
tot_LRTI_no_u <- mort_adj_func(rowSums(LRTI_age_no), U5 = U5_mort, U9 = U9_mort)
tot_LRTI_llAb_u <- mort_adj_func(rowSums(LRTI_age_llAb), U5 = U5_mort, U9 = U9_mort)
tot_LRTI_llAb_pVax_u <-mort_adj_func(rowSums(LRTI_age_llAb_pVax), U5 = U5_mort, U9 = U9_mort)

# number of kids surviving to age 6 without RSV-LRTI for each strategy
tot_wo_LRTI_no_u <- pop_tot - tot_LRTI_no_u
tot_wo_LRTI_llAb_u <- pop_tot - tot_LRTI_llAb_u
tot_wo_LRTI_llAb_pVax_u <- pop_tot - tot_LRTI_llAb_pVax_u

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_u <- prev_no_rsv_func(prev_tot_u, pop_tot, rr_w_u, tot_LRTI_no_u, tot_wo_LRTI_no_u)

# number of asthma cases among those without RSV-LRTI
asth_wo_LRTI_no_u <- asth_no_rsv_func(tot_wo_LRTI_no_u, r_asth_norsv_u)
asth_wo_LRTI_llAb_u <- asth_no_rsv_func(tot_wo_LRTI_llAb_u, r_asth_norsv_u)
asth_wo_LRTI_llAb_pVax_u <- asth_no_rsv_func(tot_wo_LRTI_llAb_pVax_u, r_asth_norsv_u)

# number of asthma cases among those with RSV-LRTI
asth_LRTI_no_u <- asth_rsv_func(tot_LRTI_no_u, r_asth_norsv_u, rr_w_u)
asth_LRTI_llAb_u <- asth_rsv_func(tot_LRTI_llAb_u, r_asth_norsv_u, rr_w_u)
asth_LRTI_llAb_pVax_u <- asth_rsv_func(tot_LRTI_llAb_pVax_u, r_asth_norsv_u, rr_w_u)

# total with asthma
tot_asth_no_u <- tot_asth_func(asth_LRTI_no_u, asth_wo_LRTI_no_u)
tot_asth_llAb_u <- tot_asth_func(asth_LRTI_llAb_u, asth_wo_LRTI_llAb_u)
tot_asth_llAb_pVax_u <- tot_asth_func(asth_LRTI_llAb_pVax_u, asth_wo_LRTI_llAb_pVax_u)

# total asthma per 10,000 population
tot_asth_no_pr_u <- tot_asth_no_u / pop_tot * 10000
tot_asth_llAb_pr_u <- tot_asth_llAb_u / pop_tot * 10000
tot_asth_llAb_pVax_pr_u <- tot_asth_llAb_pVax_u / pop_tot * 10000

# total asthma percent decrease from status quo
tot_asth_llAb_pd_u <- (tot_asth_no_u - tot_asth_llAb_u) / tot_asth_no_u * 100
tot_asth_llAb_pVax_pd_u <- (tot_asth_no_u - tot_asth_llAb_pVax_u) / tot_asth_no_u * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_u <- asth_rsv_null_func(tot_LRTI_no_u, r_asth_norsv_u)
asth_null_llAb_u <- asth_rsv_null_func(tot_LRTI_llAb_u, r_asth_norsv_u)
asth_null_llAb_pVax_u <-asth_rsv_null_func(tot_LRTI_llAb_pVax_u, r_asth_norsv_u)

# RSV-LRTI attributable asthma
att_no_u <- asth_rsv_att_func(asth_LRTI_no_u, asth_null_no_u)
att_llAb_u <- asth_rsv_att_func(asth_LRTI_llAb_u, asth_null_llAb_u)
att_llAb_pVax_u <-asth_rsv_att_func(asth_LRTI_llAb_pVax_u, asth_null_llAb_pVax_u)

# RSV-LRTI attributable asthma per 10,000 population
att_no_pr_u <- att_no_u / pop_tot * 10000
att_llAb_pr_u <- att_llAb_u / pop_tot * 10000
att_llAb_pVax_pr_u <- att_llAb_pVax_u / pop_tot * 10000

# RSV-LRTI attributable asthma percent decrease from status quo
att_llAb_pd_u <- (att_no_u - att_llAb_u) / att_no_u * 100
att_llAb_pVax_pd_u <- (att_no_u - att_llAb_pVax_u) / att_no_u * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_u <- asth_no_rsv_func(pop_tot, r_asth_norsv_u)
all_rsv_prev_pr_u <- all_rsv_prev_u / pop_tot * 10000
all_rsv_prev_pd_u <- (tot_asth_no_u - all_rsv_prev_u) / tot_asth_no_u * 100
