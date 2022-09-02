
# Mali Asthma Project Uncertainty Analysis

################################################################################
# # total RSV-LRTI cases through 3 years of age
# CI_LRTI_tot_no <- CI_func(rowSums(LRTI_age_no))
# CI_LRTI_tot_llAb <- CI_func(rowSums(LRTI_age_llAb))
# CI_LRTI_tot_llAb_pVax <- CI_func(rowSums(LRTI_age_llAb_pVax))
# 
# # total RSV-LRTI hosps through 3 years of age
# CI_hosps_tot_no <- CI_func(rowSums(hosps_age_no))
# CI_hosps_tot_llAb <- CI_func(rowSums(hosps_age_llAb))
# CI_hosps_tot_llAb_pVax <- CI_func(rowSums(hosps_age_llAb_pVax))
# 
# # total RSV-LRTI deaths through 3 years of age
# CI_deaths_tot_no <- CI_func(rowSums(deaths_age_no))
# CI_deaths_tot_llAb <- CI_func(rowSums(deaths_age_llAb))
# CI_deaths_tot_llAb_pVax <- CI_func(rowSums(deaths_age_llAb_pVax))

# total RSV-LRTI cases through 2 years of age
CI_LRTI_tot_no <- CI_func(rowSums(LRTI_age_no[,1:24]))
CI_LRTI_tot_llAb <- CI_func(rowSums(LRTI_age_llAb[,1:24]))
CI_LRTI_tot_llAb_pVax <- CI_func(rowSums(LRTI_age_llAb_pVax[,1:24]))

# total RSV-LRTI hosps through 2 years of age
CI_hosps_tot_no <- CI_func(rowSums(hosps_age_no[,1:24]))
CI_hosps_tot_llAb <- CI_func(rowSums(hosps_age_llAb[,1:24]))
CI_hosps_tot_llAb_pVax <- CI_func(rowSums(hosps_age_llAb_pVax[,1:24]))

# total RSV-LRTI deaths through 2 years of age
CI_deaths_tot_no <- CI_func(rowSums(deaths_age_no[,1:24]))
CI_deaths_tot_llAb <- CI_func(rowSums(deaths_age_llAb[,1:24]))
CI_deaths_tot_llAb_pVax <- CI_func(rowSums(deaths_age_llAb_pVax[,1:24]))

# RSV-LRTI cases through 3 years of age, with agebins
CI_LRTI_age_no <- cbind(apply(LRTI_no_agebin, 2 ,CI_func), CI_LRTI_tot_no)
colnames(CI_LRTI_age_no) <- c(age_cats, "total")
CI_LRTI_age_llAb <- cbind(apply(LRTI_llAb_agebin, 2 ,CI_func), CI_LRTI_tot_llAb)
colnames(CI_LRTI_age_llAb) <- c(age_cats, "total")
CI_LRTI_age_llAb_pVax <- cbind(apply(LRTI_llAb_pVax_agebin, 2 ,CI_func), CI_LRTI_tot_llAb_pVax)
colnames(CI_LRTI_age_llAb_pVax) <- c(age_cats, "total")

# RSV-LRTI hosps through 3 years of age, with agebins
CI_hosps_age_no <- cbind(apply(hosps_no_agebin, 2 ,CI_func), CI_hosps_tot_no)
colnames(CI_hosps_age_no) <-c(age_cats, "total")
CI_hosps_age_llAb <- cbind(apply(hosps_llAb_agebin, 2 ,CI_func), CI_hosps_tot_llAb)
colnames(CI_hosps_age_llAb) <- c(age_cats, "total")
CI_hosps_age_llAb_pVax <- cbind(apply(hosps_llAb_pVax_agebin, 2 ,CI_func), CI_hosps_tot_llAb_pVax)
colnames(CI_hosps_age_llAb_pVax) <- c(age_cats, "total")

# RSV-LRTI deaths through 3 years of age, with agebins
CI_deaths_age_no <- cbind(apply(deaths_no_agebin, 2 ,CI_func), CI_deaths_tot_no)
colnames(CI_deaths_age_no) <- c(age_cats, "total")
CI_deaths_age_llAb <- cbind(apply(deaths_llAb_agebin, 2 ,CI_func), CI_deaths_tot_llAb)
colnames(CI_deaths_age_llAb) <-c(age_cats, "total")
CI_deaths_age_llAb_pVax <- cbind(apply(deaths_llAb_pVax_agebin, 2 ,CI_func), CI_deaths_tot_llAb_pVax)
colnames(CI_deaths_age_llAb_pVax) <- c(age_cats, "total")

# total RSV-LRTI percent decrease from status quo
CI_LRTI_pd_tot_llAb <- CI_func(LRTI_pd_tot_llAb)
CI_LRTI_pd_tot_llAb_pVax <- CI_func(LRTI_pd_tot_llAb_pVax)

# RSV-LRTI percent decrease from status quo, by age bin
CI_LRTI_pd_llAb <- cbind(apply(LRTI_pd_llAb, 2, CI_func), CI_LRTI_pd_tot_llAb)
colnames(CI_LRTI_pd_llAb) <- c(age_cats, "total")
CI_LRTI_pd_llAb_pVax <- cbind(apply(LRTI_pd_llAb_pVax, 2, CI_func), CI_LRTI_pd_tot_llAb_pVax)
colnames(CI_LRTI_pd_llAb_pVax) <- c(age_cats, "total")

# total RSV-LRTI hosps percent decrease from status quo
CI_hosps_pd_tot_llAb <- CI_func(hosps_pd_tot_llAb)
CI_hosps_pd_tot_llAb_pVax <- CI_func(hosps_pd_tot_llAb_pVax)

# RSV-LRTI hosps percent decrease from status quo, by age bin
CI_hosps_pd_llAb <- cbind(apply(hosps_pd_llAb, 2, CI_func), CI_hosps_pd_tot_llAb)
colnames(CI_hosps_pd_llAb) <- c(age_cats, "total")
CI_hosps_pd_llAb_pVax <- cbind(apply(hosps_pd_llAb_pVax, 2, CI_func), CI_hosps_pd_tot_llAb_pVax)
colnames(CI_hosps_pd_llAb_pVax) <- c(age_cats, "total")

# total RSV-LRTI deaths percent decrease from status quo
CI_deaths_pd_tot_llAb <- CI_func(deaths_pd_tot_llAb)
CI_deaths_pd_tot_llAb_pVax <- CI_func(deaths_pd_tot_llAb_pVax)

# RSV-LRTI deaths percent decrease from status quo, by age bin
CI_deaths_pd_llAb <- cbind(apply(deaths_pd_llAb, 2, CI_func), CI_deaths_pd_tot_llAb)
colnames(CI_deaths_pd_llAb) <- c(age_cats, "total")
CI_deaths_pd_llAb_pVax <- cbind(apply(deaths_pd_llAb_pVax, 2, CI_func), CI_deaths_pd_tot_llAb_pVax)
colnames(CI_deaths_pd_llAb_pVax) <- c(age_cats, "total")

# RSV-LRTI attributable asthma at 6 years
CI_att_no_u <- CI_func(att_no_u)
CI_att_llAb_u <- CI_func(att_llAb_u)
CI_att_llAb_pVax_u <- CI_func(att_llAb_pVax_u)

CI_att_df <- cbind(CI_att_no_u, CI_att_llAb_u, CI_att_llAb_pVax_u)
colnames(CI_att_df) <- c("no intervention", "llAb", "llAb + pVax")

# RSV-LRTI attributable asthma per 10,000 at 6 years
CI_att_no_pr_u <- CI_func(att_no_pr_u)
CI_att_llAb_pr_u <- CI_func(att_llAb_pr_u)
CI_att_llAb_pVax_pr_u <- CI_func(att_llAb_pVax_pr_u)

CI_att_pr_df <- cbind(CI_att_no_pr_u, CI_att_llAb_pr_u, CI_att_llAb_pVax_pr_u)
colnames(CI_att_pr_df) <- c("no intervention", "llAb", "llAb + pVax")

# RSV-LRTI attributable asthma percent decrease from status quo
CI_att_llAb_pd_u <- CI_func(att_llAb_pd_u)
CI_att_llAb_pVax_pd_u <- CI_func(att_llAb_pVax_pd_u)

CI_att_pd_df <- cbind(CI_att_llAb_pd_u, CI_att_llAb_pVax_pd_u)
colnames(CI_att_pd_df) <- c("llAb", "llAb + pVax")

# total with asthma
CI_tot_asth_no_u <- CI_func(tot_asth_no_u)
CI_tot_asth_llAb_u <- CI_func(tot_asth_llAb_u)
CI_tot_asth_llAb_pVax_u <- CI_func(tot_asth_llAb_pVax_u)

CI_tot_asth_df <- cbind(CI_tot_asth_no_u, CI_tot_asth_llAb_u, CI_tot_asth_llAb_pVax_u)
colnames(CI_tot_asth_df) <- c("no intervention", "llAb", "llAb + pVax")

# total asthma per 10,000 population
CI_tot_asth_no_pr_u <- CI_func(tot_asth_no_pr_u)
CI_tot_asth_llAb_pr_u <- CI_func(tot_asth_llAb_pr_u)
CI_tot_asth_llAb_pVax_pr_u <- CI_func(tot_asth_llAb_pVax_pr_u)

CI_tot_asth_pr_df <- cbind(CI_tot_asth_no_pr_u, CI_tot_asth_llAb_pr_u, CI_tot_asth_llAb_pVax_pr_u)
colnames(CI_tot_asth_pr_df) <- c("no intervention", "llAb", "llAb + pVax")

# total ashtma percent decrease from status quo
CI_tot_asth_llAb_pd_u <- CI_func(tot_asth_llAb_pd_u)
CI_tot_asth_llAb_pVax_pd_u <- CI_func(tot_asth_llAb_pVax_pd_u)

CI_tot_asth_pd_df <- cbind(CI_tot_asth_llAb_pd_u, CI_tot_asth_llAb_pVax_pd_u)
colnames(CI_tot_asth_pd_df) <- c("llAb", "llAb + pVax")

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
CI_all_rsv_prev_u <- CI_func(all_rsv_prev_u)
CI_all_rsv_prev_pr_u <- CI_func(all_rsv_prev_pr_u)
CI_all_rsv_prev_pd_u <- CI_func(all_rsv_prev_pd_u)

CI_all_rsv_df <- cbind(CI_all_rsv_prev_u, CI_all_rsv_prev_pr_u, CI_all_rsv_prev_pd_u)
colnames(CI_all_rsv_df) <- c("cases at 6 yrs", "prevalence per 10,000 at 6 yrs", "percent decrease from status quo")


