
# Mali Asthma Project Uncertainty Analysis

################################################################################

# RSV-LRTI cases through 3 years of age, with agebins
CI_LRTI_age_no <- apply(LRTI_no_agebin, 2 ,CI_func)
CI_LRTI_age_llAb <- apply(LRTI_llAb_agebin, 2 ,CI_func)
CI_LRTI_age_llAb_pVax <- apply(LRTI_llAb_pVax_agebin, 2 ,CI_func)

# RSV-LRTI hosps through 3 years of age, with agebins
CI_hosps_age_no <- apply(hosps_no_agebin, 2 ,CI_func)
CI_hosps_age_llAb <- apply(hosps_llAb_agebin, 2 ,CI_func)
CI_hosps_age_llAb_pVax <- apply(hosps_llAb_pVax_agebin, 2 ,CI_func)

# RSV-LRTI deaths through 3 years of age, with agebins
CI_deaths_age_no <- apply(deaths_no_agebin, 2 ,CI_func)
CI_deaths_age_llAb <- apply(deaths_llAb_agebin, 2 ,CI_func)
CI_deaths_age_llAb_pVax <- apply(deaths_llAb_pVax_agebin, 2 ,CI_func)

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



