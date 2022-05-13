# Mali Asthma Project: hospitalizations

################################################################################

# Extract just hospitalizations
hosps_u_df <- HO_u_df%>%filter(metric == "Hospitalizations")

hosps_no_u_df <- hosps_u_df%>%filter(strategy == "no intervention")
hosps_llAb_u_df <- hosps_u_df%>%filter(strategy == "llAb")
hosps_llAb_pVax_u_df <- hosps_u_df%>%filter(strategy == "llAb + pVax")

# Transform data structure to be rows = trials, columns = age in months
unique_ages <- unique(hosps_u_df$age)

hosps_age_no <- hosps_no_u_df %>% 
  filter(age == unique_ages[1]) %>% 
  select(value)
names(hosps_age_no) <- paste0(names(hosps_age_no), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp <- hosps_no_u_df %>% 
    filter(age == unique_ages[idx]) %>% 
    select(value)
  names(temp) <- paste0(names(temp), "_age", unique_ages[idx])
  hosps_age_no <- cbind(hosps_age_no, 
                       temp)
}


hosps_age_llAb <- hosps_llAb_u_df %>% 
  filter(age == unique_ages[1]) %>% 
  select(value)
names(hosps_age_llAb) <- paste0(names(hosps_age_llAb), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp2 <- hosps_llAb_u_df %>% 
    filter(age == unique_ages[idx]) %>% 
    select(value)
  names(temp2) <- paste0(names(temp2), "_age", unique_ages[idx])
  hosps_age_llAb <- cbind(hosps_age_llAb, 
                         temp2)
}


hosps_age_llAb_pVax <- hosps_llAb_pVax_u_df %>% 
  filter(age == unique_ages[1]) %>% 
  select(value)
names(hosps_age_llAb_pVax) <- paste0(names(hosps_age_llAb_pVax), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp3 <- hosps_llAb_pVax_u_df %>% 
    filter(age == unique_ages[idx]) %>% 
    select(value)
  names(temp3) <- paste0(names(temp3), "_age", unique_ages[idx])
  hosps_age_llAb_pVax <- cbind(hosps_age_llAb_pVax, 
                              temp3)
}

# Bin to age categories
age_cats <- c("0-<6", "6-<12", "12-<18", "18-<24", "24-<30", "30-<36")

hosps_no_agebin <- cbind(rowSums(hosps_age_no[, 1:6]), rowSums(hosps_age_no[, 7:12]),
                        rowSums(hosps_age_no[, 13:18]), rowSums(hosps_age_no[, 19:24]),
                        rowSums(hosps_age_no[, 25:30]), rowSums(hosps_age_no[, 31:36]))
colnames(hosps_no_agebin) <- age_cats

hosps_llAb_agebin <- cbind(rowSums(hosps_age_llAb[, 1:6]), rowSums(hosps_age_llAb[, 7:12]),
                          rowSums(hosps_age_llAb[, 13:18]), rowSums(hosps_age_llAb[, 19:24]),
                          rowSums(hosps_age_llAb[, 25:30]), rowSums(hosps_age_llAb[, 31:36]))
colnames(hosps_llAb_agebin) <- age_cats

hosps_llAb_pVax_agebin <- cbind(rowSums(hosps_age_llAb_pVax[, 1:6]), rowSums(hosps_age_llAb_pVax[, 7:12]),
                               rowSums(hosps_age_llAb_pVax[, 13:18]), rowSums(hosps_age_llAb_pVax[, 19:24]),
                               rowSums(hosps_age_llAb_pVax[, 25:30]), rowSums(hosps_age_llAb_pVax[, 31:36]))
colnames(hosps_llAb_pVax_agebin) <- age_cats

# total hosps
hosps_tot_no <- rowSums(hosps_age_no)
hosps_tot_llAb <- rowSums(hosps_age_llAb)
hosps_tot_llAb_pVax <- rowSums(hosps_age_llAb_pVax)

# total hosps percent decrease from status quo
hosps_pd_tot_llAb <- (hosps_tot_no - hosps_tot_llAb) / hosps_tot_no * 100
hosps_pd_tot_llAb_pVax <- (hosps_tot_no - hosps_tot_llAb_pVax) / hosps_tot_no * 100

# hosps percent decrease from status quo, with age bins
hosps_pd_llAb <- (hosps_no_agebin - hosps_llAb_agebin) / hosps_no_agebin * 100
hosps_pd_llAb_pVax <- (hosps_no_agebin - hosps_llAb_pVax_agebin) / hosps_no_agebin * 100

## point estimate calculations
hosps_df <- HO_df%>%filter(metric == "C: Hospitalizations")
hosps_no_df <- hosps_df%>%filter(intervention == "status quo")
hosps_llAb_df <- hosps_df%>%filter(intervention == "mAb")
hosps_llAb_pVax_df <- hosps_df%>%filter(intervention == "mAb + pVax 10 & 14 wks")

sum(hosps_no_df[1:6,4])
sum(hosps_no_df[7:12,4])
sum(hosps_no_df[13:18,4])
sum(hosps_no_df[19:24,4])
sum(hosps_no_df[25:30,4])
sum(hosps_no_df[31:36,4])

sum(hosps_llAb_df[1:6,4])
sum(hosps_llAb_df[7:12,4])
sum(hosps_llAb_df[13:18,4])
sum(hosps_llAb_df[19:24,4])
sum(hosps_llAb_df[25:30,4])
sum(hosps_llAb_df[31:36,4])

sum(hosps_llAb_pVax_df[1:6,4])
sum(hosps_llAb_pVax_df[7:12,4])
sum(hosps_llAb_pVax_df[13:18,4])
sum(hosps_llAb_pVax_df[19:24,4])
sum(hosps_llAb_pVax_df[25:30,4])
sum(hosps_llAb_pVax_df[31:36,4])
