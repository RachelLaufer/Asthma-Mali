# Mali Asthma Project: deaths

################################################################################

# Extract just deaths
deaths_u_df <- HO_u_df%>%filter(metric == "Deaths")

deaths_no_u_df <- deaths_u_df%>%filter(strategy == "no intervention")
deaths_llAb_u_df <- deaths_u_df%>%filter(strategy == "llAb")
deaths_llAb_pVax_u_df <- deaths_u_df%>%filter(strategy == "llAb + pVax")

# Transform data structure to be rows = trials, columns = age in months
unique_ages <- unique(deaths_u_df$age)

deaths_age_no <- deaths_no_u_df %>% 
  filter(age == unique_ages[1]) %>% 
  select(value)
names(deaths_age_no) <- paste0(names(deaths_age_no), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp <- deaths_no_u_df %>% 
    filter(age == unique_ages[idx]) %>% 
    select(value)
  names(temp) <- paste0(names(temp), "_age", unique_ages[idx])
  deaths_age_no <- cbind(deaths_age_no, 
                        temp)
}


deaths_age_llAb <- deaths_llAb_u_df %>% 
  filter(age == unique_ages[1]) %>% 
  select(value)
names(deaths_age_llAb) <- paste0(names(deaths_age_llAb), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp2 <- deaths_llAb_u_df %>% 
    filter(age == unique_ages[idx]) %>% 
    select(value)
  names(temp2) <- paste0(names(temp2), "_age", unique_ages[idx])
  deaths_age_llAb <- cbind(deaths_age_llAb, 
                          temp2)
}


deaths_age_llAb_pVax <- deaths_llAb_pVax_u_df %>% 
  filter(age == unique_ages[1]) %>% 
  select(value)
names(deaths_age_llAb_pVax) <- paste0(names(deaths_age_llAb_pVax), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp3 <- deaths_llAb_pVax_u_df %>% 
    filter(age == unique_ages[idx]) %>% 
    select(value)
  names(temp3) <- paste0(names(temp3), "_age", unique_ages[idx])
  deaths_age_llAb_pVax <- cbind(deaths_age_llAb_pVax, 
                               temp3)
}

# Bin to age categories
age_cats <- c("0-<6", "6-<12", "12-<18", "18-<24", "24-<30", "30-<36")

deaths_no_agebin <- cbind(rowSums(deaths_age_no[, 1:6]), rowSums(deaths_age_no[, 7:12]),
                         rowSums(deaths_age_no[, 13:18]), rowSums(deaths_age_no[, 19:24]),
                         rowSums(deaths_age_no[, 25:30]), rowSums(deaths_age_no[, 31:36]))
colnames(deaths_no_agebin) <- age_cats

deaths_llAb_agebin <- cbind(rowSums(deaths_age_llAb[, 1:6]), rowSums(deaths_age_llAb[, 7:12]),
                           rowSums(deaths_age_llAb[, 13:18]), rowSums(deaths_age_llAb[, 19:24]),
                           rowSums(deaths_age_llAb[, 25:30]), rowSums(deaths_age_llAb[, 31:36]))
colnames(deaths_llAb_agebin) <- age_cats

deaths_llAb_pVax_agebin <- cbind(rowSums(deaths_age_llAb_pVax[, 1:6]), rowSums(deaths_age_llAb_pVax[, 7:12]),
                                rowSums(deaths_age_llAb_pVax[, 13:18]), rowSums(deaths_age_llAb_pVax[, 19:24]),
                                rowSums(deaths_age_llAb_pVax[, 25:30]), rowSums(deaths_age_llAb_pVax[, 31:36]))
colnames(deaths_llAb_pVax_agebin) <- age_cats

# total deaths
deaths_tot_no <- rowSums(deaths_age_no)
deaths_tot_llAb <- rowSums(deaths_age_llAb)
deaths_tot_llAb_pVax <- rowSums(deaths_age_llAb_pVax)

# total deaths percent decrease from status quo
deaths_pd_tot_llAb <- (deaths_tot_no - deaths_tot_llAb) / deaths_tot_no * 100
deaths_pd_tot_llAb_pVax <- (deaths_tot_no - deaths_tot_llAb_pVax) / deaths_tot_no * 100

# deaths percent decrease from status quo, with age bins
deaths_pd_llAb <- (deaths_no_agebin - deaths_llAb_agebin) / deaths_no_agebin * 100
deaths_pd_llAb_pVax <- (deaths_no_agebin - deaths_llAb_pVax_agebin) / deaths_no_agebin * 100


## point estimate calculations
deaths_df <- HO_df%>%filter(metric == "D: Deaths")
deaths_no_df <- deaths_df%>%filter(intervention == "status quo")
deaths_llAb_df <- deaths_df%>%filter(intervention == "mAb")
deaths_llAb_pVax_df <- deaths_df%>%filter(intervention == "mAb + pVax 10 & 14 wks")

sum(deaths_no_df[1:6,4])
sum(deaths_no_df[7:12,4])
sum(deaths_no_df[13:18,4])
sum(deaths_no_df[19:24,4])
sum(deaths_no_df[25:30,4])
sum(deaths_no_df[31:36,4])

sum(deaths_llAb_df[1:6,4])
sum(deaths_llAb_df[7:12,4])
sum(deaths_llAb_df[13:18,4])
sum(deaths_llAb_df[19:24,4])
sum(deaths_llAb_df[25:30,4])
sum(deaths_llAb_df[31:36,4])

sum(deaths_llAb_pVax_df[1:6,4])
sum(deaths_llAb_pVax_df[7:12,4])
sum(deaths_llAb_pVax_df[13:18,4])
sum(deaths_llAb_pVax_df[19:24,4])
sum(deaths_llAb_pVax_df[25:30,4])
sum(deaths_llAb_pVax_df[31:36,4])
