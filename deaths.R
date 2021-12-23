# Mali Asthma Project: deaths

################################################################################

# Extract just hospitalizations
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