
# Mali Asthma Project Parameters

################################################################################

# Under 5 mortality
U5_mort <- 101/1000
  
# 5-9 mortality
U9_mort <- 14.5/1000

# total population surviving to 6 years of age
# 698,003
pop_tot <- 698003

# baseline rate of wheeze/asthma among total pop by 6 years (w/ and without RSV)
# 10.8 %
prev_tot <- 121/1116
prev_tot_u <- rbeta(trials, 121, (1116 - 121))
  
# Approximate risk ratio of wheeze/asthma given RSV-LRTI
# based on Brunwasser et al. 2020 adjusted odds ratio
# 2.45, 95% CI (1.23, 4.88)
rr_w <- 2.45

rr_w_l <- 1.23 # lower bound
rr_w_h <- 4.88 # higher bound

log(rr_w) - log(rr_w_h)
log(rr_w) - log(rr_w_h)
# checking to make sure distance is similar
rr_w_sd <- (log(rr_w_h) - log(rr_w_l))/ (1.96*2) # standard deviation
rr_w_sample <- rnorm(trials, log(rr_w), rr_w_sd) # normal dist of log
rr_w_u <- exp(rr_w_sample) # retransformed uncertainty distribution


########
# MAKE SURE TO COMMENT TIHS OUT, UNLESS RUNNING THIS SECONDARY ANALYSIS
# As a secondary analysis, we used 5·5% 
# (severe asthma symptoms in the past 12 months for the African
# and Eastern Mediterranean Region)
# prev_tot <- 5.5/100
#########

########
# MAKE SURE TO COMMENT TIHS OUT, UNLESS RUNNING THIS SECONDARY ANALYSIS
# As a secondary analysis, we used 23·2% (highest single center estimate)
# as the estimate for the overall prevalence of wheeze/asthma
# prev_tot <- 23.2/100
#########

#########
# MAKE SURE TO COMMENT THIS OUT, UNLESS RUNNING THIS SECONDARY ANALYSIS
# As a secondary analysis, we use the lower bound of the relative risk
# for recurrent wheeze/asthma RR = 1.23
# rr_w <- rr_w_l
#########

