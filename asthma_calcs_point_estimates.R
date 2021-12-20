
# Mali Asthma Project

################################################################################
# Step 1: load in health outcomes data for base case

# Step 2: load asthma params

# Step 3: create functions for asthma calcs

# Step 4: transform data to desired structures

# Step 5: apply asthma functions to obtain output

# Step 6: repeat for uncertainty analysis

################################################################################
trials <- 10000

# S.1
HO_df <- read.csv("Health_Outcomes.csv", header = TRUE, sep = ",")

# S.2
source("asthma_params.R")

# S.3
source("asthma_funcs.R")

# S.4


