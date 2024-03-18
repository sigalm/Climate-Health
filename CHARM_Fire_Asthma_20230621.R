###################### Climate - Health Modeling v3.0 ########################

# ================================
rm(list = ls()) # remove any variables in R's memory

# setwd("C:/Users/alkin/Desktop/sigal sim/Climate-Health")
source("Sim Functions/MicroSim_parallel.R")
source("Sim Functions/MicroSim_serial.R")
source("Sim Functions/Probs2.R")
source("Sim Functions/Costs.R")
source("Sim Functions/Effs.R")
source("Sim Functions/multisheet2array.R")
source("Sim Functions/logging.R")
source("Sim Functions/Tables_Figures.R")
library(readxl)
library(dplyr)
library(profvis)
library(ggplot2)
library(reshape2)
library(scales)
library(RColorBrewer)
library(ggthemes)
library(knitr)
library(gridExtra)
library(matrixStats)

# Testing parallel processing
library(doParallel)
library(foreach)



##################


#### 1) Population Inputs ####
#### Structural parameters ####

n_i <- 5000                      # number of individuals
n_t <- 20                       # time horizon (cycles)
cycle_length <- 1/52              # length of each cycle (in years)

v_asthma_state_names <- c("0",        # No asthma 
                          "1",        # Complete control
                          "2",        # Well control
                          "3",        # Somewhat control
                          "4",        # Poor control
                          "5",        # No control at all
                          "50",       # Dead - asthma
                          "100")      # Dead - other cause


n_asthma_states <- length(v_asthma_state_names)     # save number of health states

#### Individual characteristics ####

# Read in microdata and extract subset of those with asthma
full_pop <- readRDS("Data/Microdata/microdata.rds")
set.seed(123)
asthma_pop <- subset(full_pop, subset=asthma_status>0)

# Alternative sample with no asthma restriction
# pop_sample <- full_pop[sample(nrow(full_pop), size = n_i), ]
# pop_sample$id <- 1:n_i                                       # add ID numbers
# 
# asthma_sample <- asthma_pop[sample(nrow(asthma_pop), size = n_i), ]
# asthma_sample$id <- 1:n_i

# Get list of counties and fips codes
counties <- read.csv("Data/Microdata/counties.csv")


discount_rate_costs <- discount_rate_qalys <- 0.0         # 3% discount rate for both costs and QALYs (0 for biweekly cycycle_lengthes up to a year)

v_interventions <- c("No intervention", "Distribute air filter")  # intervention names (non-medical interventions)

m_asthma_therapy_probs <- read.csv("Data/Asthma/Therapies/therapies.csv", row.names = 1)
v_asthma_therapies <- names(m_asthma_therapy_probs)             # continuous therapies / asthma management 

m_asthma_healthcare_use_probs_nofire <- read.csv("Data/Asthma/Healthcare use/Healthcare_use_input.csv", row.names = 1, stringsAsFactors = FALSE)
m_asthma_healthcare_use_probs_fireadj <- read.csv("Data/Asthma/Healthcare use/Healthcare_use_fire_input.csv", row.names = 1, stringsAsFactors = FALSE)
v_healthcare_use <- c("none", "ocs","ugt","ed","hosp")          # acute exacerbations - outcomes


baseline_birth_rate <- 0
# (52.4/1000) * cycle_length   # convert annual birth rate into rate for cycle length
annual_birth_rate_change <- 0  # annual change in birth rate - 0 for up to a year
# -0.035 * cycle_length

annual_allcause_mortality_change <- 0   # annual change in all cause mortality rate - 0 for up to a year
# (-2.2/100) * cycle_length



#### Transition probabilities ####

# Transition probabilities and risk modifiers (stored in a 3-dimensional array with risk factors along the z-axis)

risk_modifiers <- multisheet2array(
  path = "Data/Asthma/Transition probabilities/transition data_weekly_recalibrate2.xlsx", 
  range=("B1:I9"), x_names = v_asthma_state_names, y_names = v_asthma_state_names)


#### Cost and utility inputs ####

# Note - make sure costs are weekly!! see the file medical_costs_inflation.R
v_asthma_costs <- c(100,       # 0 (no asthma)
                    100,       # 1 (complete control)
                    113,     # 2 (well control)
                    142,    # 3 (somewhat control)
                    225,    # 4 (poor control)
                    439,   # 5 (no control at all)
                    0,       # 50 (dead- asthma)
                    0)       # 100 (dead - other cause)  #annual costs

names(v_asthma_costs) <- v_asthma_state_names


v_intervention_costs <- 20   # cost of intervention per person per cycle

v_asthma_hsu <- c(1,         # 0 (no asthma)
                  0.95,         # 1 (complete control)
                  0.89,       # 2 (well control)
                  0.83,       # 3 (somewhat control)
                  0.77,       # 4 (poor control)
                  0.71,       # 5 (no control at all)
                  0,         # 50 (dead - asthma)
                  0)         # 100 (dead - other cause)
names(v_asthma_hsu) <- v_asthma_state_names

v_hsu_decrements <- c(-0.200,
                      -0.163, 
                      -0.132, 
                      -0.125, 
                      -0.115, 
                      -0.065,
                      -0.039,
                      0)

max_dur_decrement <- length(v_hsu_decrements)


#### 2) Climate Data ####

# Read in smoke data and create the "no fire" dataframe
smoke_data <- readRDS("Data/Fire/smoke_per_cycle_5wkstartup.rds")
smoke_data_0 <- smoke_data
smoke_data_0[,-ncol(smoke_data)] <- 0

# Identify counties that experienced any smoke, further subset the sample to include only those affected by smoke
fire_counties <- smoke_data$countyfip[smoke_data$Cycle_5 != 0 | smoke_data$Cycle_6 != 0]
asthma_fire_pop <- subset(asthma_pop, subset=countyfip %in% fire_counties)
asthma_fire_sample <- asthma_fire_pop[sample(nrow(asthma_fire_pop), size = n_i), ]
asthma_fire_sample$id <- 1:n_i

#### 3) Run Model ####

# profvis({
sim_no_fire <-MicroSim_parallel(n_i, n_t, 
                           smoke_data = smoke_data_0,
                           v_asthma_state_names, 
                           asthma_fire_sample, 
                           risk_modifiers,
                           cycle_length, 
                           baseline_birth_rate, annual_birth_rate_change, 
                           annual_allcause_mortality_change,
                           v_asthma_therapies,
                           m_asthma_therapy_probs, 
                           v_asthma_costs,
                           v_intervention_costs,
                           intervention_coverage = 0, 
                           intervention_trigger = 0, 
                           discount_rate_costs,
                           discount_rate_qalys,
                           cores=4,
                           min_residual = 0,
                           seed = 12345,
                           record_run = FALSE,
                           description="Asthma Sim No Fire 0% Min Residual Probability")
#})

fig_no_resid <- make_figures(sim_no_fire, "Health states over time, min_residual=0")


sim_no_fire_rerun <- reRunMicroSim("Runs/results_20230912_1723.RData")

identical(sim_no_fire, sim_no_fire2)

sim_fire_0.1_resid_serial <- MicroSim_serial(n_i, n_t, 
                               smoke_data = smoke_data,
                               v_asthma_state_names, 
                               pop_sample = asthma_fire_sample, 
                               risk_modifiers,
                               cycle_length, 
                               baseline_birth_rate, annual_birth_rate_change, 
                               annual_allcause_mortality_change,
                               v_asthma_therapies,
                               m_asthma_therapy_probs, 
                               v_asthma_costs,
                               v_intervention_costs,
                               intervention_coverage = 0, 
                               intervention_trigger = 0, 
                               discount_rate_costs,
                               discount_rate_qalys,
                               # cores=4,
                               min_residual = 0.1,
                               seed = 12345,
                               record_run = FALSE,
                               description = "Asthma Sim With Fire and Lag 50% Min Resid")

figure_0.1_resid <- make_figures(sim_fire_0.1_resid, "10% min residual", 1)

identical(sim_fire_0.1_resid_serial$m_asthma_states, sim_fire_0.1_resid$m_asthma_states)
