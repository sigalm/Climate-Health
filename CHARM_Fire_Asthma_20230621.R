###################### Climate - Health Modeling v3.0 ########################

# ================================
rm(list = ls()) # remove any variables in R's memory

# setwd("C:/Users/alkin/Desktop/sigal sim/Climate-Health")
source("Sim Functions/MicroSim_20230324.R")
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



##################


#### 1) Population Inputs ####
#### Structural parameters ####

n_i <- 5000                              # number of individuals
n_t <- 8                          # time horizon (cycles)
cycle_length <- 1/52                  # length of each cycle (in years)

v_asthma_state_names <- c("0",        # No asthma 
                          "1",        # Complete control
                          "2",        # Well control
                          "3",        # Somewhat control
                          "4",        # Poor control
                          "5",        # No control at all
                          "50",       # Dead - asthma
                          "100")      # Dead - other cause


n_asthma_states <- length(v_asthma_state_names)                                  # save number of health states

#### Individual characteristics ####

# Read in microdata and extract subset
full_pop <- readRDS("Data/Microdata/microdata.rds")
set.seed(123)
asthma_pop <- subset(full_pop, subset=asthma_status>0)

# pop_sample <- full_pop[sample(nrow(full_pop), size = n_i), ]
#pop_sample$id <- 1:n_i                                                             # add ID numbers
# 
# asthma_sample <- asthma_pop[sample(nrow(asthma_pop), size = n_i), ]
# asthma_sample$id <- 1:n_i
# Get list of counties and fips codes
counties <- read.csv("Data/Microdata/counties.csv")


discount_rate_costs <- discount_rate_qalys <- 0.0                                # 3% discount rate for both costs and QALYs (0 for biweekly cycycle_lengthes up to a year)

v_interventions <- c("No intervention", "Distribute air filter")                 # intervention names (non-medical interventions)

m_asthma_therapy_probs <- read.csv("Data/Asthma/Therapies/therapies.csv", row.names = 1)
v_asthma_therapies <- names(m_asthma_therapy_probs)             # continuous therapies / asthma management 

m_asthma_healthcare_use_probs <- read.csv("Data/Asthma/Healthcare use/Healthcare_use_input.csv", row.names = 1, stringsAsFactors = FALSE)
v_healthcare_use <- c("none", "ocs","ugt","ed","hosp")                           # acute exacerbations - outcomes


baseline_birth_rate <- 0
# (52.4/1000) * cycle_length   # convert annual birth rate into rate for cycle length
annual_birth_rate_change <- 0               # annual change in birth rate - 0 for up to a year
# -0.035 * cycle_length

annual_allcause_mortality_change <- 0                                            # annual change in all cause mortality rate - 0 for up to a year
# (-2.2/100) * cycle_length



#### Transition probabilities ####

# Transition probabilities and risk modifiers (stored in a 3-dimensional array with risk factors along the z-axis)

risk_modifiers <- multisheet2array(
  path = "Data/Asthma/Transition probabilities/transition data _ weekly _ recalibrate2.xlsx", 
  range=("B1:I9"), x_names = v_asthma_state_names, y_names = v_asthma_state_names)


#### Cost and utility inputs ####

v_asthma_costs <- c(0,       # 0 (no asthma)
                    0,       # 1 (complete control)
                    100,     # 2 (well control)
                    1000,    # 3 (somewhat control)
                    5000,    # 4 (poor control)
                    10000,   # 5 (no control at all)
                    0,       # 50 (dead- asthma)
                    0)       # 100 (dead - other cause)

v_intervention_costs <- 20   # cost of intervention per person per cycle

v_asthma_hsu <- c(1,         # 0 (no asthma)
                  1,         # 1 (complete control)
                  0.9,       # 2 (well control)
                  0.8,       # 3 (somewhat control)
                  0.6,       # 4 (poor control)
                  0.5,       # 5 (no control at all)
                  0,         # 50 (dead - asthma)
                  0)         # 100 (dead - other cause)


#### 2) Climate Data ####

smoke_data <- readRDS("Data/Fire/smoke_per_cycle.rds")
smoke_data_0 <- smoke_data
smoke_data_0[,-ncol(smoke_data)] <- 0

fire_counties <- smoke_data$countyfip[smoke_data$Cycle_1 != 0 | smoke_data$Cycle_2 != 0]
asthma_fire_pop <- subset(asthma_pop, subset=countyfip %in% fire_counties)
asthma_fire_sample <- asthma_fire_pop[sample(nrow(asthma_fire_pop), size = n_i), ]
asthma_fire_sample$id <- 1:n_i

#### 3) Run Model ####

# profvis({
   sim_no_fire2 <-MicroSim(n_i, n_t, 
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
                        min_residual = 0,
                        seed = 12345,
                        record_run = TRUE,
                        description="Asthma Sim No Fire 0% Min Residual Probability")
#})

fig_no_resid <- make_figures(sim_no_fire2, "Health states over time, min_residual=0")
fig_0.1_resid <- make_figures(sim_no_fire, "Health states over time, min_residual=0.1")


sim_no_fire_rerun <- reRunMicroSim("Runs/results_20230912_1723.RData")



identical(sim_no_fire, sim_no_fire2)

sample1 <- asthma_fire_sample[asthma_fire_sample$asthma_status == "5", ]
sample1$id <- 1:nrow(sample1)

risk_modifiers2 <- risk_modifiers
risk_modifiers2[risk_modifiers2 == 1.049] <- 2

sim_fire_0.1_resid <- MicroSim(n_i, n_t, 
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
                     min_residual = 0.1,
                     seed = 12345,
                     record_run = FALSE,
                     description = "Asthma Sim With Fire and Lag 10% Min Resid")

n_t <- 20
figure_no_resid <- make_figures(sim_fire_no_resid, "No residual", 1)
figure_0.1_resid <- make_figures(sim_fire_0.1_resid, "10% min residual", 1)

