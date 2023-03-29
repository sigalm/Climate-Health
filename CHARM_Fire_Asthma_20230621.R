###################### Climate - Health Modeling v3.0 ########################

# ================================
rm(list = ls()) # remove any variables in R's memory

source("Sim Functions/MicroSim_20230324.R")
source("Sim Functions/Probs2.R")
source("Sim Functions/Costs.R")
source("Sim Functions/Effs.R")
source("Sim Functions/multisheet2array.R")
source("Sim Functions/logging.R")
library(readxl)
library(dplyr)
##################


#### 1) Population Inputs ####
#### Structural parameters ####

n_i <- 5000                           # number of individuals
n_t <- 52                             # time horizon (cycles)
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
pop_sample <- full_pop[sample(nrow(full_pop), size = n_i), ]
pop_sample$id <- 1:n_i                                                             # add ID numbers


# m_individual_features$prior_exposure <- FALSE                                    # no previous exposure at the beginning

counties <- read.csv("Data/Microdata/counties.csv")
counties <- counties$FIPS
n_counties <- length(counties) # save total number of neighborhoods

# Create county smoke impact map. The smoke impact will be the inverse of
# the distance (so if the fire is further away, the impact will be
# proportionately smaller)
library(geosphere)
coordinates <- read.table("Data/Fire/archive/county_coordinates.txt", sep = "\t",
                          header = TRUE)                                         # https://www.census.gov/geographies/reference-files/time-series/geo/gazetteer-files.html
coordinates$GEOID <- coordinates$GEOID - 6000                                    # These are the FIPS codes without the state FIPS

distance_matrix <- distm(coordinates[, c("INTPTLONG", "INTPTLAT")],
                         fun = distHaversine)                                    # Calculate the distance matrix between counties using the Haversine formula
inverse_distance <- 1/(distance_matrix + 1)                                      # Add 1 to avoid division by 0 errors

county_distance_weights <- scales::rescale(inverse_distance)                     # Rescale inverse of distance such that effect size in same county is 1, and decreases as you go farther away
rownames(county_distance_weights) <-
  colnames(county_distance_weights) <-
  coordinates$GEOID

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
  path = "Data/Asthma/Transition probabilities/transition data _ weekly.xlsx", 
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

m_fire <- readRDS("Data/Fire/archive/fire_data_28apr2023.rds")

## TESTS ##
m_fire[,] <- 0
m_fire[ ,7] <- 1

m_fire.rural <- m_fire
m_fire.rural[3:5, ] <- 0

# create the No Fire matrix
m_fire.0 <- m_fire
m_fire.0[m_fire>0] <- 0



#### 3) Run Model ####

sim_no_fire <- MicroSim(n_i, n_t, 
                        m_fire = m_fire.0,
                        v_asthma_state_names, 
                        pop_sample, 
                        risk_modifiers,
                        cycle_length, 
                        baseline_birth_rate, annual_birth_rate_change, 
                        annual_allcause_mortality_change,
                        v_asthma_therapies,
                        m_asthma_therapy_probs, 
                        v_asthma_costs,
                        v_intervention_costs,
                        counties,
                        county_distance_weights, 
                        intervention_coverage = 0, 
                        intervention_trigger = 0, 
                        discount_rate_costs,
                        discount_rate_qalys,
                        seed = 12345,
                        logger = FALSE)



# sim_fire_noIntervention <- MicroSim(v_init_asthma_states,
#                                     n_i, n_t,
#                                     m_fire=m_fire,
#                                     v_asthma_state_names,
#                                     m_individual_features,
#                                     cycle_length,
#                                     baseline_birth_rate, annual_birth_rate_change,
#                                     annual_allcause_mortality_change,
#                                     discount_rate_costs, discount_rate_qalys,
#                                     intervention=0,
#                                     intervention_trigger=0,
#                                     seed = 12345,
#                                     debug = FALSE)
# 
# 
# sim_fire_universalIntervention <- MicroSim(v_init_asthma_states,
#                                            n_i, n_t,
#                                            m_fire=m_fire,
#                                            v_asthma_state_names,
#                                            m_individual_features,
#                                            cycle_length,
#                                            baseline_birth_rate, annual_birth_rate_change,
#                                            annual_allcause_mortality_change,
#                                            discount_rate_costs, discount_rate_qalys,
#                                            intervention=1,
#                                            intervention_trigger=1,
#                                            seed = 12345,
#                                            debug = FALSE)
# 
# 
# sim_fire_50percentIntervention <- MicroSim(v_init_asthma_states,
#                                            n_i, n_t,
#                                            m_fire=m_fire,
#                                            v_asthma_state_names,
#                                            m_individual_features,
#                                            cycle_length,
#                                            baseline_birth_rate, annual_birth_rate_change,
#                                            annual_allcause_mortality_change,
#                                            discount_rate_costs, discount_rate_qalys,
#                                            intervention=0.5,
#                                            intervention_trigger=1,
#                                            seed = 12345,
#                                            debug = FALSE)
# 
# 
# 
# sim_ruralfire_universalIntervention <- MicroSim(v_init_asthma_states,
#                                                 n_i, n_t,
#                                                 m_fire=m_fire.rural,
#                                                 v_asthma_state_names,
#                                                 m_individual_features,
#                                                 cycle_length,
#                                                 baseline_birth_rate, annual_birth_rate_change,
#                                                 annual_allcause_mortality_change,
#                                                 discount_rate_costs, discount_rate_qalys,
#                                                 intervention=1,
#                                                 intervention_trigger=1,
#                                                 seed = 12345,
#                                                 debug = FALSE)
# 
# 
# sim_ruralfire_50percentIntervention <- MicroSim(v_init_asthma_states,
#                                                 n_i, n_t,
#                                                 m_fire=m_fire.rural,
#                                                 v_asthma_state_names,
#                                                 m_individual_features,
#                                                 cycle_length,
#                                                 baseline_birth_rate, annual_birth_rate_change,
#                                                 annual_allcause_mortality_change,
#                                                 discount_rate_costs, discount_rate_qalys,
#                                                 intervention=0.5,
#                                                 intervention_trigger=1,
#                                                 seed = 12345,
#                                                 debug = FALSE)
# 
# 
# 
# 
# 
# 
