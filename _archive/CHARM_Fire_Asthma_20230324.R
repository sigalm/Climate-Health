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
##################


#### 1) Population Inputs ####
#### Structural parameters ####

n_i <- 2000                           # number of individuals
n_t <- 18                             # time horizon (cycles)
cycle_length <- 2/52                  # length of each cycle (in years)

# list of health states in model:

v_asthma_state_names <- c("0",        # No asthma 
                          "1",        # Complete control
                          "2",        # Well control
                          "3",        # Somewhat control
                          "4",        # Poor control
                          "5",        # No control at all
                          "50",       # Dead - asthma
                          "100"       # Dead - other cause
)

# starting probabilities of having each health state:

v_init_asthma_probs <- c(0.907,       # No asthma
                         0.008,       # Complete control
                         0.036,       # Well control
                         0.027,       # Somewhat control
                         0.018,       # Poor control
                         0.004,       # No control at all
                         0.000,       # Dead - asthma
                         0.000)       # Dead - other cause

n_asthma_states <- length(v_asthma_state_names)                                  # save number of health states

# determine mix of starting health states:

set.seed(277)
v_init_asthma_states <- sample(v_asthma_state_names, size = n_i, 
                               prob = v_init_asthma_probs, replace=TRUE)

discount_rate_costs <- discount_rate_qalys <- 0.0                                # 3% discount rate for both costs and QALYs (0 for biweekly cycycle_lengthes up to a year)

v_interventions <- c("No intervention", "Distribute air filter")                 # intervention names (non-medical interventions)

v_asthma_therapies <- c("NoTx", "S1", "S2", "S3", "S4", "S5", "OCS")             # continuous therapies / asthma management 

v_healthcare_use <- c("none", "ocs","ugt","ed","hosp")                           # acute exacerbations - outcomes


# determine mix of therapies individuals are on when entering simulation

# get therapy probabilities
m_asthma_therapy_probs <- as.data.frame(read_xlsx("Data/Asthma/Therapies/asthmaTherapyMatrix.xlsx", range=("B2:H9"),
                                                  col_names = v_asthma_therapies))
row.names(m_asthma_therapy_probs) <- v_asthma_state_names

m_asthma_healthcare_use_probs <- read.csv("Data/Asthma/Healthcare use.csv", row.names = 1, stringsAsFactors = FALSE)
m_asthma_healthcare_use_probs <- m_asthma_healthcare_use_probs[ ,-ncol(m_asthma_healthcare_use_probs)]


baseline_birth_rate <- 0
# (52.4/1000) * cycle_length   # convert annual birth rate into rate for cycle length
annual_birth_rate_change <- 0               # annual change in birth rate - 0 for up to a year
# -0.035 * cycle_length

annual_allcause_mortality_change <- 0                                            # annual change in all cause mortality rate - 0 for up to a year
# (-2.2/100) * cycle_length


#### Individual characteristics ####

m_individual_features <- data.frame(matrix(nrow=n_i, ncol=0))                    # create matrix that will store individual characteristics
m_individual_features$id <- seq(1,n_i,by=1)                                      # assign ids
set.seed(20)
m_individual_features$age <- rnorm(n_i,48,12)                                    # determine each individual's age based on a normal dist'n with mean=48 and sd=12
set.seed(30)
m_individual_features$sex <- rbinom(n_i,1,0.55)                                  # determine each individual's sex based on a binomial dist'n with 0=male, 1=female, and p(female)=0.55
m_individual_features$prior_exposure <- FALSE                                    # no previous exposure at the beginning
set.seed(40)
m_individual_features$rural <- rbinom(n_i, 1, 0.30)                              # determine each individual's residence based on a binomial dist'n with 0=urban, 1=rural, and p(rural)=0.30

for (i in 1:n_i) {
  m_individual_features$asthmaCare[i] <- 
    sample(v_asthma_therapies, size=1, prob=m_asthma_therapy_probs[as.character(v_init_asthma_states[i]), ])
}

# assign neighborhood based on rural/urban status
# Note: this may be a very roundabout way of doing this. Might have made more 
# sense to first give everyone a neighborhood based on neighborhood density
# (i.e., probability of individual i to be in neighborhood k is based on 
# what % of total population lives in k) but this also works. 

v_rural_neighborhoods <- c(1111,2222)                                            # vector of rural neighborhoods
v_prob_rural <- c(0.65, 1-0.65)                                                  # probability of being in either rural neighborhood
v_urban_neighborhoods <- c(3333,4444,5555)                                       # vector of urban neighborhoods
v_prob_urban <- c(0.35,0.45,1-0.35-0.45)                                         # probability of being in one of urban neighborhoods
v_all_neighborhoods <- c(v_rural_neighborhoods, v_urban_neighborhoods)           # vector of all neighborhoods

# cycle through all rural individuals to assign one of two rural neighborhoods
for (k in 1:nrow(m_individual_features[m_individual_features$rural==1, ])) {
  m_individual_features$neighborhood[m_individual_features$rural==1][k] <-  sample(v_rural_neighborhoods,size=1,prob=v_prob_rural)
}

# repeat for urban individuals
for (k in 1:nrow(m_individual_features[m_individual_features$rural==0, ])) {
  m_individual_features$neighborhood[m_individual_features$rural==0][k] <-  sample(v_urban_neighborhoods,size=1,prob=v_prob_urban)
}

n_neighborhoods <- length(v_all_neighborhoods) # save total number of neighborhoods

# Create neighborhood map (the distance in miles between neighborhoods)
# This will determine how much smoke neighboring communities will be exposed to if there is a fire outside of their neighborhood

neighborhood_map <- matrix(data= 0, nrow=n_neighborhoods, ncol=n_neighborhoods, 
                           dimnames=(list(paste(v_all_neighborhoods), 
                                          paste(v_all_neighborhoods))))
neighborhood_map[1, ] <-  c(0, 0.5, 1, 3, 5)
neighborhood_map[2, ] <-  c(0.5, 0, 1, 6, 4)
neighborhood_map[3, ] <-  c(1,   1, 0, 8, 10)
neighborhood_map[4, ] <-  c(3,   6, 8, 0, 3)
neighborhood_map[5, ] <-  c(5,   4,10, 3, 0)

# Rescale neighborhood map to contain relative effect size of distance to fire. 
# e.g., if fire happened in 1111, the effect size is 1, and decreases 
# as you go farther away

neighborhood_effect_size <- scales::rescale(1/(neighborhood_map+1))

# set household ID based on neighborhood (household aren't used for now, 
# but will be important for other conditions)
# mean household size is 4

m_individual_features$fam_id <- NA
mean_fam_size <- 4
fam_count <- 1
for (k in v_all_neighborhoods) {
  neighborhood_size <- 
    nrow(m_individual_features[m_individual_features$neighborhood==1111, ])
  family_size <- ceiling(neighborhood_size/mean_fam_size)
  m_individual_features$fam_id[m_individual_features$neighborhood==1111] <- 
    sample(fam_count:(fam_count+family_size), size = neighborhood_size, 
           replace=TRUE)
  fam_count <- fam_count+family_size+1
}


#### Transition probabilities ####

# Transition probabilities and risk modifiers (stored in a 3-dimensional array with risk factors along the z-axis)

risk_modifiers <- multisheet2array(
  path = "Data/Asthma/Transition probabilities/transition data _ calibrated.xlsx", 
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

m_fire <- readRDS("Data/Fire/fire_data_oct10.RDS")

## TESTS ##
m_fire[,] <- 0
m_fire[ ,7] <- 1

m_fire.rural <- m_fire
m_fire.rural[3:5, ] <- 0

# create the No Fire matrix
m_fire.0 <- m_fire
m_fire.0[m_fire>0] <- 0


#### 3) Run Model ####

sim_no_fire <- MicroSim(v_init_asthma_states, 
                        n_i, n_t, 
                        m_fire = m_fire.0,
                        v_asthma_state_names, 
                        m_individual_features, 
                        risk_modifiers,
                        cycle_length, 
                        baseline_birth_rate, annual_birth_rate_change, 
                        annual_allcause_mortality_change,
                        v_asthma_therapies,
                        m_asthma_therapy_probs, 
                        v_asthma_costs,
                        v_intervention_costs,
                        v_all_neighborhoods,
                        neighborhood_effect_size, 
                        intervention_coverage = 0, 
                        intervention_trigger = 0, 
                        discount_rate_costs,
                        discount_rate_qalys,
                        seed = 12345,
                        logger = FALSE)



sim_fire_noIntervention <- MicroSim(v_init_asthma_states,
                                    n_i, n_t,
                                    m_fire=m_fire,
                                    v_asthma_state_names,
                                    m_individual_features,
                                    cycle_length,
                                    baseline_birth_rate, annual_birth_rate_change,
                                    annual_allcause_mortality_change,
                                    discount_rate_costs, discount_rate_qalys,
                                    intervention=0,
                                    intervention_trigger=0,
                                    seed = 12345,
                                    debug = FALSE)


sim_fire_universalIntervention <- MicroSim(v_init_asthma_states,
                                           n_i, n_t,
                                           m_fire=m_fire,
                                           v_asthma_state_names,
                                           m_individual_features,
                                           cycle_length,
                                           baseline_birth_rate, annual_birth_rate_change,
                                           annual_allcause_mortality_change,
                                           discount_rate_costs, discount_rate_qalys,
                                           intervention=1,
                                           intervention_trigger=1,
                                           seed = 12345,
                                           debug = FALSE)


sim_fire_50percentIntervention <- MicroSim(v_init_asthma_states,
                                           n_i, n_t,
                                           m_fire=m_fire,
                                           v_asthma_state_names,
                                           m_individual_features,
                                           cycle_length,
                                           baseline_birth_rate, annual_birth_rate_change,
                                           annual_allcause_mortality_change,
                                           discount_rate_costs, discount_rate_qalys,
                                           intervention=0.5,
                                           intervention_trigger=1,
                                           seed = 12345,
                                           debug = FALSE)



sim_ruralfire_universalIntervention <- MicroSim(v_init_asthma_states,
                                                n_i, n_t,
                                                m_fire=m_fire.rural,
                                                v_asthma_state_names,
                                                m_individual_features,
                                                cycle_length,
                                                baseline_birth_rate, annual_birth_rate_change,
                                                annual_allcause_mortality_change,
                                                discount_rate_costs, discount_rate_qalys,
                                                intervention=1,
                                                intervention_trigger=1,
                                                seed = 12345,
                                                debug = FALSE)


sim_ruralfire_50percentIntervention <- MicroSim(v_init_asthma_states,
                                                n_i, n_t,
                                                m_fire=m_fire.rural,
                                                v_asthma_state_names,
                                                m_individual_features,
                                                cycle_length,
                                                baseline_birth_rate, annual_birth_rate_change,
                                                annual_allcause_mortality_change,
                                                discount_rate_costs, discount_rate_qalys,
                                                intervention=0.5,
                                                intervention_trigger=1,
                                                seed = 12345,
                                                debug = FALSE)






