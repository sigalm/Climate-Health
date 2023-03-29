###################### Climate - Health Modeling v3.0 ########################

##################
rm(list = ls()) # remove any variables in R's memory

# call functions
source("Sim Functions/generatePopulation.R")
source("Sim Functions/MicroSim_20230104.R")
source("Sim Functions/Probs.R")
source("Sim Functions/Costs.R")
source("Sim Functions/Effs.R")
source("Sim Functions/modifyRisk.R")
source("Sim Functions/multisheet2array.R")
library(readxl)
##################


#### 1) Population Inputs ####
 #### Structural parameters ####

n.i <-2000              # number of individuals
n.t <- 10               # time horizon (in cycles)
cl <- 2/52              # length of each cycle (in years)

# list of health states in model:

v.n_asthma <- c("0",   # No asthma 
                "1",   # Complete control
                "2",   # Well control
                "3",   # Somewhat control
                "4",   # Poor control
                "5",   # No control at all
                "50",  # Dead - asthma
                "100"  # Dead - other cause
)

# starting probabilities of having each health state:

initProbs <- c(0.902,  # No asthma
               0.03,   # Complete control
               0.0253, # Well control
               0.0158, # Somewhat control
               0.016,  # Poor control
               0.0109, # No control at all
               0.000,  # Dead - asthma
               0.000)  # Dead - other cause


n.s_asthma <- length(v.n_asthma)      # save number of health states

# determine mix of starting health states:

set.seed(10)
initStates <- sample(v.n_asthma, size = n.i, 
                     prob = initProbs, replace=TRUE)

d.c <- d.e <- 0.0              # 3% discount rate for both costs and QALYs (0 for biweekly cycles up to a year)

v.intn <- c("No intervention", "Distribute air filter")    # intervention names (non-medical interventions)
# add an intervention at a non-0 cycle

v.therapy <- c("NoTx","S1", "S2","S3","S4","S5","OCS")   # continuous therapies / asthma management 
v.acute <- c("ocs","ugt","ed","hosp")       # acute exacerbations - outcomes


# determine mix of therapies individuals are on when entering simulation

# get therapy matrix
therapyMatrix <- as.data.frame(read_xlsx("Data/Asthma/Therapies/asthmaTherapyMatrix.xlsx", range=("B2:H9"),
                           col_names = v.therapy))
row.names(therapyMatrix) <- v.n_asthma



birthRate_bl <-  (52.4/1000) * cl   # convert annual birth rate into rate for cycle length
birthRate_change <- -0.035 * cl             # annual change in birth rate - 0 for up to a year 

allCauseMortality_bl <- (583.1/100000) * cl  # convert annual birth rate into rate for cycle length (NOT IN USE - mortality is calculated within the transition matrix)
allCauseMortality_change <- 0                # annual change in all cause mortality rate - 0 for up to a year
# (-2.2/100) * cl

#### Individual characteristics ####

m.x <- data.frame(matrix(nrow=n.i, ncol=0))  # create matrix that will store individual characteristics
m.x$id <- seq(1,n.i,by=1)                    # assign ids
set.seed(20)
m.x$age <- rnorm(n.i,48,12)           # determine each individual's age based on a normal dist'n with mean=48 and sd=12
set.seed(30)
m.x$sex <- rbinom(n.i,1,0.55)                # determine each individual's sex based on a binomial dist'n with 0=male, 1=female, and p(female)=0.55
m.x$exposure <- FALSE                        # no previous exposure at the beginning
set.seed(40)
m.x$rural <- rbinom(n.i, 1, 0.30)            # determine each individual's residence based on a binomial dist'n with 0=urban, 1=rural, and p(rural)=0.30

for (i in 1:n.i) {
  m.x$asthmaCare[i] <- 
    sample(v.therapy, size=1, prob=therapyMatrix[as.character(initStates[i]), ])
}

# assign neighborhood based on rural/urban status
# Note: this may be a very roundabout way of doing this. Might have made more sense to first give everyone a neighborhood based on neighborhood density
# (i.e., probability of individual i to be in neighborhood k is based on what % of total population lives in k) but this also works. 

ruralNeighborhoods <- c(1111,2222)       # vector of rural neighborhoods
probsRural <- c(0.65, 1-0.65)            # probability of being in either rural neighborhood
urbanNeighborhoods <- c(3333,4444,5555)  # vector of urban neighborhoods
probsUrban <- c(0.35,0.45,1-0.35-0.45)   # probability of being in one of urban neighborhoods
allNeighborhoods <- c(ruralNeighborhoods, urbanNeighborhoods)  # vector of all neighborhoods

# cycle through all rural individuals to assign one of two rural neighborhoods
for (k in 1:nrow(m.x[m.x$rural==1, ])) {
  m.x$neighborhood[m.x$rural==1][k] <-  sample(ruralNeighborhoods,size=1,prob=probsRural)
}

# repeat for urban individuals
for (k in 1:nrow(m.x[m.x$rural==0, ])) {
  m.x$neighborhood[m.x$rural==0][k] <-  sample(urbanNeighborhoods,size=1,prob=probsUrban)
}

n.neighborhood <- length(allNeighborhoods) # save total number of neighborhoods

# Create neighborhood map (the distance in miles between neighborhoods)
# This will determine how much smoke neighboring communities will be exposed to if there is a fire outside of their neighborhood

neighborhood.map <- matrix(data= 0, nrow=n.neighborhood, ncol=n.neighborhood, dimnames=(list(paste(allNeighborhoods), paste(allNeighborhoods))))
neighborhood.map[1, ] <-  c(0, 0.5, 1, 3, 5)
neighborhood.map[2, ] <-  c(0.5, 0, 1, 6, 4)
neighborhood.map[3, ] <-  c(1,   1, 0, 8, 10)
neighborhood.map[4, ] <-  c(3,   6, 8, 0, 3)
neighborhood.map[5, ] <-  c(5,   4,10, 3, 0)

# Rescale neighborhood map to contain relative effect size of distance to fire. 
# e.g., if fire happened in 1111, the effect size is 1, and decreases as you go farther away

neighborhood.effect.size <- scales::rescale(1/(neighborhood.map+1))

# set household ID based on neighborhood (household aren't used for now, but will be important for other conditions)
# mean household size is 4

m.x$fam_id <- NA
meanFamSize <- 4
fam_count <- 1
for (k in allNeighborhoods) {
  neighborhoodSize <- nrow(m.x[m.x$neighborhood==k, ])
  familySize <- ceiling(neighborhoodSize/meanFamSize)
  m.x$fam_id[m.x$neighborhood==k] <- sample(fam_count:(fam_count+familySize), size = neighborhoodSize, replace=TRUE)
  fam_count <- fam_count+familySize+1
}


#### Transition probabilities ####

# Transition probabilities and risk modifiers (stored in a 3-dimensional array with risk factors along the z-axis)

riskmodifiers <- multisheet2array(path = "Data/Asthma/Transition probabilities/transition data.xlsx", range=("B1:I9"), x_names = v.n_asthma, y_names = v.n_asthma)


#### Cost and utility inputs ####

v.costs <- c(0,     # 0 (no asthma)
             0,     # 1 (complete control)
             100,   # 2 (well control)
             1000,  # 3 (somewhat control)
             5000,  # 4 (poor control)
             10000, # 5 (no control at all)
             0,     # 50 (dead- asthma)
             0)     # 100 (dead - other cause)

interventionCost <- 20   # cost of intervention per person per cycle

v.utilities <- c(1,    # 0 (no asthma)
                 1,    # 1 (complete control)
                 0.9,  # 2 (well control)
                 0.8,  # 3 (somewhat control)
                 0.6,  # 4 (poor control)
                 0.5,  # 5 (no control at all)
                 0,    # 50 (dead - asthma)
                 0)    # 100 (dead - other cause)


#### 2) Climate Data ####

m.fire <- readRDS("Data/Fire/fire_data_oct10.RDS")

## TESTS ##
m.fire[,] <- 0
m.fire[ ,7] <- 1

m.fire.rural <- m.fire
m.fire.rural[3:5, ] <- 0

# create the No Fire matrix
m.fire.0 <- m.fire
m.fire.0[m.fire>0] <- 0


#### 3) Run Model ####

sim_no_fire <- MicroSim(initStates, 
                        n.i, n.t, 
                        m.fire=m.fire.0,
                        v.n_asthma, 
                        m.x,
                        riskmodifiers,
                        cl, 
                        birthRate_bl, birthRate_change, 
                        allCauseMortality_change,
                        d.c, d.e, 
                        intervention=0,
                        intervention_trigger=0,
                        seed = 12345,
                        debug = TRUE)


sim_fire_noIntervention <- MicroSim(initStates,
                                    n.i, n.t,
                                    m.fire=m.fire,
                                    v.n_asthma,
                                    m.x,
                                    cl,
                                    birthRate_bl, birthRate_change,
                                    allCauseMortality_change,
                                    d.c, d.e,
                                    intervention=0,
                                    intervention_trigger=0,
                                    seed = 12345,
                                    debug = FALSE)


sim_fire_universalIntervention <- MicroSim(initStates,
                                  n.i, n.t,
                                  m.fire=m.fire,
                                  v.n_asthma,
                                  m.x,
                                  cl,
                                  birthRate_bl, birthRate_change,
                                  allCauseMortality_change,
                                  d.c, d.e,
                                  intervention=1,
                                  intervention_trigger=1,
                                  seed = 12345,
                                  debug = FALSE)


sim_fire_50percentIntervention <- MicroSim(initStates,
                                           n.i, n.t,
                                           m.fire=m.fire,
                                           v.n_asthma,
                                           m.x,
                                           cl,
                                           birthRate_bl, birthRate_change,
                                           allCauseMortality_change,
                                           d.c, d.e,
                                           intervention=0.5,
                                           intervention_trigger=1,
                                           seed = 12345,
                                           debug = FALSE)



sim_ruralfire_universalIntervention <- MicroSim(initStates,
                                           n.i, n.t,
                                           m.fire=m.fire.rural,
                                           v.n_asthma,
                                           m.x,
                                           cl,
                                           birthRate_bl, birthRate_change,
                                           allCauseMortality_change,
                                           d.c, d.e,
                                           intervention=1,
                                           intervention_trigger=1,
                                           seed = 12345,
                                           debug = FALSE)


sim_ruralfire_50percentIntervention <- MicroSim(initStates,
                                                n.i, n.t,
                                                m.fire=m.fire.rural,
                                                v.n_asthma,
                                                m.x,
                                                cl,
                                                birthRate_bl, birthRate_change,
                                                allCauseMortality_change,
                                                d.c, d.e,
                                                intervention=0.5,
                                                intervention_trigger=1,
                                                seed = 12345,
                                                debug = FALSE)






