###################### Climate - Health Modeling v2.0 ########################

##################
rm(list = ls()) # remove any variables in R's memory

# call functions
source("MicroSim.R")
source("Probs.R")
source("Costs.R")
source("Effs.R")
source("modifyRisk.R")
source("multisheet2array.R")
##################


#### 1) Population Inputs #### 

n.i <- 5000                     # number of individuals
n.t <- 52/2                     # time horizon (in cycles)
cl <- 2/52                      # length of each cycle (in years)
v.n_asthma <- c("0",   # No asthma 
                "1",   # Asthma - controlled
                "21",  # Asthma exacerbation - oral corticosteroid (OCS) ONLY
                "22",  # Asthma exacerbation - ED visit ONLY
                "212", # Asthma exacerbation - OCS + ED visit
                "223", # Asthma exacerbation - ED + hospitalization
                "50",  # Dead - asthma
                "100"  # Dead - other cause
)

# add asthma with inhaled therapy?
 
n.s_asthma <- length(v.n_asthma)               # number of health states
initStates <- sample(v.n_asthma, size = n.i, 
                     prob = c(0.902, 0.09, 0.0053, 0.0008, 0.001, 0.0009, 0.000, 0.0000), replace=TRUE)   # determine mix of starting states
d.c <- d.e <- 0.0              # 3% discount rate for both costs and QALYs (0 for biweekly cycles up to a year)
v.intn <- c("No intervention", "Distribute air filter")    # strategy names
# add an intervention at a non-0 cycle

birthRate_bl <- (52.4/1000) * cl   # convert into rate for cycle length
birthRate_change <- 0
  # -0.035 * cl

allCauseMortality_bl <- (583.1/100000) * cl  
allCauseMortality_change <- 0
  # (-2.2/100) * cl

# Individual characteristics

m.x <- data.frame(matrix(nrow=n.i, ncol=0))  # create matrix that will store individual characteristics
m.x$id <- seq(1,n.i,by=1)          # assign ids
m.x$age <- floor(rnorm(n.i,48,12))  # determine each individual's age based on a normal dist'n with mean=48 and sd=12
m.x$sex <- rbinom(n.i,1,0.55)      # determine each individual's sex based on a binomial dist'n with 0=male, 1=female, and p(female)=0.55
m.x$exposure <- FALSE              # no previous exposure at the beginning
m.x$rural <- rbinom(n.i, 1, 0.30)  # determine each individual's residence based on a binomial dist'n with 0=urban, 1=rural, and p(rural)=0.30


# assign neighborhood based on rural/urban status
# Note: this may be a very roundabout way of doing this. Might have made more sense to first give everyone a neighborhood based on neighborhood density
# (i.e., probability of individual i to be in neighborhood k is based on what % of total population lives in k) but this also works. 

ruralNeighborhoods <- c(1111,2222)
probsRural <- c(0.65, 1-0.65)
urbanNeighborhoods <- c(3333,4444,5555)
probsUrban <- c(0.35,0.45,1-0.35-0.45)
allNeighborhoods <- c(ruralNeighborhoods, urbanNeighborhoods)

for (k in 1:nrow(m.x[m.x$rural==1, ])) {
  m.x$neighborhood[m.x$rural==1][k] <-  sample(ruralNeighborhoods,size=1,prob=probsRural)
}

for (k in 1:nrow(m.x[m.x$rural==0, ])) {
  m.x$neighborhood[m.x$rural==0][k] <-  sample(urbanNeighborhoods,size=1,prob=probsUrban)
}

n.neighborhood <- length(allNeighborhoods)

# Create neighborhood map (the distance in miles between neighborhoods)
# This will determine how much smoke neighboring communities will be exposed to if there is a fire outside of their neighborhood

neighborhood.map <- matrix(data= 0, nrow=n.neighborhood, ncol=n.neighborhood, dimnames=(list(paste(allNeighborhoods), paste(allNeighborhoods))))
neighborhood.map[1, ] <-  c(0, 0.5, 1, 3, 5)
neighborhood.map[2, ] <-  c(0.5, 0, 1, 6, 4)
neighborhood.map[3, ] <-  c(1,   1, 0, 8, 10)
neighborhood.map[4, ] <-  c(3,   6, 8, 0, 3)
neighborhood.map[5, ] <-  c(5,   4,10, 3, 0)

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


# m.x$intervention <-
#   1  # Universal coverage
  # rbinom(n.i, 1, 0.5)  # 50% coverage
  # ifelse(m.x$neighborhood==1111 | m.x$neighborhood==3333, 1, 0)   # Coverage for only selected villages

# Transition probabilities and risk modifiers (stored in a 3-dimensional array with risk factors along the z-axis)

riskmodifiers <- multisheet2array(path = "C:/Users/smaya/Box/AA_Sigal_Documents/Climate - Health/Modeling/transition data.xlsx", x_names = v.n_asthma, y_names = v.n_asthma)


# Cost and utility inputs

v.costs <- c(0,  # 0
             0,  # 1
             100,  # 21
             1000,  # 22
             5000,  # 212
             10000,  # 223
             0, # 50
             0)  # 100
interventionCost <- 20   # cost per person per cycle

v.utilities <- c(1,  # 0
                 1,  # 1
                 0.9,  # 21
                 0.8,  # 22
                 0.6,  # 212
                 0.5,  # 223
                 0, # 50
                 0)  # 100


#### 2) Climate Data ####

m.fire <- readRDS("fire_data.RDS")

## TESTS ##
m.fire[,] <- 0
m.fire[ ,7] <- 1

# create the No Fire matrix
m.fire.0 <- m.fire
m.fire.0[m.fire>0] <- 0


#### 3) Run Model ####

sim_no_fire <- MicroSim(initStates, 
                        n.i, n.t, 
                        m.fire=m.fire.0,
                        v.n_asthma, 
                        m.x, 
                        cl, 
                        birthRate_bl, birthRate_change, 
                        allCauseMortality_bl, allCauseMortality_change,
                        d.c, d.e, 
                        intervention=FALSE, 
                        seed = 1,
                        debug = FALSE)


sim_fire_noIntervention <- MicroSim(initStates,
                n.i, n.t,
                m.fire=m.fire,
                v.n_asthma,
                m.x,
                cl,
                birthRate_bl, birthRate_change,
                allCauseMortality_bl, allCauseMortality_change,
                d.c, d.e,
                intervention=FALSE,
                seed = 1,
                debug = FALSE)


sim_fire_Intervention <- MicroSim(initStates,
                                    n.i, n.t,
                                    m.fire=m.fire,
                                    v.n_asthma,
                                    m.x,
                                    cl,
                                    birthRate_bl, birthRate_change,
                                    allCauseMortality_bl, allCauseMortality_change,
                                    d.c, d.e,
                                    intervention=TRUE,
                                    seed = 1,
                                    debug = FALSE)

# do a version of the graph that is a single fire, one in a dense population, one in smaller neighborhood etc
# do one highly effective intervention, one less effective 


