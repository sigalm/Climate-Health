set.seed(12345)
# library(dplyr)

#### Model inputs ####

n.i <- 1000                      # number of individuals
n.t <- 20                        # time horizon
cl <- 1                          # length of each cycle (year)
v.n <- c("H", "Act", "Prm", "D") # possible health states: Healthy, Acute injury, Permanent injury, Dead
n.s <- length(v.n)               # number of health states
initStates <- rep(v.n[1], n.i)   # everyone begins in the Healthy state
d.c <- d.e <- 0.03               # 3% discount rate for both costs and QALYs
v.intn <- c("No prescribed burning", "Prescribed burning")    # strategy names

birthRate_bl <- 52.4/1000
birthRate_change <- -0.035

allCauseMortality_bl <- 583.1/100000
allCauseMortality_change <- -2.2/100

##### Transition probabilities per cycle #####

p.fire <- 0.30                   # probability that a wildfire occurs 
rr.fire.urban <- 0.25            # risk ratio for fire in urban area versus rural
p.HD <- 0.03                     # probability to die when healthy
p.HAct <- 0.1                    # probability of acute injury when healthy
p.ActH <- 0.75                   # probability to become healthy when acutely injured
p.ActPrm <- 0.08                 # probability to become permanently injured when acutely injured
rr.Act <- 1.25                   # rate ratio of death when acutely injured vs healthy
rr.Prm <- 2                      # rate ratio of death when permanently injured vs healthy
r.HD <- -log(1-p.HD)             # rate of death when healthy
r.ActD <- rr.Act*r.HD            # rate of death when acutely injured
r.PrmD <- rr.Prm*r.HD            # rate of death when permanently injured
p.ActD <- 1-exp(-r.ActD)         # probability of death when acutely injured
p.PrmD <- 1-exp(-r.PrmD)         # probability of death when permanently injured
rp.smoke <- 0.05                 # increase of mortality rate for all with history of smoke exposure conditional on duration of exposure

rr.HD.f <- 1.2                   # risk ratio for healthy -> dead for females
rr.HAct.f <- 1.4                 # risk ratio for healthy -> acute injury for females
rr.HD.65 <- 1.3                  # risk ratio for healthy -> dead for those aged 65 or older
rr.HAct.65 <- 1.5                # risk ratio for healthy -> acute injury for those aged 65 or older

rr.fireOverTime <- 1.05          # additional fire risk each consecutive year


##### Cost and utility inputs per cycle #####

c.H <- 0                         # cost of remaining healthy
c.Act <- 10000                   # cost of acute injury
c.Prm <- 20000                   # cost of permanent injury
c.intn <- 5000                   # cost of intervention
c.fire <- 15000                  # cost of fire occurring e.g., infrastructure damage, rebuilding homes

u.H <- 1                         # utility when healthy
u.Act <- 0.75                    # utility when acutely injured
u.Prm <- 0.65                    # utility when permanently injured
u.intn <- u.Act                  # utility with intervention (in this case, intervention reduces fire risk and doesn't affect utility)
ru.ActPrm <- 0                   # decrease in utility with every additional year being injured (assume same for now)

##### Individual characteristics #####

m.x <- data.frame(matrix(nrow=n.i, ncol=0))  # create matrix that will store individual characteristics
m.x$id <- seq(1,n.i,by=1)          # assign ids
m.x$age <- floor(rnorm(n.i,48,5))  # determine each individual's age based on a normal dist'n with mean=48 and sd=5
m.x$sex <- rbinom(n.i,1,0.55)      # determine each individual's sex based on a binomial dist'n with 0=male, 1=female, and p(female)=0.55
m.x$rural <- rbinom(n.i, 1, 0.30)  # determine each individual's residence based on a binomial dist'n with 0=urban, 1=rural, and p(rural)=0.30
m.x$exposure <- FALSE              # no previous exposure at the beginning

# assign neighborhood based on rural/urban status
# Note: this may be a very roundabout way of doing this. Might have made more sense to first give everyone a neighborhood based on neighborhood density
# (i.e., probability of individual i to be in neighborhood k is based on what % of total population lives in k) but this also works. 

ruralNeighborhoods <- c("A","B")
probsRural <- c(0.65, 1-0.65)
urbanNeighborhoods <- c("C","D","E")
probsUrban <- c(0.35,0.45,1-0.35-0.45)
allNeighborhoods <- c(ruralNeighborhoods, urbanNeighborhoods)

for (k in 1:nrow(m.x[m.x$rural==1, ])) {
  m.x$neighborhood[m.x$rural==1][k] <-  sample(ruralNeighborhoods,size=1,prob=probsRural)
}

for (k in 1:nrow(m.x[m.x$rural==0, ])) {
  m.x$neighborhood[m.x$rural==0][k] <-  sample(urbanNeighborhoods,size=1,prob=probsUrban)
}

m.x$neighborhood <- as.factor(m.x$neighborhood)
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
for (k in levels(m.x$neighborhood)) {
  neighborhoodSize <- nrow(m.x[m.x$neighborhood==k, ])
  familySize <- ceiling(neighborhoodSize/meanFamSize)
  m.x$fam_id[m.x$neighborhood==k] <- sample(fam_count:(fam_count+familySize), size = neighborhoodSize, replace=TRUE)
  fam_count <- fam_count+familySize+1
}



# Get year over year fire data per neighborhood (or another geographic granularity) and air quality data - since no data yet, create matrix

m.fire <- matrix(nrow=n.neighborhood, ncol=n.t, dimnames = list(paste(allNeighborhoods), paste("cycle",1:n.t)))
m.smoke.duration <- matrix(data = 0, nrow=n.neighborhood, ncol=n.t, dimnames= list(paste(allNeighborhoods), paste("cycle",1:n.t)))
fireIntensity <- 1

for (t in 1:n.t) {
  p.fire.tmp <- min(p.fire*rr.fireOverTime^(t-1), 1)                     # calculate fire risk for year t
  fireIntensity.tmp <- fireIntensity * rr.fireOverTime^(t-1)           # calculate average fire intensity for year t (analogous to fire duration for simplicity)
  
  for (k in 1:n.neighborhood) {
    currentNeighborhood <- allNeighborhoods[k]                           # get neighborhood name
    m.fire[k, t] <- rbinom(1,size=1,prob=(
      (currentNeighborhood %in% ruralNeighborhoods)*p.fire.tmp +
        (currentNeighborhood %in% urbanNeighborhoods)*min(p.fire.tmp*rr.fire.urban,1)))        # calculate whether fire happened given fire risk and urban/rural designation
    m.smoke.duration[k, t] <- rnorm(1, mean=fireIntensity.tmp, sd = fireIntensity.tmp/2)*m.fire[k, t]    # calculate number of smoky days for neighborhood k in year t due to fire in neighborhood
    m.smoke.duration[-k ,t] <- m.smoke.duration[-k, t] + m.smoke.duration[k, t]/neighborhood.map[k, -k]  # calculate ADDITIONAL number of smoky days for neighboring communities given fire in and distance to neighborhood k 
  }
}


# calculate cumulative smoke exposure days over t years

m.smoke.duration.cum <- m.smoke.duration 
for (t in 1:(n.t-1)) {
  m.smoke.duration.cum[ , t+1] <- m.smoke.duration.cum[ ,t] + m.smoke.duration[ ,t+1]
}




#### Create functions ####

MicroSim <- function(initStates,                  # vector of initial health states
                     n.i,                         # number of individuals
                     n.t,                         # number of cycles    
                     m.fire,                      # annual fire data by neighborhood
                     m.smoke.duration.cum,        # annual smoke exposure data (i.e., air pollution by neighorhood)
                     v.n,                         # vector of health state names
                     m.x,                         # matrix with individual characteristics
                     cl,                          # cycle length
                     birthRate_bl,                # baseline birthrate
                     birthRate_change,            # annual change in birth rate
                     allCauseMortality_bl,        # baseline annual all cause mortality
                     allCauseMortality_change,    # annual change in all cause mortality
                     d.c,                         # discount rate for costs
                     d.e,                         # discount rate for utilities
                     TR.out = TRUE,               # should the output include a microsimulation trace?
                     TS.out = TRUE,               # should the output include a transition array between states?
                     intn=FALSE,                  # is there an intervention? (scalar with Boolean value, could be vector of Booleans if mix of interventions)
                     seed = 1) {                  # starting seed number
  
  # You need the following functions:
  # Probs: to estimate transition probabilities
  # Costs: to estimate costs of state values
  # Effs: to estimate health outcomes
  
  v.dwc <- 1 / (1 + d.c) ^ (0:n.t)     # calculate discount weights for costs per cycle
  v.dwe <- 1 / (1 + d.e) ^ (0:n.t)     # calculate discount weights for QALYs per cycle
  
  
  # initialize matrices to capture fires, health states, costs, and health outcomes each cycle:
  
  m.M <- m.C <- m.E <- matrix(nrow=n.i, ncol=n.t+1, dimnames = list(paste("ind", 1:n.i), paste("cycle",0:n.t)))  
  
  m.M[ ,1] <- initStates    
  m.C[ ,1] <- Costs(M_it=m.M[ ,1], intervention = intn)
  m.E[ ,1] <- Effs(M_it=m.M[ ,1], intervention=intn, x_i=m.x , cl=1)
  

  for (t in 1:n.t) {
    totalBirths_t <- 0     # initialize variable to keep track of number of births (define this in the for loop because we want it to reset each cycle)
    birthRate_t <- birthRate_bl * (1-birthRate_change)^(t-1)
    deathRate_t <- allCauseMortality_bl * (1-allCauseMortality_change)^(t-1)
    
    for (i in 1:n.i) {
      set.seed(seed+i)                                                               # set seed for every individual
      neighborhood_index <- match(m.x$neighborhood[i], allNeighborhoods)
      
      # calculate the transition probability for individual i at cycle t
      
      v.p <- Probs(M_it=m.M[i,t], 
                   fire_it=m.fire[neighborhood_index, t], 
                   x_i=m.x[i, ], 
                   m.smoke.duration.cum[neighborhood_index, t]
      )
      m.M[i, t+1] <- sample(v.n, prob=v.p, size=1)                                    # sample the next health state given probability to transition
      m.C[i, t+1] <- Costs(M_it=m.M[i, t+1], intervention=intn)                       # estimate costs for individual i at cycle t+1
      m.E[i, t+1] <- Effs(M_it=m.M[i, t+1], intervention=intn, x_i=m.x[i, ], cl)      # estimate QALYs for individual i at cycle t+1
      
      m.x$age[i] <- m.x$age[i] + cl                                                   # increase age by cycle length
      
      if (m.fire[neighborhood_index, t]==1) {m.x$exposure[i] <-  TRUE}                # if there has been a fire, set exposure history to TRUE, otherwise keep the same
      
      kids_it <- rpois(1, birthRate_t)                       # determine if individual i had children and how many
      
      # if individual i had children, assign characteristics to child and add new row to m.x
      if (kids_it > 0) {
        for (kid in 1:kids_it) {
          new_x <- c(m.x[nrow(m.x), 'id'] + 1,                              # get last id and add 1
                     0,                                                     # age
                     rbinom(n.i,1,0.55),                                    # sex (0=male, 1=female)
                     m.x$rural[i],                                          # same rural/urban designation as parent
                     ifelse(m.fire[neighborhood_index, t]==1, TRUE, FALSE), # exposure is TRUE if fire happened that year
                     m.x$neighborhood[i],                                   # same neighborhood as parent
                     m.x$fam_id[i])                                         # same family as parent
          m.x <- rbind(m.x, new_x)
        }
      }
      
      totalBirths_t <- totalBirths_t + kids_it        # calculate total number of births in cycle t
      
      # if individual is not dead, determine whether they die from other causes (i.e., all cause mortality)
      
      if (m.M[i,t+1] != 'D') {
        death <- rbinom(n=1, size=1, prob=deathRate_t)
        m.M[i, t+1] <- ifelse(death==1, 'D', m.M[i, t+1])
      }
      
    }  # close loop for individuals
    
    n.i <- n.i + totalBirths_t   # increase n.i for next cycle
    
  }    # close loop for cycles
  
  tc <- m.C %*% v.dwc     # total discounted costs per individual
  te <- m.E %*% v.dwe     # total discounted QALYs per individual
  tc_hat <- mean(tc)      # average discounted costs
  te_hat <- mean(te)      # average discounted QALYS
  
  
  if (TS.out == TRUE) {                                    # create matrix of transition across states
    TS <- paste(m.M, cbind(m.M[,-1], "D"), sep = "->")     # this kills off everyone in the last cycle, which is not true. need to fix.
    TS <- matrix(TS, nrow=n.i)
    rownames(TS) <- paste("ind", 1:n.i)
    colnames(TS) <- paste("cycle", 0:n.t)
  } else {
    TS <-  NULL
  }
  
  if (TR.out == TRUE) {                                    # create the distribution trace 
    TR <- t(apply(m.M, 2, function(x) table(factor(x, levels=v.n, ordered=TRUE))))
    TR <- TR/n.i
    colnames(TR) <- v.n
    rownames(TR) <- paste("cylce", 0:n.t)
  } else {
    TR <- NULL
  }
  
  # return outputs as a list
  
  results <- list(
    m.M=m.M, 
    m.C=m.C, 
    m.E=m.E, 
    tc=tc, 
    te=te, 
    tc_hat=tc_hat, 
    te_hat=te_hat, 
    m.x=m.x, 
    TS=TS, 
    TR=TR)
  
  return(results)
  
}


# Create the probabilities function

Probs <- function(M_it,                      # health state occupied by individual i at cycle t
                  fire_it,                   # fire experience of individual i at cycle t (binary)
                  x_i,                       # individual characteristics of individual i (row i of m.x)
                  smoke.duration.cum_it) {   # cumulative smoke exposure of individual i up to cycle t
  
  
  v.p.it <- rep(NA, n.s)   # initialize vector of state transition probabilities
  names(v.p.it) <- v.n     # name columns with the health states
  
  # update v.p.it with appropriate probabilities (note: v.n is H; Act; Prm; D)
  
  p.PrmD_it <- min(1, fire_it *
                     (1-exp(-r.PrmD * (1 + rp.smoke * smoke.duration.cum_it))))     # calculate p.PrmD conditional on cumulative exposure
  
  p.HAct_it <- min(1, p.HAct * fire_it * 
                     (x_i$sex * rr.HAct.f + (1-x_i$sex)) *          # risk modification for sex
                     ifelse(x_i$age>=65, rr.HAct.65,1))                   # risk modification for age
  
  p.HD_it <- min(1, p.HD * fire_it * 
                   (x_i$sex * rr.HD.f + (1-x_i$sex)) *              # risk modification for sex
                   ifelse(x_i$age>=65, rr.HD.65,1))                       # risk modification for age
  
  v.p.it[M_it == "H"] <- c(1-p.HAct_it-p.HD_it , p.HAct_it, 0, p.HD_it)
  v.p.it[M_it == "Act"] <- c(p.ActH, 1-p.ActH-p.ActPrm-p.ActD, p.ActPrm, p.ActD)
  v.p.it[M_it == "Prm"] <- c(0, 0, 1-p.PrmD_it, p.PrmD_it)
  v.p.it[M_it == "D"] <- c(0, 0, 0, 1)
  
  ifelse(all.equal(sum(v.p.it), 1), return(v.p.it), print(c("Probabilities do not sum to 1", v.p.it, sum(v.p.it))))     # return error if probabilities do not sum to 1
}


# Create the costs function

Costs <- function(M_it,                     # M_it: health state occupied by individual i at cycle t
                  intervention=FALSE) {     # intervention: is there an intervention?
  
  
  c.it <- 0                                             # everyone starts with 0 costs
  c.it[M_it == "H"] <-  c.H                             # cost of staying healthy for one cycle
  c.it[M_it == "Act"] <-  c.Act + c.intn*intervention   # cost of staying acutely injured for one cycle plus cost of intervention
  c.it[M_it == "Prm"] <-  c.Prm + c.intn*intervention   # cost of staying permanently injured for one cycle plus cost of intervention
  
  return(c.it)                                          # return the costs
}



# Create the health outcomes function

Effs <- function(M_it,                 # M_it: health state occupied by individual i at cycle t
                 intervention=FALSE,   # intervention: is there an intervention?
                 x_i=NULL,         # x_i: matrix of individual characteristics
                 cl=1) {               # cl: cycle length in years
  
  u.it <- 0                                                                # everyone starts with utility 0
  u.it[M_it == "H"] <- u.H                                                 # utility if healthy
  u.it[M_it == "Act"] <- intervention*u.intn + (1-intervention)*u.Act      # utility if acutely injured, conditional on intervention
  u.it[M_it == "Prm"] <- u.Prm                                             # utility if permanently injured
  
  QALYs <- u.it * cl        # calculate QALYs during cycle t
  return(QALYs)             # return QALYs
}



#### Run simulation ####

sim <- MicroSim(initStates, n.i, n.t, m.fire, m.smoke.duration.cum, v.n, m.x, cl, birthRate_bl, birthRate_change, allCauseMortality_bl, allCauseMortality_change,d.c, d.e, TR.out = TRUE, TS.out = TRUE, intn=FALSE, seed = 1)


#### Next steps ####

# Refine transition probability functions to include the effect of other important characteristics
# Refine the Costs function to include the effect of other important characteristics (e.g. access to healthcare/insurance etc.)
# Refine the Effs function to allow the effect of access to medical care? Or should this be in transition probs?
#   
# The model is currently a closed cohort. Incorporate births/deaths and immigration.
# Get real population data to define the cohort.
# Think about how to model multiple outcomes together - can discuss with Alkin
# Would be great to potentially include GIS data - could get input and support from Tarik Benmarhnia
# 
# 
# (1) Some interaction between individuals - e.g., geographic grouping (if same or proximate, than risk of fire is linked with risk of fire of other groups)
# (2) A second health condition that is either related or unrelated (TBD)


#### Figures and tables ####

library(ggplot2)
library(reshape2)
library(scales)

trace <- as.data.frame(sim$TR)
trace$cycle <- factor(0:n.t)
trace_long <- melt(data=trace,id.vars="cycle", variable.name="state", value.name="proportion")

fig_trace <- ggplot(trace_long, aes(x=cycle, y=proportion*100, group=state)) +
  geom_line(aes(color=state)) +
  labs(title = "Health states over time", x="time", y="% of population")
# ggsave("wildfire_trace.tiff",fig_trace,units = "in",w=10,h=7)

mean_costs <- melt(colMeans(sim$m.C), value.name = "mean_cost") 
mean_costs$cycle <- factor(0:n.t)

fig_costs <- ggplot(data=mean_costs, aes(x=cycle, y=mean_cost)) +
  geom_point() +
  labs(title="Mean cost per person per cycle", x="time",y="mean annual cost ($/year)")
# ggsave("wildfire_costs.tiff",fig_costs,units = "in",w=10,h=7)

tot_qalys <- melt(colSums(sim$m.E), value.name="tot_qalys")
tot_qalys$cycle <- factor(0:n.t)
tot_qalys$cum_qalys_lost <- 100-tot_qalys$tot_qalys


fig_qalyslost <- ggplot(data=tot_qalys, aes(x=cycle, y=cum_qalys_lost)) +
  geom_point() +
  labs(title="Total QALYs lost per cycle", x="time",y="total QALYs lost")
# ggsave("wildfire_qalys_lost.tiff",fig_qalyslost,units = "in",w=10,h=7)











