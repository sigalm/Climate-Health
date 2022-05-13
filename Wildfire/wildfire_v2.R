set.seed(12345)
library(dplyr)

#### Model inputs ####

n.i <- 100                       # number of individuals
n.t <- 10                        # time horizon, 10 cycles
cl <- 1                          # length of each cycle (year)
v.n <- c("H", "Act", "Prm", "D") # possible health states: Healthy, Acute injury, Permanent injury, Dead
n.s <- length(v.n)               # number of health states
v.M_1 <- rep(v.n[1], n.i)        # everyone begins in the Healthy state
d.c <- d.e <- 0.03               # 3% discount rate for both costs and QALYs
v.intn <- c("No prescribed burning", "Prescribed burning")    # strategy names
# v.x <- c("id", "age","sex","rural","exposure")    # individual characteristics of relevance (age, sex, rural/urban designation, previous wildfire exposure)

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
rp.smoke <- 0.05                 # increase of mortality rate for permanently injured with history of exposure to wildfire

rr.HD.f <- 1.2                   # risk ratio for healthy -> dead for females
rr.HAct.f <- 1.4                 # risk ratio for healthy -> acute injury for females

rr.fire.over.time <- 1.05        # additional fire risk each consecutive year


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

# set neighborhood based on rural/urban status
rural_neighborhoods <- c("A","B")
probs_rural <- c(0.65, 1-0.65)
urban_neighborhoods <- c("C","D","E")
probs_urban <- c(0.35,0.45,1-0.35-0.45)
all_neighborhoods <- c(rural_neighborhoods, urban_neighborhoods)

for (k in 1:nrow(m.x[m.x$rural==1, ])) {
  m.x$neighborhood[m.x$rural==1][k] <-  sample(rural_neighborhoods,size=1,prob=probs_rural)
}
for (k in 1:nrow(m.x[m.x$rural==0, ])) {
  m.x$neighborhood[m.x$rural==0][k] <-  sample(urban_neighborhoods,size=1,prob=probs_urban)
}
m.x$neighborhood <- as.factor(m.x$neighborhood)

n.neighborhood <- length(all_neighborhoods)

# set household ID based on neighborhood
# mean household size is 4
m.x$fam_id <- NA
mean_fam_size <- 4
fam_count <- 1
for (k in levels(m.x$neighborhood)) {
  neighborhood_size <- nrow(m.x[m.x$neighborhood==k, ])
  family_size <- ceiling(neighborhood_size/mean_fam_size)
  m.x$fam_id[m.x$neighborhood==k] <- sample(fam_count:(fam_count+family_size), size = neighborhood_size, replace=TRUE)
  fam_count <- fam_count+family_size+1
  }

#### Create functions ####

MicroSim <- function(v.M_1, n.i, n.t, p.fire, v.n, m.x, cl, d.c, d.e, TR.out = TRUE, TS.out = TRUE, intn=FALSE, seed = 1) {
  
  # Arguments:
  # v.M_1: vector of initial states for each individual
  # n.i: number of individuals
  # n.t: number of cycles
  # p.fire: probability of a fire in a given cycle
  # v.n: vector of health state names
  # m.x: vector or matrix with individual characteristics
  # cl: cycle length
  # d.c, d.e: discount rates for cost and health outcomes
  # TR.out: should the output include a microsimulation trace?
  # TS.out: should the output include a transition array between states?
  # intn: is there an intervention? (scalar with Boolean value, could be vector of Booleans if some people receive intn and some don't)
  # seed: starting seed number
  
  # You need the following functions:
  # Probs: to estimate transition probabilities
  # Costs: to estimate costs of state values
  # Effs: to estimate health outcomes
  
  v.dwc <- 1 / (1 + d.c) ^ (0:n.t)     # calculate discount weights for costs per cycle
  v.dwe <- 1 / (1 + d.e) ^ (0:n.t)    # calculate discount weights for QALYs per cycle
  
  
  # initialize matrices to capture fires, health states, costs, and health outcomes each cycle:
  
  m.fire <- matrix(nrow=n.neighborhood, ncol=n.t, dimnames = list(all_neighborhoods, paste("cycle",1:n.t)))   

  for (k in 1:n.neighborhood) {
     current_neighborhood <- all_neighborhoods[k]
     
     for (t in 1:n.t) {
      p.fire.tmp <- min(p.fire*rr.fire.over.time^(t-1), 1)
      print(p.fire.tmp)
      m.fire[k, t] <- rbinom(1,size=1,prob=(
        (current_neighborhood %in% rural_neighborhoods)*p.fire.tmp + 
          (current_neighborhood %in% urban_neighborhoods)*min(p.fire.tmp*rr.fire.urban,1))) 
    }
  }
  
  
  # m.M <- m.C <- m.E <- matrix(nrow=n.i, ncol=n.t+1, dimnames = list(paste("ind", 1:n.i), paste("cycle",0:n.t)))  
  m.M[ ,1] <- v.M_1    # indicate initial health states in first column (first cycle)
  
  
  
  
  
  for (i in 1:n.i) {
    set.seed(seed+i)                                              # set seed for every individual
    m.C[i, 1] <- Costs(M_it=m.M[i, 1], intervention=intn)         # estimate cost for individual i given their health state and intervention
    m.E[i, 1] <- Effs(M_it=m.M[i, 1], intervention=intn, 
                      m.chars=m.x[i, ], cl=1)                     # estimate QALYs for individual i 
    
    for (t in 1:n.t) {
      location <- m.x$neighborhood[i]
      v.p <- Probs(M_it=m.M[i,t], m.fire_it=m.fire[location, t], m.chars_i=m.x[i, ])               # calculate the transition probability for individual i at cycle t
      m.M[i, t+1] <- sample(v.n, prob=v.p, size=1)                                          # sample the next health state given probability to transition
      m.C[i, t+1] <- Costs(M_it=m.M[i, t+1], intervention=intn)                             # estimate costs for individual i at cycle t+1
      m.E[i, t+1] <- Effs(M_it=m.M[i, t+1], intervention=intn, m.chars=m.x[i, ], cl)        # estimate QALYs for individual i at cycle t+1
      m.x$age[i] <- m.x$age[i] + cl                                                         # increase age by cycle length
      
      if (m.fire[location, t]==1) {m.x$exposure[i] <-  TRUE}        # if there has been a fire, set exposure history to TRUE, otherwise keep the same
      
    } # close loop for cycles
    
    # display simulation progress
    # cat('\r', paste(i/n.i * 100, "% done"))
    
    
  } # close loop for individuals
  
  
  tc <- m.C %*% v.dwc     # total discounted costs per individual
  te <- m.E %*% v.dwe     # total discounted QALYs per individual
  tc_hat <- mean(tc)      # average discounted costs
  te_hat <- mean(te)      # average discounted QALYS
  
  
  if (TS.out == TRUE) {                                    # create matrix of transition across states
    TS <- paste(m.M, cbind(m.M[,-1], "D"), sep = "->")     # this kills off everyone in the last cycle, which is not true. need to fix.
    TS <- matrix(TS, nrow=n.i)
    rownames(TS) <- paste("ind", 1:n.i)
    colnames(TS) <- paste("cycle", 1:n.t)
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
  
  results <- list(m.M=m.M, m.C=m.C, m.E=m.E, tc=tc, te=te, tc_hat=tc_hat, te_hat=te_hat, m.fire=m.fire, m.x=m.x, TS=TS, TR=TR)
  return(results)
  
}


# Create the probabilities function

Probs <- function(M_it, m.fire_it, m.chars_i) {
  # M_it: health state occupied by individual i at cycle t
  # m.fire_it: fire experience of individual i at cycle t (binary)
  # m.chars_i: individual characteristics of individual i
  
  v.p.it <- rep(NA, n.s)   # initialize vector of state transition probabilities
  names(v.p.it) <- v.n     # name columns with the health states
  
  # update v.p.it with appropriate probabilities (note: v.n is H; Act; Prm; D)
  
  p.PrmD_tmp <- 1-exp(-r.PrmD * (1 + rp.smoke * m.chars_i$exposure))*m.fire_it     # calculate p.PrmD conditional on previous exposure
  p.HAct_tmp <- p.HAct*m.fire_it
  p.HD_tmp <- p.HD*m.fire_it
  
  v.p.it[M_it == "H"] <- c(1-p.HAct_tmp-p.HD_tmp , p.HAct_tmp, 0, p.HD_tmp)
  v.p.it[M_it == "Act"] <- c(p.ActH, 1-p.ActH-p.ActPrm-p.ActD, p.ActPrm, p.ActD)
  v.p.it[M_it == "Prm"] <- c(0, 0, 1-p.PrmD_tmp, p.PrmD_tmp)
  v.p.it[M_it == "D"] <- c(0, 0, 0, 1)
  
  ifelse(sum(v.p.it) == 1, return(v.p.it), print("Probabilities do not sum to 1"))     # return error if probabilities do not sum to 1
}


# Create the costs function

Costs <- function(M_it, intervention=FALSE) {
  # M_it: health state occupied by individual i at cycle t
  # intervention: is there an intervention?
  
  c.it <- 0                                             # everyone starts with 0 costs
  c.it[M_it == "H"] <-  c.H                             # cost of staying healthy for one cycle
  c.it[M_it == "Act"] <-  c.Act + c.intn*intervention   # cost of staying acutely injured for one cycle plus cost of intervention
  c.it[M_it == "Prm"] <-  c.Prm + c.intn*intervention   # cost of staying permanently injured for one cycle plus cost of intervention
  
  return(c.it)                                          # return the costs
}



# Create the health outcomes function

Effs <- function(M_it, intervention=FALSE, m.chars=NULL, cl=1) {
  # M_it: health state occupied by individual i at cycle t
  # intervention: is there an intervention?
  # m.chars: matrix of individual characteristics
  # cl: cycle length in years
  
  u.it <- 0                                                                # everyone starts with utility 0
  u.it[M_it == "H"] <- u.H                                                 # utility if healthy
  u.it[M_it == "Act"] <- intervention*u.intn + (1-intervention)*u.Act      # utility if acutely injured, conditional on intervention
  u.it[M_it == "Prm"] <- u.Prm                                             # utility if permanently injured
  
  QALYs <- u.it * cl        # calculate QALYs during cycle t
  return(QALYs)             # return QALYs
}



#### Run simulation ####

sim <- MicroSim(v.M_1, n.i, n.t, p.fire, v.n, m.x, cl, d.c, d.e, TR.out = TRUE, TS.out = TRUE, intn=FALSE, seed = 1)


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
ggsave("wildfire_trace.tiff",fig_trace,units = "in",w=10,h=7)

mean_costs <- melt(colMeans(sim$m.C), value.name = "mean_cost") 
mean_costs$cycle <- factor(0:n.t)

fig_costs <- ggplot(data=mean_costs, aes(x=cycle, y=mean_cost)) +
  geom_point() +
  labs(title="Mean cost per person per cycle", x="time",y="mean annual cost ($/year)")
ggsave("wildfire_costs.tiff",fig_costs,units = "in",w=10,h=7)

tot_qalys <- melt(colSums(sim$m.E), value.name="tot_qalys")
tot_qalys$cycle <- factor(0:n.t)
tot_qalys$cum_qalys_lost <- 100-tot_qalys$tot_qalys


fig_qalyslost <- ggplot(data=tot_qalys, aes(x=cycle, y=cum_qalys_lost)) +
  geom_point() +
  labs(title="Total QALYs lost per cycle", x="time",y="total QALYs lost")
ggsave("wildfire_qalys_lost.tiff",fig_qalyslost,units = "in",w=10,h=7)











