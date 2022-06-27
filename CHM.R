###################### Climate - Health Modeling v2.0 ########################

##################
# call functions
source("MicroSim.R")
source("Probs.R")
source("Costs.R")
source("Effs.R")
##################


#### 1) Population Inputs #### 

n.i <- 1000                     # number of individuals
n.t <- 52/2                       # time horizon (in cycles)
cl <- 2/52                      # length of each cycle (in years)
v.n_asthma <- c(0,   # No asthma 
                1,   # Asthma - controlled
                21,  # Asthma exacerbation - oral corticostreroid (OCS) ONLY
                22,  # Asthma exacerbation - ED visit ONLY
                212, # Asthma exacerbation - OCS + ED visit
                223, # Asthma exacerbation - ED + hospitalization
                50,  # Dead - asthma
                100  # Dead - other cause
)

# add asthma with inhaled therapy?
 
n.s_asthma <- length(v.n_asthma)               # number of health states
initStates <- sample(v.n_asthma[1:3], size = n.i, 
                     prob = c(0.7, 0.2, 0.1), replace=TRUE)   # determine mix of starting states
d.c <- d.e <- 0.03               # 3% discount rate for both costs and QALYs
v.intn <- c("intervention 1", "intervention 2")    # strategy names

birthRate_bl <- (52.4/1000) * cl   # convert into rate for cycle length
birthRate_change <- -0.035 * cl

allCauseMortality_bl <- (583.1/100000) * cl  
allCauseMortality_change <- (-2.2/100) * cl

# Individual characteristics

m.x <- data.frame(matrix(nrow=n.i, ncol=0))  # create matrix that will store individual characteristics
m.x$id <- seq(1,n.i,by=1)          # assign ids
m.x$age <- floor(rnorm(n.i,48,5))  # determine each individual's age based on a normal dist'n with mean=48 and sd=5
m.x$sex <- rbinom(n.i,1,0.55)      # determine each individual's sex based on a binomial dist'n with 0=male, 1=female, and p(female)=0.55
m.x$rural <- rbinom(n.i, 1, 0.30)  # determine each individual's residence based on a binomial dist'n with 0=urban, 1=rural, and p(rural)=0.30
m.x$exposure <- FALSE              # no previous exposure at the beginning
m.x$asthma <- initStates

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

# m.x$neighborhood <- as.factor(m.x$neighborhood)
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


# Transition probabilities 

p.0.1 <- 0.05

p.1.21 <- 0.12  # controlled asthma to OCS burst
p.1.22 <- 0.05
p.1.212 <- 0.1
p.1.223 <- 0.02

p.21.1 <- 0.8
p.21.22 <- 0.06
p.21.212 <- 0.06
p.21.223 <- 0.01

p.22.1 <- 0.1
p.22.21 <- 0.75
p.22.212 <- 0.05
p.22.223 <- 0.03

p.212.1 <- 0.05
p.212.21 <- 0.4
p.212.22 <- 0.2
p.212.223 <- 0.1

p.223.1 <- 0.01
p.223.21 <- 0.75
p.223.22 <- 0.02
p.223.212 <- 0.1

p.asthmaMort <- 0.002

# 
# 
# #                from:    Successful |  Suboptimal | PrimaryCare | Hospital | Failure
# transitionGrid <- matrix(c( 0.306,         0.058,       0.001,       0.004,   0.002,    # to: Successful control
#                             0.064,         0.382,       0.001,       0.003,   0.009,    # to: Suboptimal control
#                             0.001,         0.001,       0.001,       0.001,   0.001,    # to: Primary care managed exacerbation
#                             0.003,         0.003,       0.001,       0.002,   0.001,    # to: Hospital managed exacerbation
#                             0.000,         0.000,       0.000,       0.000,   0.079),   # to: Treatment failure
#                          nrow=n.s_asthma, ncol=n.s_asthma, dimnames = list(v.n_asthma,v.n_asthma))
# # distn of the destination by state. e.g, of ppl with succesful, ~88% stay there, 10% go to suboptimal
# # 10% seek primary care, 2% go to hospital, 0.2% die. 100% of ppl in each state has to have a destination. 
# # maybe take another look at papers that use markov disease states and microsim. 
# transitionGrid <- prop.table(transitionGrid, margin=1)
# 
# m.transitionProbs <-  melt(transitionGrid)
# colnames(m.transitionProbs) <-  c("from", "to","prob")
# 
# 
# #                from:    Successful |  Suboptimal | PrimaryCare | Hospital | Failure
# m.transitionProbs$sex_adj <- c(1,          1.05,        1.05,       1.05,         1,       # to: Successful control 
#                                0.95,       1,           1.05,       1.05,         1,       # to: Suboptimal control
#                                0.95,       0.95,        1,          1.05,         1,       # to: Primary care managed exacerbation
#                                0.95,       0.95,        0.95,       1,            1,       # to: Hospital managed exacerbation
#                                0.95,       0.95,        0.95,       0.95,         1)       # to: Treatment failure
#                                
# #                from:    Successful |  Suboptimal | PrimaryCare | Hospital | Failure
# m.transitionProbs$age_adj <- c(1,          1.05,        1.05,       1.05,         1,       # to: Successful control 
#                                0.95,       1,           1.05,       1.05,         1,       # to: Suboptimal control
#                                0.95,       0.95,        1,          1.05,         1,       # to: Primary care managed exacerbation
#                                0.95,       0.95,        0.95,       1,            1,       # to: Hospital managed exacerbation
#                                0.95,       0.95,        0.95,       0.95,         1)       # to: Treatment failure
# 
# #                from:    Successful |  Suboptimal | PrimaryCare | Hospital | Failure
# m.transitionProbs$fire_adj <- c(1,          1,           1   ,       1   ,         1,       # to: Successful control 
#                                 1.20,       1,           1   ,       1   ,         1,       # to: Suboptimal control
#                                 1.30,       0.95,        1,          1   ,         1,       # to: Primary care managed exacerbation
#                                 1.60,       0.95,        0.95,       1,            1,       # to: Hospital managed exacerbation
#                                 1.15,       0.95,        0.95,       0.95,         1)       # to: Treatment failure


# p.HD <- 0.03                     # probability to die when healthy
# p.HAct <- 0.1                    # probability of acute injury when healthy
# p.ActH <- 0.75                   # probability to become healthy when acutely injured
# p.ActPrm <- 0.08                 # probability to become permanently injured when acutely injured
# rr.Act <- 1.25                   # rate ratio of death when acutely injured vs healthy
# rr.Prm <- 2                      # rate ratio of death when permanently injured vs healthy
# r.HD <- -log(1-p.HD)             # rate of death when healthy
# r.ActD <- rr.Act*r.HD            # rate of death when acutely injured
# r.PrmD <- rr.Prm*r.HD            # rate of death when permanently injured
# p.ActD <- 1-exp(-r.ActD)         # probability of death when acutely injured
# p.PrmD <- 1-exp(-r.PrmD)         # probability of death when permanently injured
rp.smoke <- 0.05                 # increase of mortality rate for all with history of smoke exposure conditional on duration of exposure

rr.HD.f <- 1.2                   # risk ratio for healthy -> dead for females
rr.HAct.f <- 1.4                 # risk ratio for healthy -> acute injury for females
rr.HD.65 <- 1.3                  # risk ratio for healthy -> dead for those aged 65 or older
rr.HAct.65 <- 1.5                # risk ratio for healthy -> acute injury for those aged 65 or older

# Cost and utility inputs

v.costs <- c(0, 500, 1000, 5000, 10000)
v.utilities <- c(1, 0.75, 0.65, 0.6, 0.6)

# c.H <- 0                         # cost of remaining healthy
# c.Act <- 10000                   # cost of acute injury
# c.Prm <- 20000                   # cost of permanent injury
# c.intn <- 5000                   # cost of intervention
# c.fire <- 15000                  # cost of fire occurring e.g., infrastructure damage, rebuilding homes
# 
# u.H <- 1                         # utility when healthy
# u.Act <- 0.75                    # utility when acutely injured
# u.Prm <- 0.65                    # utility when permanently injured
# u.intn <- u.Act                  # utility with intervention (in this case, intervention reduces fire risk and doesn't affect utility)
# ru.ActPrm <- 0                   # decrease in utility with every additional year being injured (assume same for now)


#### 2) Climate Data ####

p.fire <- 0.30                   # probability that a wildfire occurs 
rr.fire.urban <- 0.25            # risk ratio for fire in urban area versus rural
rr.fireOverTime <- 1.05          # additional fire risk each consecutive year

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
# NOTE: this will need to be calculated per individual, because someone born in the last year will only have that year as their exposure

m.smoke.duration.cum <- m.smoke.duration 
for (t in 1:(n.t-1)) {
  m.smoke.duration.cum[ , t+1] <- m.smoke.duration.cum[ ,t] + m.smoke.duration[ ,t+1]
}



#### 3) Run Model ####

sim <- MicroSim(initStates, 
                n.i, n.t, 
                m.fire, m.smoke.duration.cum, 
                v.n_asthma, 
                m.x, 
                cl, 
                birthRate_bl, birthRate_change, 
                allCauseMortality_bl, allCauseMortality_change,
                d.c, d.e, 
                TR.out = TRUE, TS.out = FALSE, 
                intn=FALSE, 
                seed = 1)

#### 4) Tables and Figures ####


library(ggplot2)
library(reshape2)
library(scales)


# add figure with cycle on x axis, %by state on the vertical axis. A series of lines and the space in between the lines is shaded
# in one sim you start with everyone starting at 1. then start with everyone at 0 but with some small risk of developing asthma

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

