#### Create microdata ####
generatePopulation <- function(n.i,            # number of individuals at baseline
                               v.n_asthma,     # vector of asthma health state names
                               v.initProbs,    # vector of initial probabilities (i.e., distribution of asthma health states at baseline)
                               v.therapy,      # vector of asthma therapy names
                               therapyMatrix,  # matrix with the probabilities of receiving different therapies based on health state
                               v.places,       # vector with names of neighborhoods
                               probs.places,   # probability of a person living in each place in v.places
                               seed = 1        # seed number
) {
  
  m.x <- data.frame(matrix(nrow=n.i, ncol=0))  # create matrix that will store individual characteristics
  m.x$id <- seq(1,n.i,by=1)                    # assign ids
  set.seed(seed)
  m.x$age <- floor(rnorm(n.i,48,12))           # determine each individual's age based on a normal dist'n with mean=48 and sd=12
  set.seed(seed*2)
  m.x$sex <- rbinom(n.i,1,0.55)                # determine each individual's sex based on a binomial dist'n with 0=male, 1=female, and p(female)=0.55
  m.x$exposure <- FALSE                        # no previous exposure at the beginning
  set.seed(seed*4)
  m.x$rural <- rbinom(n.i, 1, 0.30)            # determine each individual's residence based on a binomial dist'n with 0=urban, 1=rural, and p(rural)=0.30
  
  # determine mix of starting health states
  n.s_asthma <- length(v.n_asthma)      # save number of health states
  set.seed(seed*6) 
  m.x$initStates <- sample(v.n_asthma, size = n.i, 
                            prob = initProbs, replace=TRUE)
  
  #determine what therapy each individual is on at baseline
  for (i in 1:n.i) {
    set.seed(seed+2*i)
    m.x$asthmaCare[i] <- sample(v.therapy, size=1, prob=therapyMatrix[as.character(m.x$initStates[i]), ])
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
    set.seed(seed+91*k)
    m.x$neighborhood[m.x$rural==1][k] <-  sample(ruralNeighborhoods,size=1,prob=probsRural)
  }
  
  # repeat for urban individuals
  for (k in 1:nrow(m.x[m.x$rural==0, ])) {
    set.seed(seed+17*k)
    m.x$neighborhood[m.x$rural==0][k] <-  sample(urbanNeighborhoods,size=1,prob=probsUrban)
  }
  
  
  # set household ID based on neighborhood (household aren't used for now, but will be important for other conditions)
  # mean household size is 4
  
  m.x$fam_id <- NA
  meanFamSize <- 4
  fam_count <- 1
  for (k in allNeighborhoods) {
    neighborhoodSize <- nrow(m.x[m.x$neighborhood==k, ])
    familySize <- ceiling(neighborhoodSize/meanFamSize)
    set.seed(seed*k)
    m.x$fam_id[m.x$neighborhood==k] <- sample(fam_count:(fam_count+familySize), size = neighborhoodSize, replace=TRUE)
    fam_count <- fam_count+familySize+1
  }
  
  
  
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
  
  my_list <- list(m.x=m.x, neighborhood.effect.size=neighborhood.effect.size)
  
  return(my_list)
  
}
