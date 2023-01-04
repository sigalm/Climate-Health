#### Create microdata ####

library(readxl)


n.i <- 2000
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
               0.09,   # Complete control
               0.0053, # Well control
               0.0008, # Somewhat control
               0.001,  # Poor control
               0.0009, # No control at all
               0.000,  # Dead - asthma
               0.000)  # Dead - other cause


n.s_asthma <- length(v.n_asthma)      # save number of health states

# determine mix of starting health states:

initStates <- sample(v.n_asthma, size = n.i, 
                     prob = initProbs, replace=TRUE)

v.therapy <- c("NoTx","S1", "S2","S3","S4","S5","OCS")   # continuous therapies / asthma management 

# get therapy matrix
therapyMatrix <- as.data.frame(read_xlsx("C:/Users/smaya/Box/AA_Sigal_Documents/Climate - Health/Modeling/asthmaTherapyMatrix.xlsx", range=("B2:H9"),
                                         col_names = v.therapy))
row.names(therapyMatrix) <- v.n_asthma

m.x <- data.frame(matrix(nrow=n.i, ncol=0))  # create matrix that will store individual characteristics
m.x$id <- seq(1,n.i,by=1)                    # assign ids
m.x$age <- floor(rnorm(n.i,48,12))           # determine each individual's age based on a normal dist'n with mean=48 and sd=12
m.x$sex <- rbinom(n.i,1,0.55)                # determine each individual's sex based on a binomial dist'n with 0=male, 1=female, and p(female)=0.55
m.x$exposure <- FALSE                        # no previous exposure at the beginning
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


saveRDS(m.x, file="InitPop")
