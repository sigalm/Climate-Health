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


### OPTION 1: Create new starting population data ####

n.i <-2000              # number of individuals
n.t <- 10               # time horizon (in cycles)
cl <- 2/52              # length of each cycle (in years)

v.n_asthma <- c("0",   # No asthma
                "1",   # Complete control
                "2",   # Well control
                "3",   # Somewhat control
                "4",   # Poor control
                "5",   # No control at all
                "50",  # Dead - asthma
                "100") # Dead - other cause

initProbs <- c(0.902,  # No asthma
               0.03,   # Complete control
               0.0253, # Well control
               0.0158, # Somewhat control
               0.016,  # Poor control
               0.0109, # No control at all
               0.000,  # Dead - asthma
               0.000)  # Dead - other cause

v.therapy <- c("NoTx","S1", "S2","S3","S4","S5","OCS")

therapyMatrix <- as.data.frame(read_xlsx("Data/Asthma/Therapies/asthmaTherapyMatrix.xlsx", range=("B2:H9"),
                                         col_names = v.therapy))
row.names(therapyMatrix) <- v.n_asthma

popData <- generatePopulation(n.i = n.i,
                          v.n_asthma = v.n_asthma,
                          v.initProbs = v.initProbs,
                          v.therapy = v.therapy,
                          therapyMatrix = therapyMatrix,
                          v.places = c("1111","2222","3333","4444","5555"),
                          probs.places = )
m.x <- popData$m.x
neighborhood.effect.size <- popData$neighborhood.effect.size

# ### OPTION 2: Import starting population data ####

m.x <- readRDS("Data/Microdata/InitPop.rds")
neighborhood.effect.size <- 



