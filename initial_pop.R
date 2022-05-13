#### Set initial population ####

n.i <- 100
v.x <- c("id","family","neighborhood","rural","age","sex","exposure")
m.x <- data.frame(matrix(nrow=n.i, ncol=length(v.x)))
colnames(m.x) <- v.x


# 30% of population lives in rural neighborhoods
set.seed(123)
m.x$rural <- factor(rbinom(n.i,1,0.30), labels=c("urban","rural"))

# There are 2 rural and 3 urban neighborhoods
rural_neighborhoods <- c("A","B")
urban_neighborhoods <- c("C","D","E")

for (k in 1:nrow(m.x[m.x$rural=="rural", ])) {
  m.x$neighborhood[m.x$rural=="rural"][k] <-  sample(rural_neighborhoods,size=1,prob=c(0.65,1-0.65))
}
  
for (k in 1:nrow(m.x[m.x$rural=="urban", ])) {
  m.x$neighborhood[m.x$rural=="urban"][k] <-  sample(urban_neighborhoods,size=1,prob=c(0.35,0.45,1-0.35-0.45))
}

m.x$neighborhood <- as.factor(m.x$neighborhood)

# Define families

m.x[m.x$neighborhood=="A"]


mean_family_size <- 4
n.family <- n.i/mean_family_size
set.seed(123)
m.x$family <- round(runif(n.i,1,n.family),0)




# Give everyone and ID number










# Age is normally distributed around mean = 48 with sd = 5
m.x$age <- round(rnorm(n.i, 48, 5),0)

# Sex is randomly selected with 55% probability of being female
m.x$sex <- factor(rbinom(n.i,1,0.55), labels=c("male","female"))





# Individuals start with no wildfire exposure



