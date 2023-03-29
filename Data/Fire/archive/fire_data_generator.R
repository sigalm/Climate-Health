#### Placeholder fire data #####

p.fire <- 0.10                   # probability that a wildfire occurs 
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


saveRDS(m.fire, "fire_data_oct10.RDS")
