# network_sf_rstream.R
#####################################################################################
# This file demonstrates the network simulation
# using the snowfall package for parallelization and
# the rstream package for random number generation
#####################################################################################

#####################################################################################
# General Setup
#####################################################################################
rm(list=ls())
library(sna)
library(ergm)
library(latentnet)
library(RDS)
require(doParallel)
library(doRNG)
set.seed(3510)

#####################################################################################
# Functions
#####################################################################################
#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
# Create network
#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
create.nets <- function(n.sim){
  nets <- simulate(mymodel, nsim = n.sim, coef = c(beta0, beta1, beta2), control = control.simulate.ergm(MCMC.burnin = 1e+5))
  return(nets)
}

#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
# RDS Sampling: Sample one wave
#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
sample.nodes <- function(net, seeds, prob.rec, ch.rec, value.rec, rds.mat, wave) {
  n.seeds <- length(seeds)
  i <- 1
  tot.ss <- sum(rds.mat[, 4])
  
  while ((i <= n.seeds) & (tot.ss < sample.size)){
    avail.neigh <- which((net[seeds[i], ] + 2 * rds.mat[, 4] * (rep == FALSE)) == 1)
    n.neigh <- length(avail.neigh)
    
    if (n.neigh > 0) {
      neigh.attr <- get.vertex.attribute(net, ch.rec)[avail.neigh]
      v.prob <- rep(0, n.neigh)
      v.prob[neigh.attr == value.rec] <- prob.rec
      v.prob[neigh.attr != value.rec] <- (1 - prob.rec)
      if (n.neigh <= n.rec) {
        s.nodes <- avail.neigh
      } else {
        s.nodes <- sort(sample(avail.neigh, n.rec, replace = rep, v.prob))
      }
      rds.mat[s.nodes, 3] <- seeds[i]
      rds.mat[s.nodes, 4] <- 1
      rds.mat[s.nodes, 5] <- wave
    }
    
    i <- i + 1
    tot.ss <- sum(rds.mat[, 4])
  }
  
  # In case over-recruited
  if (tot.ss > sample.size) {
    n.excess <- (tot.ss - sample.size)
    deleted.node <- s.nodes[1:n.excess]
    rds.mat[deleted.node, 3:5] <- 0
  }
  
  return(rds.mat)
}

#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
# RDS Sampling: Sample all waves
#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
rds.s <- function(net){
  rds.mat2 <- matrix(0, n.nodes, 5)
  deg <- degree(net, gmode="graph")
  nodes.avail <- which(((get.vertex.attribute(net, ch.seeds) == value.seeds) *
                          ind.seeds + rep(1, n.nodes) * (1 - ind.seeds)) == 1)
  prob.weigth.seeds <- deg[nodes.avail]
  seeds <- sort(sample(nodes.avail, size = n.seeds, replace = rep, prob = prob.weigth.seeds)) #Initial sample
  rds.mat2[, 1] <- c(1:n.nodes)
  rds.mat2[, 2] <- degree(net)/2
  rds.mat2[seeds, 3] <- 0
  rds.mat2[seeds, 4] <- 1
  rds.mat2[seeds, 5] <- 1
  
  k <- 1
  tot.ss <- length(which(rds.mat2[, 4] == 1))
  
  while ((tot.ss < sample.size) & (length(seeds) > 0)){
    sample.ow <- sample.nodes(net, seeds, prob.rec, ch.rec, value.rec, rds.mat2[, ], (k + 1))
    rds.mat2[, ] <- sample.ow
    seeds <- which(sample.ow[, 5] == (k + 1))
    
    #Make sure won't be in infinite loop because nodes don't have neighbors
    tot.ss.old <- tot.ss
    tot.ss <- length(which(rds.mat2[, 4] == 1))
    increm <- (tot.ss - tot.ss.old)
    
    if ((tot.ss < sample.size) & (increm == 0)){
      not.sampled <- (1:n.nodes)[which(rds.mat2[, 4] != 1)]
      remain1 <- (get.vertex.attribute(net, ch.seeds)[not.sampled] == value.seeds)
      remain2 <- rep(1, n.nodes)[not.sampled]
      remain <- not.sampled[which((remain1 * ind.seeds + remain2 * (1 - ind.seeds)) == 1)]
      
      s.size <- min(n.seeds, max((sample.size - tot.ss), 0))
      n.remain <- length(remain)
      
      if (n.remain <= s.size) {seeds <- remain} else 
      {seeds<-sort(sample(remain, size = s.size, replace = rep))}
      
      rds.mat2[seeds, 3] <- 0
      rds.mat2[seeds, 4] <- 1
      rds.mat2[seeds, 5] <- (k + 1)
    }
    
    k <- k + 1
    tot.ss <- length(which(rds.mat2[, 4] == 1))
  }
  return(rds.mat2)
}

#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
# Create RDS dataframe
#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
create.df <- function(col, sample, degree){
  s.pos <-sample[, 4] == 1
  deg.s <- degree[s.pos]
  dataframe <-as.data.frame(cbind(sample[s.pos, 1], deg.s, sample[s.pos, 3], col[s.pos]))
  colnames(dataframe)[1] <-"id"
  colnames(dataframe)[2] <-"network.size"
  colnames(dataframe)[3] <-"recruiter.id"
  colnames(dataframe)[4] <-"outcome"
  rdsdata <-as.rds.data.frame(dataframe, id = "id", recruiter.id = "recruiter.id",
                              network.size = "network.size")
  return(rdsdata)
}

#####################################################################################
# User input
##################################################################################### 
#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
# network parameters
#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
n.net <- 40                     #number of networks
n.nodes <- 1000                 #number of vertices
avg.deg <- 10                   #average degree
prob.tie <- avg.deg / n.nodes   #probability of a tie - no input
propA <- 0.2                    #proportion of node with characteristic A
hm <- 5                         #homophily - characterization based on prob

#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
# Recruiting information
#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
n.samples <- 2                  #THE PROGRAM IS NO LONGER ADAPTED FOR MULTIPLE SAMPLES
n.seeds <- 10                   #number of seeds
sample.size <- 500              #Desired Sample size
n.rec <- 2                      #number of recruits
rep <- FALSE                    #"TRUE" if w/ replacement "FALSE" else
desc <- "atrandom"

# Recruitment biased towards group/neighborhood
ch.rec <- "charA"               #Differential recruitment characteristic of interest
value.rec <- 1                  #Value of interest
prob.rec <- 0.5                 #Differential recruitment probability (.5 means no diff rec)

# Seed selection regimes (random, all from one group or neighborhood)
ch.seeds <- "charA"             #bias towards nodes of ch.seeds
ind.seeds <- 0                  #Indicator if bias is tested (1 = yes, 0 = no)
value.seeds <- 1                #Value of interest (all seeds will be from this group/neigh)

#####################################################################################
# Calculations
##################################################################################### 
#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
# Network Characteristics
#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
A <- rep(0, n.nodes)
w.A <- sample(1:n.nodes, round(propA * n.nodes, 0), replace = FALSE)
A[w.A] <- 1

#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
# Networks Model
#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
n.A <- sum(A)
n.B <- n.nodes - n.A
pAA <- avg.deg * (1 - n.B / (hm * (n.A - 1) + n.B)) / (n.A - 1)
pAB <- avg.deg / (hm * (n.A - 1) + n.B)
pBB <- avg.deg * (1 - n.A / (hm * (n.A - 1) + n.B)) / (n.B - 1)
beta0 <- log(pAB) - log(1 - pAB)
beta1 <- log(pBB) - log(1 - pBB) - beta0
beta2 <- log(pAA) - log(1 - pAA) - beta0
g.A <- rgraph(n = n.A, m = 1, mode = 'graph', tprob = pAA)
g.B <- rgraph(n = n.B, m = 1, mode = 'graph', tprob = pBB)
g.AB <- matrix(rbinom(n.A * n.B, 1, pAB), n.A, n.B)
g <- matrix(NA, n.nodes, n.nodes)
g[1:n.A, 1:n.A] <- g.A
g[1:n.A, (n.A+1):n.nodes] <- g.AB
g[(n.A+1):n.nodes, 1:n.A] <- t(g.AB)
g[(n.A+1):n.nodes, (n.A+1):n.nodes] <- g.B

net.p <- as.network(g, directed = FALSE)
set.vertex.attribute(net.p, "charA", A)
mymodel <- ergm(net.p ~ edges + nodematch("charA", diff = TRUE))

#####################################################################################
# Example network
##################################################################################### 
#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o
## Method 3 (parallel computing with foreach/doParallel, doRNG for RNG)
#~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o~o

# create cluster with 4 "cpus", using the OS-dependent default for communication
nCores <- 4
cl <- makeCluster(nCores)

# register the cluster so that doParallel is used as the back end
registerDoParallel(cl)

# export all objects to the cluster nodes
clusterExport(cl = cl, ls(), envir = environment())

t3a <- system.time({
  # parallelize the for loop using foreach and doRNG
  # load the packages statnet and RDS on the cluster nodes
  # set the RNG seed to 123
  # combine results using the cbind function
  est3a <-foreach(i = 1:n.net, .packages = c("statnet", "RDS"), .options.RNG = 123, .combine = cbind) %dorng% {
    net <- create.nets(1)
    rds.sample3 <- rds.s(net)
    rds.frame <- create.df(A, rds.sample3, rds.sample3[, 2])
    result <- RDS.II.estimates(rds.frame, outcome.variable = "outcome")$estimate
  }
})

t3b <- system.time({
  est3b <-foreach(i = 1:n.net, .packages = c("statnet", "RDS"), .options.RNG = 123, .combine = cbind) %dorng% {
    net <- create.nets(1)
    rds.sample3 <- rds.s(net)
    rds.frame <- create.df(A, rds.sample3, rds.sample3[, 2])
    result <- RDS.II.estimates(rds.frame, outcome.variable = "outcome")$estimate
  }
})

# stop the cluster
stopCluster(cl)

identical(est3a, est3b)

t3a
t3b
