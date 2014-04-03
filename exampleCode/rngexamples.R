rm(list=ls())

## Reproducability -- set.seed not effective.

set.seed(1)
r1a <- runif(10)
set.seed(1)
r1b <- runif(10)

identical(r1a, r1b)




require(doParallel)
nCores<-8
cl <- makeCluster(nCores)
registerDoParallel(cl)

set.seed(1)
(r2a <-foreach(i=1:10, .combine=cbind) %dopar% {
  runif(1)
})

set.seed(1)
(r2b <-foreach(i=1:10, .combine=cbind) %dopar% {
  runif(1)
})

identical(r2a, r2b)

stopCluster(cl)




library("snowfall")
sfInit( parallel=TRUE, cpus=8, type="SOCK" )

set.seed(1)
(r3a <- sfSapply(1:10, function(i) runif(1)))

set.seed(1)
(r3b <- sfSapply(1:10, function(i) runif(1)))

identical(r3a, r3b)





## using the rsprng package
library("rsprng")

## two methods:
##   (1) call init.sprng in each process, or
##   (2) call init.sprng and spawn.sprng in parent process, then call unpack.sprng in children

## Method (1):

init.sprng(nstream = 10, streamno = 0)

res1a <- apply(matrix(seq_len(10) - 1), 1, function(streamno) {
  # initialize an rng stream using default seed, kindprng, and parameters (for default kindprng, parameters is # of lags)
  init.sprng(nstream = 10, streamno = streamno)
  
  temp <- runif(10)
  
  # free memory allocated by rsprng to store stream states, reset RNGkind to default
  free.sprng()
  
  return(temp)
})

res1b <- apply(matrix(seq_len(10) - 1), 1, function(streamno) {
  # initialize an rng stream using default seed, kindprng, and parameters (for default kindprng, parameters is # of lags)
  init.sprng(nstream = 10, streamno = streamno)
  
  temp <- runif(10)
  
  # free memory allocated by rsprng to store stream states, reset RNGkind to default
  free.sprng()
  
  return(temp)
})

identical(res1a, res1b)


## Using the rlecuyer package
library("rlecuyer")

seeds <- c(-1007132623, -549223669, 312902813, 1753239107, -1281266349, 1711070667)
.lec.SetPackageSeed(seeds)
.lec.CreateStream(as.character(1:10))
.lec.WriteStateFull("1")
.lec.GetState("1")

old.rng.stream <- .lec.CurrentStream("1")
.lec.WriteStateFull("1")
.lec.GetState("1")

(r1a <- runif(10))
.lec.WriteStateFull("1")
.lec.GetState("1")

.lec.ResetStartStream("1")
.lec.WriteStateFull("1")
.lec.GetState("1")

(r1b <- runif(10))
.lec.WriteStateFull("1")
.lec.GetState("1")

identical(r1a, r1b)

.lec.CurrentStreamEnd(old.rng.stream)

.lec.WriteStateFull("1")

.lec.DeleteStream(as.character(1:10))



