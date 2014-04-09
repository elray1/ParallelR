rm(list=ls())
## using snowfall + rsprng
library("snowfall")
library("rsprng")

## redefine our example function for use with rsprng.
test_fun <- function(vec_lens, rngstream) {
  # Spawn some child streams.  A separate rng stream will be used for each runif vector.
  # This is not really necessary, but demonstrates the functionality.
  # For example, this technique may be useful if you might add more random number generation at a later date.
  unpack.sprng(rngstream)
  
  lapply(vec_lens,
    function(len) {
      rnorm(len)
    })
}

## demo function use -- not parallel
# there are two methods to use rsprng:
#  (1) call init.sprng in each process, or
#  (2) call init.sprng and spawn.sprng in parent process, then call unpack.sprng in children
# here we use the second method since it requires passing one fewer arguments around

lens <- list(1:2, 4:5)

# initialize an rng stream using manually specified seed, default kindprng and parameters (for default kindprng, parameters is # of lags)
init.sprng(nstream = 1, streamno = 0, seed = 98413551)
child_streams <- spawn.sprng(length(lens))

a <- lapply(seq_along(lens), function(ind) test_fun(lens[[ind]], child_streams[, ind]))

# repeat the above -- should get identical results
init.sprng(nstream = 1, streamno = 0, seed = 98413551)
child_streams <- spawn.sprng(length(lens))

b <- lapply(seq_along(lens), function(ind) test_fun(lens[[ind]], child_streams[, ind]))

identical(a, b)

# repeat the above with an extra vector in lens.  The first two components of the result should be identical to a and b
lens <- list(1:2, 4:5, 3:4)

init.sprng(nstream = 1, streamno = 0, seed = 98413551)
child_streams <- spawn.sprng(length(lens))

c <- lapply(seq_along(lens), function(ind) test_fun(lens[[ind]], child_streams[, ind]))

identical(a, c[1:2])



## now, parallelize using snowfall

# initialize a local cluster with snowfall
sfInit( parallel=TRUE, cpus=8, type="SOCK" )

# do a above again, but this time in parallel using snowfall.  The result should be identical to a above.
lens <- list(1:2, 4:5)

init.sprng(nstream = 1, streamno = 0, seed = 98413551)
child_streams <- spawn.sprng(length(lens))

pa <- lapply(seq_along(lens), function(ind) test_fun(lens[[ind]], child_streams[, ind]))

identical(a, pa)

# do c above again, but this time in parallel using snowfall.  The result should be identical to c above.
lens <- list(1:2, 4:5, 3:4)

init.sprng(nstream = 1, streamno = 0, seed = 98413551)
child_streams <- spawn.sprng(length(lens))

pc <- lapply(seq_along(lens), function(ind) test_fun(lens[[ind]], child_streams[, ind]))

identical(c, pc)

# stop the cluster
sfStop()






# redefine our example function to use rstream functionality "manually"
test_fun <- function(vec_lens, rngstream) {
  lapply(vec_lens,
    function(len) {
      
      runif(len)
    })
}

# create an rng stream
rngstream <- new("rstream.mrg32k3a")




## doParallel/foreach
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

init.sprng(nstream = 10, streamno = 0, seed = 9873499)
(resa <- runif(5))
free.sprng()

init.sprng(nstream = 10, streamno = 0, seed = 9873499)
(resb <- runif(5))
free.sprng()

res1a <- apply(matrix(seq_len(10) - 1), 1, function(streamno) {
  # initialize an rng stream using default seed, kindprng, and parameters (for default kindprng, parameters is # of lags)
  init.sprng(nstream = 10, streamno = streamno, seed = 98413551)
  
  temp <- runif(5)
  
  # free memory allocated by rsprng to store stream states, reset RNGkind to default
  free.sprng()
  
  return(temp)
})

res1b <- apply(matrix(seq_len(10) - 1), 1, function(streamno) {
  # initialize an rng stream using default seed, kindprng, and parameters (for default kindprng, parameters is # of lags)
  init.sprng(nstream = 10, streamno = streamno, seed = 98413551)
  
  temp <- runif(5)
  
  # free memory allocated by rsprng to store stream states, reset RNGkind to default
  free.sprng()
  
  return(temp)
})

res1c <- apply(matrix(seq_len(11) - 1), 1, function(streamno) {
  # initialize an rng stream using default seed, kindprng, and parameters (for default kindprng, parameters is # of lags)
  init.sprng(nstream = 11, streamno = streamno, seed = 98413551)
  
  temp <- runif(5)
  
  # free memory allocated by rsprng to store stream states, reset RNGkind to default
  free.sprng()
  
  return(temp)
})

identical(res1a, res1b)
identical(res1a, res1c[, 1:10])


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



