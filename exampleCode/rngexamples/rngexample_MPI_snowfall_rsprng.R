rm(list=ls())
## using snowfall + rsprng
library("snowfall")
library("rsprng")

## redefine our example function for use with rsprng, and insert call to Sys.sleep so that we can detect whether parallelization helped
test_fun <- function(vec_lens, rngstream) {
  # Spawn some child streams.  A separate rng stream will be used for each runif vector.
  # This is not really necessary, but demonstrates the functionality.
  # For example, this technique may be useful if you might add more random number generation at a later date.
  Sys.sleep(1)
  
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

time1 <- system.time({a <- lapply(seq_along(lens), function(ind) test_fun(lens[[ind]], child_streams[, ind]))})


## now, parallelize using snowfall

# initialize a local cluster with snowfall
sfInit(parallel = TRUE, cpus = 2, type = "SOCK")
sfLibrary("rsprng", character.only = TRUE)

# do a above again, but this time in parallel using snowfall.  The result should be identical to c above, the execution time should be faster.
init.sprng(nstream = 1, streamno = 0, seed = 98413551)
child_streams <- spawn.sprng(length(lens))

sfExport(list = c("test_fun", "lens", "child_streams"))

time2 <- system.time({pa <- sfLapply(seq_along(lens), function(ind) test_fun(lens[[ind]], child_streams[, ind]))})

identical(a, pa)

time1
time2

# stop the cluster
sfStop()









sfInit(parallel = TRUE, cpus = 2, type = "SOCK")
sfLibrary("rsprng", character.only = TRUE)

# do a above again, but this time in parallel using snowfall.  The result should be identical to c above, the execution time should be faster.
init.sprng(nstream = 1, streamno = 0, seed = 98413551)
child_streams <- spawn.sprng(length(lens))

sfExport(list = c("test_fun", "lens", "child_streams"))

time3 <- system.time({pa2 <- sfLapply(seq_along(lens), function(ind) test_fun(lens[[ind]], child_streams[, ind]))})

identical(pa, pa2)

sfStop()
