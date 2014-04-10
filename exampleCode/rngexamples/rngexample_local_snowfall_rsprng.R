rm(list=ls())
## using snowfall + rsprng
library("snowfall")
library("rsprng")

# the documentation for rsprng states that there are two methods to use rsprng:
#  (1) call init.sprng in each process, or
#  (2) call init.sprng and spawn.sprng in parent process, then call unpack.sprng in children
# here we use the first method -- we weren't able to obtain reproducible results using the second method.

## redefine our example function for use with rsprng, insert a call to Sys.sleep so that we can tell when parallelization is being used
test_fun <- function(vec_lens, nstream, streamno, seed) {
  # Spawn some child streams.  A separate rng stream will be used for each runif vector.
  # This is not really necessary, but demonstrates the functionality.
  # For example, this technique may be useful if you might add more random number generation at a later date.
  Sys.sleep(1)
  
  rngstream <- init.sprng(nstream = nstream, streamno = streamno, seed = seed)
  
  lapply(vec_lens,
         function(len) {
           rnorm(len)
         })
}


## demo function use -- not parallel

lens <- list(1:2, 4:5)

# initialize an rng stream using manually specified seed, default kindprng and parameters (for default kindprng, parameters is # of lags)
timea <- system.time({
  a <- lapply(seq_along(lens), function(ind) test_fun(vec_lens = lens[[ind]], nstream = length(lens), streamno = ind - 1, seed = 98413551))
})

# repeat the above -- should get identical results
b <- lapply(seq_along(lens), function(ind) test_fun(vec_lens = lens[[ind]], nstream = length(lens), streamno = ind - 1, seed = 98413551))

identical(a, b)

# repeat the above with an extra vector in lens.  The first two components of the result should be identical to a and b
lens <- list(1:2, 4:5, 3:4)

timec <- system.time({
  c <- lapply(seq_along(lens), function(ind) test_fun(vec_lens = lens[[ind]], nstream = length(lens), streamno = ind - 1, seed = 98413551))
})

identical(a, c[1:2])



## now, parallelize using snowfall

# initialize a local cluster with snowfall
sfInit( parallel=TRUE, cpus=8, type="SOCK" )
sfLibrary("rsprng", character.only = TRUE)

# do a above again, but this time in parallel using snowfall.  The result should be identical to a above.
lens <- list(1:2, 4:5)

sfExport(list = c("test_fun", "lens"))

timepa <- system.time({
  pa <- sfLapply(seq_along(lens), function(ind) test_fun(vec_lens = lens[[ind]], nstream = length(lens), streamno = ind - 1, seed = 98413551))
})

identical(a, pa)

# do c above again, but this time in parallel using snowfall.  The result should be identical to c above.
lens <- list(1:2, 4:5, 3:4)

sfExport(list = c("test_fun", "lens"))

timepc <- system.time({
  pc <- sfLapply(seq_along(lens), function(ind) test_fun(vec_lens = lens[[ind]], nstream = length(lens), streamno = ind - 1, seed = 98413551))
})

identical(c, pc)

# stop the cluster
sfStop()

timea
timepa
timec
timepc





















# method (2) of using rsprng below -- doesn't seem to be reliable.

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

lens <- list(1:2, 4:5)

# initialize an rng stream using manually specified seed, default kindprng and parameters (for default kindprng, parameters is # of lags)
init.sprng(nstream = 1, streamno = 0, seed = 98413551)
child_streams <- spawn.sprng(length(lens))

a <- lapply(seq_along(lens), function(ind) test_fun(lens[[ind]], child_streams[, ind]))

# repeat the above -- should get identical results
init.sprng(nstream = 1, streamno = 0, seed = 98413551)
bchild_streams <- spawn.sprng(length(lens))

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
sfLibrary("rsprng", character.only = TRUE)

# do a above again, but this time in parallel using snowfall.  The result should be identical to a above.
lens <- list(1:2, 4:5)

init.sprng(nstream = 1, streamno = 0, seed = 98413551)
child_streams <- spawn.sprng(length(lens))

sfExport(list = c("test_fun", "lens", "child_streams"))

pa <- lapply(seq_along(lens), function(ind) test_fun(lens[[ind]], child_streams[, ind]))

identical(a, pa)

# do c above again, but this time in parallel using snowfall.  The result should be identical to c above.
lens <- list(1:2, 4:5, 3:4)

init.sprng(nstream = 1, streamno = 0, seed = 98413551)
child_streams <- spawn.sprng(length(lens))

sfExport(list = c("test_fun", "lens", "child_streams"))

pc <- lapply(seq_along(lens), function(ind) test_fun(lens[[ind]], child_streams[, ind]))

identical(c, pc)

# stop the cluster
sfStop()
