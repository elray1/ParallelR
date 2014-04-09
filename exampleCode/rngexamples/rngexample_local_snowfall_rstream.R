rm(list=ls())
## using snowfall + rstream
library("snowfall")
library("rstream")

## redefine our example function for use with rstream.
test_fun <- function(vec_lens, rngstream, substream_ind) {
  # Spawn some child streams.  A separate rng stream will be used for each runif vector.
  # This is not really necessary, but demonstrates the functionality.
  # For example, this technique may be useful if you might add more random number generation at a later date.
  rstream.packed(rngstream) <- FALSE
  for(i in seq_len(substream_ind))
    rstream.nextsubstream(rngstream)
  
  rstream.RNG(rngstream)
  
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
rngstream <- new("rstream.mrg32k3a", seed = c(-1007132623, -549223669, 312902813, 1753239107, -1281266349, 1711070667))
rstream.packed(rngstream) <- TRUE

a <- lapply(seq_along(lens), function(ind) test_fun(lens[[ind]], rngstream, ind))

# reset the stream and repeat the above -- should get identical results
rstream.packed(rngstream) <- FALSE
rstream.reset(rngstream)
rstream.packed(rngstream) <- TRUE

b <- lapply(seq_along(lens), function(ind) test_fun(lens[[ind]], rngstream, ind))

identical(a, b)


# repeat the above with an extra vector in lens.  The first two components of the result should be identical to a and b
rstream.packed(rngstream) <- FALSE
rstream.reset(rngstream)
rstream.packed(rngstream) <- TRUE
lens <- list(1:2, 4:5, 3:4)

c <- lapply(seq_along(lens), function(ind) test_fun(lens[[ind]], rngstream, ind))

identical(a, c[1:2])



## now, parallelize using snowfall

# initialize a local cluster with snowfall
sfInit( parallel=TRUE, cpus=8, type="SOCK" )

# do a above again, but this time in parallel using snowfall.  The result should be identical to a above.
rstream.packed(rngstream) <- FALSE
rstream.reset(rngstream)
rstream.packed(rngstream) <- TRUE

lens <- list(1:2, 4:5)

pa <- lapply(seq_along(lens), function(ind) test_fun(lens[[ind]], rngstream, ind))

identical(a, pa)

# do c above again, but this time in parallel using snowfall.  The result should be identical to c above.
rstream.packed(rngstream) <- FALSE
rstream.reset(rngstream)
rstream.packed(rngstream) <- TRUE
lens <- list(1:2, 4:5, 3:4)

pc <- lapply(seq_along(lens), function(ind) test_fun(lens[[ind]], rngstream, ind))

identical(c, pc)

# stop the cluster
sfStop()
