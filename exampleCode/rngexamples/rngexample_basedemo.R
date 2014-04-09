rm(list=ls())

# our example function
test_fun <- function(vec_lens) {
  lapply(vec_lens, function(len) rnorm(len))
}

# demo function use
test_fun(1:3)

# we would like to parallelize the outer lapply in the following call, while
#  (a) maintaining stochastic independence of the random numbers generated
#  (b) ensuring reproducability: we want to be able to get the same results later by setting seeds.
#  (c) it would be nice if we could also add on more vectors and/or make the vectors longer without screwing up (a) and (b)
lens <- list(1:2, 4:5)

set.seed(1)
a <- lapply(lens, test_fun)

set.seed(1)
b <- lapply(lens, test_fun)

identical(a, b)


# It would be nice if we could also add on more vectors and/or make the vectors longer without screwing up (a) and (b)
lens <- list(1:2, 4:5, 3:4)

set.seed(1)
c <- lapply(lens, test_fun)

identical(a, c[1:2])
