#!/bin/bash
#BSUB -R rusage[mem=1024] # ask for memory
#BSUB -n 4                # how many cores we want for our job
#BSUB -R span[hosts=1]    # ask for all the cores on a single machine
#BSUB -W 0:10             # not sure what this is doing
#BSUB -q short            # which queue we want to run in

module load R/3.0.2
R CMD BATCH /home/er71a/ParallelR/network_dopar_rng.R