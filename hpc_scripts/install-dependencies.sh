#!/bin/bash
#PBS -l walltime=01:00:00
#PBS -l select=1:ncpus=1:ompthreads=1:mem=64gb

REPO_PATH="/rds/general/user/sd121/home/contact-survey-fatigue"

module load anaconda3/personal
source activate contact-survey-fatigue

# Move into repository
cd $REPO_PATH

# Install pkg-config (this deals with installation issues for ragg)
conda install -c conda-forge pkg-config

# Run Stan model
Rscript install-dependencies-hpc.R