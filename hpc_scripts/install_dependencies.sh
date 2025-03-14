#!/bin/bash
#PBS -l walltime=01:00:00
#PBS -l select=1:ncpus=1:ompthreads=1:mem=64gb

REPO_PATH="/rds/general/user/sd121/home/contact-survey-fatigue"

eval "$(~/miniforge3/bin/conda shell.bash hook)"
conda create -n contact-survey-fatigue r-base=4.4.1 -c conda-forge
source activate contact-survey-fatigue

# Move into repository
cd $REPO_PATH

# Install pkg-config (this deals with installation issues for ragg)
conda update -n base -c defaults conda   # Update conda
conda install -c conda-forge pkg-config zlib freetype libxml2 udunits2 gdal

# Run Stan model
Rscript install-dependencies-hpc.R