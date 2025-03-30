#!/bin/bash
REPO_PATH="/rds/general/user/sd121/home/contact-survey-fatigue"
OUT_PATH="/rds/general/user/sd121/home/contact-survey-fatigue-outputs"
CONFIG_FILE="negb_longit_gp.yaml"

eval "$(~/miniforge3/bin/conda shell.bash hook)"
source activate contact-survey-fatigue

# Move into repository
cd $REPO_PATH

# Summarise posterior quantities
Rscript scripts/postproc_summarise_longit.R --config "$CONFIG_FILE"