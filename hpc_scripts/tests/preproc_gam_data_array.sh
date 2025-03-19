#!/bin/bash
REPO_PATH="/rds/general/user/sd121/home/contact-survey-fatigue"
OUT_PATH="/rds/general/user/sd121/home/contact-survey-fatigue-outputs"
CONFIG_FILE="negb_gam_array.yaml"

eval "$(~/miniforge3/bin/conda shell.bash hook)"
source activate contact-survey-fatigue

cd $REPO_PATH
Rscript scripts/preproc_gam_data_array.R --config "$CONFIG_FILE" --arr_idx 10