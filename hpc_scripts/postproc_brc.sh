#!/bin/bash
REPO_PATH="/rds/general/user/sd121/home/contact-survey-fatigue"
OUT_PATH="/rds/general/user/sd121/home/contact-survey-fatigue-outputs"
CONFIG_FILE="negb_brc_m52_noadj.yaml"

# Create main script
# TODO: Don't recycle the environment from bayes-rate-consistency
cat > "$OUT_PATH/postproc-summarise-brc.pbs" <<EOF
#!/bin/bash
#PBS -l walltime=08:00:00
#PBS -l select=1:ncpus=4:ompthreads=1:mem=50gb

module load anaconda3/personal
source activate contact-survey-fatigue

# Move into repository
cd $REPO_PATH

# Summarise posterior quantities
Rscript scripts/postproc_summarise_brc.R --config "$CONFIG_FILE"
EOF

# Execute main script
cd $OUT_PATH
qsub "postproc-summarise-brc.pbs"