#!/bin/bash
REPO_PATH="/rds/general/user/sd121/home/contact-survey-fatigue"
OUT_PATH="/rds/general/user/sd121/home/contact-survey-fatigue-outputs"
CONFIG_FILE="negb_longit_single.yaml"

# Create main script
# TODO: Don't recycle the environment from bayes-rate-consistency
cat > "$OUT_PATH/fullproc_longit.pbs" <<EOF
#!/bin/bash
#PBS -l walltime=8:00:00
#PBS -l select=1:ncpus=6:ompthreads=1:mem=50gb

module load anaconda3/personal
source activate contact-survey-fatigue

# Move into repository
cd $REPO_PATH

# Run Stan model
Rscript scripts/run_stan_longit.R --config "$CONFIG_FILE"

# Diagnosis
Rscript scripts/postproc_checks_longit.R --config "$CONFIG_FILE"

# Postprocess
Rscript scripts/postproc_summarise_longit.R --config "$CONFIG_FILE"
EOF

# Execute main script
cd $OUT_PATH
qsub "fullproc_longit.pbs"