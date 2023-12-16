#!/bin/bash
REPO_PATH="/rds/general/user/sd121/home/contact-survey-fatigue"
OUT_PATH="/rds/general/user/sd121/home/contact-survey-fatigue-outputs"
CONFIG_FILE="fatigue-independent.yml"

# Create main script
cat > "$OUT_PATH/fit-model-longit.pbs" <<EOF
#!/bin/bash
#PBS -l walltime=24:00:00
#PBS -l select=1:ncpus=4:ompthreads=1:mem=100gb

module load anaconda3/personal
source activate contact-survey-fatigue

# Move into repository
cd $REPO_PATH

# Run Stan model
Rscript scripts/run_stan_longit.R --config "$CONFIG_FILE"
EOF

# Execute main script
cd $OUT_PATH
qsub "fit-model-longit.pbs"