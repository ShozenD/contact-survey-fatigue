#!/bin/bash
REPO_PATH="/rds/general/user/sd121/home/contact-survey-fatigue"
OUT_PATH="/rds/general/user/sd121/home/contact-survey-fatigue-outputs"
CONFIG_FILE="zip_horseshoe_balanced.yaml"

# Create main script
cat > "$OUT_PATH/fullproc_varselect_array.pbs" <<EOF
#!/bin/bash
#PBS -l walltime=04:00:00
#PBS -l select=1:ncpus=4:ompthreads=1:mem=50gb
#PBS -J 1-28

module load anaconda3/personal
source activate contact-survey-fatigue

# Move into repository
cd $REPO_PATH

# Run Stan model
Rscript scripts/run_stan_varselect_array.R --config "$CONFIG_FILE" --arr_idx \$PBS_ARRAY_INDEX

# post-fit diagnostics
Rscript scripts/postproc_checks_varselect_array.R --config "$CONFIG_FILE" --arr_idx \$PBS_ARRAY_INDEX
EOF

# Execute main script
cd $OUT_PATH
qsub "fullproc_varselect_array.pbs"