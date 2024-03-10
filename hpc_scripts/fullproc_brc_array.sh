#!/bin/bash
REPO_PATH="/rds/general/user/sd121/home/contact-survey-fatigue"
OUT_PATH="/rds/general/user/sd121/home/contact-survey-fatigue-outputs"
CONFIG_FILE="negb_brc_m52_array.yaml"

# Create main script
cat > "$OUT_PATH/fullproc_brc_array.pbs" <<EOF
#!/bin/bash
#PBS -l walltime=08:00:00
#PBS -l select=1:ncpus=4:ompthreads=1:mem=50gb
#PBS -J 1-21

module load anaconda3/personal
source activate contact-survey-fatigue

# Move into repository
cd $REPO_PATH

# Create datasets
Rscript scripts/preproc_brc_data_array.R --config "$CONFIG_FILE" --arr_idx \$PBS_ARRAY_INDEX

# Run Stan model
Rscript scripts/run_stan_brc_array.R --config "$CONFIG_FILE" --arr_idx \$PBS_ARRAY_INDEX

# Postprocess
Rscript scripts/postproc_summarise_brc_array.R --config "$CONFIG_FILE" --arr_idx \$PBS_ARRAY_INDEX
EOF

# Execute main script
cd $OUT_PATH
qsub "fullproc_brc_array.pbs"