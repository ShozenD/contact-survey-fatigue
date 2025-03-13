#!/bin/bash
REPO_PATH="/rds/general/user/sd121/home/contact-survey-fatigue"
OUT_PATH="/rds/general/user/sd121/home/contact-survey-fatigue-outputs"
CONFIG_FILE="negb_gam_array.yaml"

# Create main script
cat > "$OUT_PATH/postproc_gam_array.pbs" <<EOF
#!/bin/bash
#PBS -l walltime=01:00:00
#PBS -l select=1:ncpus=4:ompthreads=1:mem=50gb
#PBS -J 1-21

module load anaconda3/personal
source activate contact-survey-fatigue

# Move into repository
cd $REPO_PATH

# Postprocess
Rscript scripts/postproc_gam_array.R --config "$CONFIG_FILE" --arr_idx \$PBS_ARRAY_INDEX
EOF

# Execute main script
cd $OUT_PATH
qsub "postproc_gam_array.pbs"