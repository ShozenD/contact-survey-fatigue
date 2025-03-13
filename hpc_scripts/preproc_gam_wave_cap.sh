#!/bin/bash
REPO_PATH="/rds/general/user/sd121/home/contact-survey-fatigue"
OUT_PATH="/rds/general/user/sd121/home/contact-survey-fatigue-outputs"
CONFIG_FILE="negb_gam_wave_cap.yaml"

# Create main script
cat > "$OUT_PATH/preproc_gam_wave_cap.pbs" <<EOF
#!/bin/bash
#PBS -l walltime=01:00:00
#PBS -l select=1:ncpus=4:ompthreads=1:mem=50gb

module load anaconda3/personal
source activate contact-survey-fatigue

# Move into repository
cd $REPO_PATH

Rscript scripts/preproc_gam_wave_cap.R --config "$CONFIG_FILE"
EOF

# Execute main script
cd $OUT_PATH
qsub "preproc_gam_wave_cap.pbs"