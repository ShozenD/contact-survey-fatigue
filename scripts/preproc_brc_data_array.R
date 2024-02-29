# Import libraries
library(optparse)
library(yaml)
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(devtools)
load_all()

# ========== Parse command line arguments ==========
option_list <- list(
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file"),
  make_option(c("--arr_idx"), type = "integer", default = NA, help = "pbs array index", dest = "arr_idx")
)
cli_args <- parse_args(OptionParser(option_list = option_list))

# ========== Load data ==========
cat(" Loading data and configurations...\n")
covimod_data <- read_rds("./data/COVIMOD/COVIMOD_data_2022-12-29.rds")
config <- read_yaml(file.path("config", cli_args$config_file))
REPEAT <- cli_args$arr_idx - 1

# Unpack data
dt_part <- data.table(covimod_data$part)
dt_hh <- data.table(covimod_data$hh)
dt_nhh <- data.table(covimod_data$nhh)
dt_pop <- data.table(covimod_data$pop)

# ========== Data preprecoessing ==========
# Count the number of previous participations for each participant
setkeyv(dt_part, cols = c("new_id", "wave"))
dt_part[, rep := seq_len(.N) - 1, by = .(new_id)]

dt_part <- dt_part[rep <= REPEAT]

# If age_strata is 85+ change it to 80-84
dt_part[age_strata == "85+", age_strata := "80-84"]
dt_hh[alter_age_strata == "85+", alter_age_strata := "80-84"]
dt_nhh[alter_age_strata == "85+", alter_age_strata := "80-84"]

# Extract data for wave 21
dt_part <- dt_part[wave == 21]
dt_hh <- dt_hh[wave == 21]
dt_nhh <- dt_nhh[wave == 21]

# Sort dt_part by new_id
dt_part <- dt_part[order(new_id)]

# Remove participants with missing values in the gender column
dt_part <- dt_part[!is.na(gender)]

# Remove participants with missing values in the age_strata column
dt_part <- dt_part[!is.na(age_strata)]

# Impute missing ages for children
dt_part <- fill_missing_child_ages(dt_part, seed = 123)


### Prepare contact count vector Y

# Create a grid of all possible combinations of participant IDs and alter_age_strata
# Extract a sorted vector of unique participant IDs from dt_part
part_ids <- unique(dt_part$new_id)

# Extract a vector of unique alter_age_strata from dt_nhh without missing values
alter_age_strata <- unique(dt_nhh[!is.na(alter_age_strata), alter_age_strata])

# Create a data.table with all possible combinations of participant IDs and alter_age_strata
dt_grid <- data.table(expand.grid(new_id = part_ids, alter_age_strata = alter_age_strata))
print(nrow(dt_grid))

# Append the imp_age column of dt_part to dt_grid
dt_grid <- merge(dt_grid, dt_part[, .(new_id, imp_age)], by = "new_id", all.x = TRUE)

# For nhh Count the number of rows (contacts) by participant and alter_age_strata
dt_nhh_sum <- dt_nhh[, .(y_nhh = .N), by = .(new_id, alter_age_strata)]

# For hh sum the number of rows (contacts) by participant and alter_age_strata
dt_hh_sum <- dt_hh[, .(y_hh = sum(hh_met_this_day)), by = .(new_id, alter_age_strata)]

# Merge dt_nhh_sum and dt_hh_sum to dt_grid by left join
dt_grid <- merge(dt_grid, dt_nhh_sum, by = c("new_id", "alter_age_strata"), all.x = TRUE)
dt_grid <- merge(dt_grid, dt_hh_sum, by = c("new_id", "alter_age_strata"), all.x = TRUE)

# Set NA in y_nhh and y_hh to 0
dt_grid[is.na(y_nhh), y_nhh := 0]
dt_grid[is.na(y_hh), y_hh := 0]

# Add the columns y_nhh and y_hh and save it in a new column named y
dt_grid[, y := y_nhh + y_hh]

# Make participant IDX
dt_grid[, part_idx := .GRP, by = new_id]

## Prepare participant characteristics X
### Occupations

jobs_of_interest <- c("Full-time parent, homemaker",
                      "Long-term sick or disabled",
                      "Unemployed and not looking for a job",
                      "Unemployed but looking for a job")
dt_part[, job_2 := ifelse(job %in% jobs_of_interest, as.character(job), "Other")]

dt_dummies <- fastDummies::dummy_cols(dt_part[, .(new_id, job_2)],
                                      select_columns = "job_2",
                                      remove_selected_columns = TRUE,
                                      omit_colname_prefix = TRUE)
dt_dummies[, Other := NULL] # Remove the Other column
dt_dummies <- dt_dummies[,-1]


## Prepare repeat effects dummy
# Age variables
age_strata_of_interest <- c("0-4", "5-9", "10-14", "15-19", "20-24", "45-54", "75-79")
dt_part[, age_strata_2 := ifelse(age_strata %in% age_strata_of_interest, as.character(age_strata), "Other")]
dt_rep_dummy_age <- fastDummies::dummy_cols(dt_part[, .(new_id, age_strata_2)],
                                            select_columns = "age_strata_2",
                                            remove_selected_columns = TRUE,
                                            omit_colname_prefix = TRUE)
dt_rep_dummy_age[, Other := NULL] # Remove the Other column

# Jobs
jobs_of_interest <- c("Employed full-time (34 hours or more)",
                      "Employed part-time (less than 34 hours)",
                      "Self-employed",
                      "Unemployed and not looking for a job")
dt_part[, job_2 := ifelse(job %in% jobs_of_interest, as.character(job), "Other")]
dt_rep_dummy_job <- fastDummies::dummy_cols(dt_part[, .(new_id, job_2)],
                                            select_columns = "job_2",
                                            remove_selected_columns = TRUE,
                                            omit_colname_prefix = TRUE)
dt_rep_dummy_job[, Other := NULL] # Remove the Other column

dt_rep_dummy <- merge(dt_rep_dummy_age, dt_rep_dummy_job)
rep_matrix <- as.matrix(dt_rep_dummy[, -1])
job_idx <- rep(0, nrow(rep_matrix))
for (i in 1:nrow(rep_matrix)) {
  if (sum(rep_matrix[i,]) > 0) {
    job_idx[i] <- last(which(rep_matrix[i,] > 0, arr.ind = TRUE))
  }
}

## Prepare indexes and maps
### Age and age strata index set

dt_grid[, age_idx := imp_age + 1]
dt_grid[, age_idx := ifelse(age_idx > 85, 85, age_idx)]
dt_grid[, alter_age_strata_idx := as.numeric(alter_age_strata)]

age_idxset <- as.matrix(dt_grid[, .(age_idx, alter_age_strata_idx)])

### Age strata map

age_strata_map <- make_age_strata_map(dt_grid$alter_age_strata, 85, 13)


## Prepare offsets
grid <- as.data.table(expand.grid(age_idx = 1:85))
tmp <- distinct(dt_grid[, .(new_id, age_idx)])
tmp <- tmp[, .(N = .N), by = age_idx]
dt_N <- merge(grid, tmp, by = "age_idx", all.x = TRUE)
dt_N[, N := ifelse(is.na(N), 1, N)]

offN <- dt_N$N

dt_pop <- dt_pop[, .(pop = sum(pop)), by = age]
offP <- dt_pop[age <= 84, pop]

# ===== Contacts with missing age strata information =====
# In dt_nhh_sum and dt_hh_sum extract the rows where alter_age_strata is NA
dt_nhh_na <- dt_nhh_sum[is.na(alter_age_strata)]
dt_hh_na <- dt_hh_sum[is.na(alter_age_strata)]

# Remove the column alter_age_strata
dt_nhh_na[, alter_age_strata := NULL]
dt_hh_na[, alter_age_strata := NULL]

# ===== Group Contacts =====
# In dt_part, sum the values in columns Q75_u18_work to Q75_o64_else and save it as y_grp
SDcols <- colnames(dt_part)[str_detect(colnames(dt_part), "Q")]
dt_part[, y_grp := rowSums(.SD, na.rm = TRUE), .SDcols = SDcols]

# Extract the new_id and y_grp columns from dt_part and save it as dt_grp
dt_grp <- dt_part[, .(new_id, y_grp)]

# ===== Combine the three data.tables =====
dt_cnt_na <- merge(dt_grp, dt_hh_na, by = "new_id", all.x = TRUE)
dt_cnt_na <- merge(dt_cnt_na, dt_nhh_na, by = "new_id", all.x = TRUE)

dt_cnt_na[, `:=`(
  y_grp = ifelse(is.na(y_grp), 0, y_grp),
  y_hh = ifelse(is.na(y_hh), 0, y_hh),
  y_nhh = ifelse(is.na(y_nhh), 0, y_nhh))]

dt_cnt_na[, y_miss := y_grp + y_hh + y_nhh]

# ===== Compute group contact offset =====
dt_cnt_sum <- dt_grid[, .(y = sum(y)), by = new_id]
dt_cnt_sum <- merge(dt_cnt_sum, dt_cnt_na, by = "new_id", all.x = TRUE)
dt_cnt_sum[, y_miss := ifelse(y_miss > 60, 60, y_miss)]
dt_cnt_sum[, S := y / (y + y_miss)]
dt_cnt_sum[is.nan(S), S := 1]
dt_cnt_sum[S == 0, S := 1/(1 + y_miss)]

## Gather and export data
data <- list(
  # Sample size
  Nobs = nrow(dt_grid),
  Npart = nrow(dt_part),

  # Dimensions
  A = 85,
  A2 = 85*(85 + 1)/2,
  C = 13,
  P = ncol(dt_dummies),
  J = max(job_idx),
  R = max(dt_part$rep + 1),

  # Outcome
  Y = dt_grid$y,

  # Predictors
  X = as.matrix(dt_dummies),

  # Indexes
  job_idx = job_idx,
  rep_idx = dt_part$rep + 1,
  part_idx = dt_grid$part_idx,
  age_idxset = age_idxset,
  age_strata_map = age_strata_map,

  # Offsets
  offN = offN,
  offP = offP,
  offS = dt_cnt_sum$S
)

file_name <- paste("covimod-wave-21", "increp", as.character(REPEAT), sep = "-")
file_name <- paste0(file_name, ".rds")
saveRDS(data, file.path("data/silver", file_name))

cat(" DONE!\n")






