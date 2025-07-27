# Addressing survey fatigue bias in longitudinal social contact studies to improve pandemic preparedness
This repository contains the code and data for the paper "Addressing survey fatigue bias in longitudinal social contact studies to improve pandemic preparedness" by Shozen D. et al. (2023). The paper is available at [https://doi.org/10.1038/s41598-025-02235-0](https://doi.org/10.1038/s41598-025-02235-0).

## Abstract
Social contact surveys are an important tool to assess infection risks within populations, and the effect of non-pharmaceutical interventions on social behaviour during disease outbreaks, epidemics, and pandemics. Numerous longitudinal social contact surveys were conducted during the COVID-19 era, however data analysis is plagued by survey fatigue, a phenomenon whereby the average number of social contacts reported declines with the number of repeat participations and as participants’ engagement decreases over time. Using data from the German COVIMOD Study between April 2020 to December 2021, we demonstrate that survey fatigue varied considerably by sociodemographic factors and was consistently strongest among parents reporting children contacts (parental proxy reporting), students, middle-aged individuals, those in full-time employment and those self-employed. We find further that, when using data from first-time participants as gold standard, statistical models incorporating a simple logistic function to control for survey fatigue were associated with substantially improved estimation accuracy relative to models with no survey fatigue adjustments, and that no cap on the number of repeat participations was required. These results indicate that existing longitudinal contact survey data can be meaningfully interpreted under an easy-to-implement statistical approach addressing survey fatigue confounding, and that longitudinal designs including repeat participants are a viable option for future social contact survey designs.

## Installation
Clone the repository to your chosen directory on your local machine or server.
```{bash}
git clone https://github.com/ShozenD/contact-survey-fatigue.git
```

### Installing dependencies
This repository uses the R package `renv` to manage dependencies. First, install the `renv` on your system.
```{r}
install.packages("renv")
```
For more details on `renv`, see the [renv documentation](https://rstudio.github.io/renv/index.html).

To install the required packages, navigate to the root of the cloned directory and run the following command in R:
```{r}
renv::restore()
```

Additionally, we require the `cmdstanr` package for Bayesian inference. You can install the latest beta release with
```{r}
# we recommend running this in a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
```
One may also have to check the cmdstan toolchain and install the CmdStan software. This can be done with the following commands in R:
```{r}
cmdstanr::check_cmdstan_toolchain(fix = TRUE)
cmdstanr::install_cmdstan()
```
For up-to-date installation details and documentation on `cmdstanr`, see the [cmdstanr documentation](https://mc-stan.org/cmdstanr).

### Installing the `contactSurveyFatigue` package locally
This repository contains numerous R functions that clean, process, analyse, post-process, and visualise the data. For ease of use, we recommend installing these functions as a package. To do this, make sure you are in the root of the cloned directory and run the following command in R:
```{r}
devtools::install()
```
This will install the `contactSurveyFatigue` package locally, allowing you to use the functions in your R scripts or R Markdown documents.

Alternatively, you can use the `load_all()` function from the `devtools` package to load all functions without installing the package:
```{r}
devtools::load_all()
```

## Data
The data used in this study is available from Zenodo at [https://zenodo.org/records/15237935](https://zenodo.org/records/15237935).
For ease of use, unzip the data file, rename it to `data` and place it in the root of the cloned directory.

## Replicating the analysis
### Variable selection
To replicate the variable selection analysis (Figure 3), first preprocess the data by running `preproc_varselect_wave_4.R` and `preproc_varselect_wave_21.R` in the `scripts` directory. This will create the preprocessed data files `covimod_wave_4.rds` and `covimod_wave_21.rds` in the `data` directory.
```{bash}
Rscript scripts/preproc_varselect_wave_4.R
Rscript scripts/preproc_varselect_wave_21.R
```

To run the first stage of variable selection, execute the `run_stan_varselect_s1.R` script in the `scripts` directory. This will run the first stage of the variable selection process for both wave 4 and wave 21.
```{bash}
Rscript scripts/run_stan_varselect_s1.R
```
The script will create and save the results to a directory outside the cloned repository called `contact-survey-fatigue-outputs`, under `results/varselect/summary_stage_1.rds`.

To run the second stage of the variable selection, execute the `run_stan_varselect_s2.R`.
```{bash}
Rscript scripts/run_stan_varselect_s2.R
```
The results will be saved in the output directory under `results/varselect/summary_stage_2.rds`.

### Functional form of survey fatigue
To replicate the analysis of the functional form of survey fatigue (Figure 4), begin by preprocessing the data by running `scripts/preproc_wave_3_12.R`. This will create a file named `covimod_wave_3_12.rds` in the `data` directory.
```{bash}
Rscript scripts/preproc_wave_3_12.R
```

Next, run the `scripts/run_stan_longit.R` script to fit the Bayesian model. You can edit the configuration file contained within the `config` directory to specify the type of function to use for the survey fatigue term. Below is an example of a `yaml` file
```{yaml}
out_dir: "[/path/to/contact-survey-fatigue-outputs]" # Replace with the path to your output directory
experiment_name: "negb_longit_hill" # This can be any name you choose

model:
  name: "negb_longit_hill"
  
mcmc:
  seed: 0
  iter_warmup: 500
  iter_sampling: 2000
  chains: 4
  parallel_chains: 4
  max_treedepth: 12
  adapt_delta: 0.99
```
The `name` field under `model` must correspond to the file name of a `.stan` file in the `stan_models` directory. For example, if you want to use the Hill function for survey fatigue, the `name` should be `negb_longit_hill`, which corresponds to the file `negb_longit_hill.stan`. You can of course create your own `.stan` files and specify them in the configuration file.

Once you have set up the configuration file, run the following command in the root of the cloned directory:
```{bash}
Rscript scripts/run_stan_longit.R --config config/negb_longit_hill.yaml
```
We note that this script may take a long time to run, depending on the complexity of the model. We recommend running it on a high-performance computing cluster. However, it should also be possible to run it on a local machine with sufficient resources.

### Evaluating the accuracy of the fatigue adjustment
To run the analysis where we evaluate the accuracy of the fatigue adjustment, Documentation WIP...

### Replicating Figure 1
Documentation WIP...

## Quick Start
### For Imperial HPC
For those using Imperial's High Performance Computing Clusters, begin by following the instructions in the Conda application guide to setup Conda for your HPC account. Assuming you have done this, begin by creating an environment into which we will install the required R and dependencies.

```bash
module load anaconda3/personal
conda create -n contact-survey-fatigue r-base=4.1.3 -c conda-forge
source activate contact-survey-fatigue
```
Navigate to the root of the directory and execute `install-dependencies-hpc.R`
```bash
Rscript install-dependencies-hpc.R
```