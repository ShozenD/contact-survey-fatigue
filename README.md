## Installation
Clone the repository to your chosen directory on your local machine or server.
```{bash}
git clone git@github.com:ShozenD/contact-survey-fatigue.git
```

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