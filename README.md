
---
MASTER'S THESIS RESEARCH ARCHIVE
version 0.1 
Last updated: 16-5-2022
---

Thesis: Comparing the performance of MILC and tree-MILC to estimate and correct for multiple sources of errors in combined datasets: A simulation study

Student: Daniëlle Remmerswaal

Supervisors: Prof. Dr. Ton de Waal & Dr. Laura Boeschoten

Institution: Department of Methodology and Statistics, Utrecht University, the Netherlands.


---
# Introduction

This repository contains all necessary files to replicate the simulation study for the MSc thesis "Comparing the performance of MILC and tree-MILC to estimate and correct for multiple sources of errors in combined datasets: A simulation study" by Daniëlle Remmerswaal. 

In this project a simulation study is performed to compare the performance, in terms of accuracy and confidence interval measures, of MILC and tree-MILC. Eight different versions of a combined dataset containing 4 indicator variables and one covariate with four categories, and four latent classes are generated - they are referred to as the simulation conditions. The conditions differ regarding the following three factors:
1) level of selection error (5 or 20%)
2) level of measurement error (5 or 20%)
3) type of error the covariate correlates with (selection or measurement error).
This results in a 2 x 2 x 2 design with eight conditions.

<img src="/ThesisTableConditions.png" alt="Overview simulation conditions" style="height: 400px; width:800px;"/>



# Software requirements 
The whole project is coded with R version 4.1.3 (http://www.r-project.org}). Most important package for the simulation study is the R-package poLCA (version 1.4.1, https://cran.r-project.org/web/packages/poLCA/index.html). An overview of all used packages can be seen below.

Overview of all used packages:
dplyr 1.0.8\
confreq 1.5.6\
poLCA 1.6.0\
MASS 7.3\
resample 0.4\
vcd 1.4-9\
doRNG 1.8.2\
foreach 1.5.2\
rngtools 1.5.2\
ggplot2 3.3.5\
xtable 1.8-4\



---
# Instructions for running the scripts

The simulation study consists of three parts:
1. Generate datasets 
2. Apply MILC and tree-MILC
3. Calculate performance measures

These parts can be found in the folders `Data`, `Scripts` and `post-processing`. 
A fourth folder `Output` contains scripts to generate the tables and figures presented in the thesis.



The repository contains the following files:

| Folders/Files              | Description   |
| -----------------          | ------------- |
|Data          |Folder with scripts for data generation|
|/TOP_generate_data.R       |Script to generate the data for the simulation study|
|/Fun1a_specify_simconditions.R  |Function for specifying the simulation conditions|
|/Fun1b_simulate_dataset.R       |Function for the dataset generation|
|/Fun1c_generate_bootstraps.R    |Function to draw the bootstraps (first MILC step)|
|Scripts         |Folder with scripts for application of MILC and tree-MILC|
|/2_TOP_MILC.R                 |Script to apply MILC on the simulated datasets|
|/2_TOP_MILC_parallel.R        |Parallelised script to apply MILC| 
|/2a_LC4_model.R               |Function for MILC with 4 classes|
|/2_TOP_treeMILC.R                 |Script to apply tree-MILC on the simulated datasets|
|/2_TOP_treeMILC_parallel.R        |Parallelised script to apply tree-MILC| 
|/2b_LC2sel_model.R            |Function for the selection error part of tree-MILC|
|/2c_LC3meas_model.R            |Function for the measurement error part of tree-MILC|
|3_TOP_PerformanceMeasures       |Folder with scripts for calculation of performance measures|
|/3a_proportions.R              |Function to calculate measures for class size estimates|
|/3b_covariates.R           |Function to calculate measures for class-covariate estimates|
|Output       |Folder with scripts for producing tables and figures|
|/Tables.R             |Script to create tables of results|
|/Figures.R               |Script to create figures of the results|
|ThesisDanielleTreeMILC.pdf          |Thesis manuscript|



To reproduce the simulation study, follow these steps:

## Dataset generation (Folder `Data`)

1. Make sure your working directory is set at the right folder
2. Run the `TOP_generate_data.R` script  - set parameter `nsim` to the number of desired simulation iterations (default is 100). 
3. Save the generated datasets and the population estimates 

## Apply MILC and tree-MILC to the simulated data (Folder `Scripts`)

1. Make sure the datafile `simbootdat.RData` generated in the `Data` folder is loaded. 
2. Choose a TOP script (sequential or parallel) to run MILC with
3. Choose a TOP script (sequential or parallel) to run tree-MILC with
4. Make sure you save the results for both methods

### Parallelised versions of the scripts
The simulations take a long time, for nsim=100 it takes xx hours for MILC, and xx hours for tree-MILC to run. For this reason, parallelised scripts are designed. The workers of the cores in the scripts are divided over the conditions. Since nconds = 8 in this study, the number of cores useful to speed up the process are powers of 2 with a maximum of 2^3=8. Using 4 cores takes the same time as using 5, 6 or 7 cores, all make the process approximately 4 times as fast. 
The sequential and parallelised versions of the scripts give the same results.

## Calculate performance measures (`Folder Post-processing`)
1. Make sure the datafiles of the population estimates (from folder `Data`), and of the MILC and tree-MILC estimates (from folder `Scripts`) are loaded
2. Run script `CalculatePerformanceMeasues.R` to - as the name says-  calculate the performance measures for both methods. 
3. Save the whole workspace to be able to use it to create the figures and tables (folder `Output`)

## Present results in figures and tables (Folder `Output`)
1. Make sure the workspace with all the performance measures for MILC and tree-MILC is loaded
2. Run script `Tables` to generate the tables as presented in the appendix of the thesis
3. Run script `Plots_prop_estimates` to generate the faceted ggplots presented in the body of the thesis for the class size estimates
4. Run script `Plots_cov_estimates` to generate the faceted ggplots presented in the body of the thesis for the covariate-class relation estimates





# Ethics
All the data is simulated. The research protocol has been approved by the Ethical Review Board of the Faculty of Social and Behavioural Sciences of Utrecht University.


# Access
This archive is publicly available on Github: https://github.com/daniellerem/Thesis-treeMILC . No external data is necessary, all data is generated with R code. For any questions or comments, you can contact Daniëlle Remmerswaal (d.m.remmerswaal@uu.nl). 

