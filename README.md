
---
RESEARCH ARCHIVE
---

Thesis: Comparing the performance of MILC and tree-MILC to estimate and correct for multiple sources of errors in combined datasets: A simulation study

Student: Daniëlle Remmerswaal

Supervisors: Prof. Dr. Ton de Waal & Dr. Laura Boeschoten

---
# Introduction

This repository contains all necessary files to replicate the simulation study for the MSc thesis "Comparing the performance of MILC and tree-MILC to estimate and correct for multiple sources of errors in combined datasets: A simulation study" by DaniÃ«lle Remmerswaal. 
In this project a simulation study is performed to compare the performance, in terms of accuracy and confidence interval measures, of MILC and tree-MILC. Eight different versions of a combined dataset containing 4 indicator variables and one covariate with four categories, and four latent classes are generated - they are referred to as the simulation conditions. The conditions differ regarding the following three factors:
1) level of selection error (5 or 20%)
2) level of measurement error (5 or 20%)
3) type of error the covariate correlates with (selection or measurement error).
This results in a 2 x 2 x 2 design with eight conditions.

[evt plaatje tabel met overzicht condities]


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

The first character of the name of each script is a number referring to which of the three parts the script belongs. So the names all the scripts related to data generation begin with a '1'. In the scripts starting with a '4', the code for the presentation of the results in graphs and tables can be found. 


The repository contains the following files:

| Folders/Files              | Description   |
| -----------------          | ------------- |
|1_Data_Generation           |Folder with scripts for data generation|
|/1_TOP_generate_data.R       |Script to generate the data for the simulation study|
|/1a_specify_simconditions.R  |Function for specifying the simulation conditions|
|/1b_simulate_dataset.R       |Function for the dataset generation|
|/1c_generate_bootstraps.R    |Function to draw the bootstraps (first MILC step)|
|2_MILCandtreeMILC           |Folder with scripts for application of methods|
|2_TOP_MILC.R                 |Script to apply MILC on the simulated datasets|
|2_TOP_MILC_parallel.R        |Parallelised script to apply MILC| 
|2a_LC4_model.R               |Function for MILC with 4 classes|
|2_TOP_treeMILC.R                 |Script to apply tree-MILC on the simulated datasets|
|2_TOP_treeMILC_parallel.R        |Parallelised script to apply tree-MILC| 
|2b_LC2sel_model.R            |Function for the selection error part of tree-MILC|
|2c_LC3meas_model.R            |Function for the measurement error part of tree-MILC|
|3_TOP_PerformanceMeasures       |Folder with scripts for calculation of performance measures|
|3a_proportions.R              |Function to calculate measures for class size estimates|
|3b_covariates.R           |Function to calculate measures for class-covariate estimates|
|4_Tables_and_figures        |Folder with scripts for producing tables and figures|
|4_Tables.R             |Script to create tables of results|
|4_Figures.R               |Script to create figures of the results|
|Thesis.pdf                  |Thesis manuscript|



To reproduce the simulation study, follow these steps:

## Dataset generation 

## Apply MILC and tree-MILC to the simulated data

## Calculate performance measures

## Present results in figures and tables







# Access
This archive is publicly available. No external data is necessary, . For any questions or comments, you can contact Daniëlle Remmerswaal (d.m.remmerswaal@uu.nl). 

