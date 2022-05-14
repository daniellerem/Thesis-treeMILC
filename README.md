
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
The whole project is coded with R version 4.1.3 (http://www.r-project.org}). Most important package for the simulation study is the R-package poLCA (version 1.4.1, https://cran.r-project.org/web/packages/poLCA/index.html).

Overview of all used packages:
dplyr 1.0.8
confreq 1.5.6
poLCA 1.6.0
MASS 7.3
resample 0.4
vcd 1.4-9
doRNG 1.8.2
foreach 1.5.2
rngtools 1.5.2
ggplot2 3.3.5
xtable 1.8-4



---
# Instructions for running the scripts
The repository contains the following files:

| Folders/Files            | Description   |
| -----------------        | ------------- |
|1
|Thesis.pdf                |Thesis manuscript|



To reproduce the simulation study, follow these steps:

## Dataset generation 

## Apply MILC and tree-MILC to the simulated data

## Calculate performance measures

## Present results in figures and tables







# Access
This archive is publicly available and can be used by anyone. For any questions or comments, you can contact Dani\"elle Remmerswaal (d.m.remmerswaal@uu.nl). 

