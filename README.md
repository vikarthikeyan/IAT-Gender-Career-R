# IAT-Gender-Career-R

Building Gaussian Additive Models (GAMs) to understand the implicit biases of people when it comes to Gender-career associations. The goal of this project is to try to make sense of a few societal characteristics using the Implicit Association Test data by Harvard. 

1. What is the population distribution in the US for those who are biased towards Male-Career and Female-Family, vice-versa?
2. What is the state and region-wise biases in the US for Gender-Career?
3. How do we go towards a more netural stand in the future?

## Data points : 2007-2017

## Pre-requisites:

1. Before cloning the repo, install `git-lfs`. This will download the large .sav and .csv files in this repo. `https://git-lfs.github.com/`
2. Install RStudio 

## R Scripts:

Basic pre-processing scripts of RAW data obtained from IAT dataset - `preprocessing/*.R`
Analysis and plots for implicit Gender-Career bias - `implicit.R`
Analysis and plots for explicit Gender-Career bias - `explicit.R`
Generating weekly data for google correlate - `google_correlate.R`

# 
### Weekly aggregates for google correlate is available in `inv_weekly_2007_2017.csv` and `weekly_2007_2017.csv`

