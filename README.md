#### LMM Case Study ####

This project consist of three parts (with corresponding R scripts):

1. Data wrangling and preparation     -> Data_prep.R
   Please do NOT run this script again!
2. Exploratory analysis               -> Exploratory_analysis.R
3. Classical Linear Mixed Model       -> Classical_LMM.R
4. Longitudinal Linear Mixed Model    -> Longitudinal_LMM.R

Please update the description above if you create more R-scripts for your analysis

Subfolders and description:

1. Data -> the original datafile and newly created datafiles
2. Plots -> All created plots

A few tips Bas gave for the analysis:

Random effects: Pens and Chickens
Fixed effects: departments, time, groups (=2 treatments)
Response: weight development over time

The pens are nested in the departments. And the chickens are nested in the pens. There are roughly 3-4 chickens per pen.

Start the analysis with a a classical LMM. Later use a random intercept/slope model (equality of variance needed?)
There are a few missing datapoints, but you can just assume they are missing completely at random (no need for a missing data mechanism)
