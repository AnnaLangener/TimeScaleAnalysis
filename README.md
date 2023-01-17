### Code used for "It's all about timing: Exploring different temporal resolutions for analyzing digital phenotyping data"

Here we provide the code that was used for the paper "It's all about
timing: Exploring different temporal resolutions for analyzing digital
phenotyping data".

An example data set and code that is written completely in R can be
found here: <https://annalangener.github.io/TimeScaleAnalysis_Example/>.

This repository contains the following files

**Scripts for data cleaning:**

1.  CleaningNew.R: was used to clean the ESM data (based on the raw
    mpath files)

2.  SocialContext_BehappID.py: Was used to add the BehappID to the
    cleaned ESM dataset

3.  Match with ESM data.py: Was used to **aggregate the passive
    smartphone measures on different time scales and to use different
    missing data handling strategies.**

4.  data_matching.R: Was used to match the ESM and BehappID

**Scripts for data analysis:**

1.  TimeScalePaper.Rmd: Was used for the main analysis of the paper

2.  ParallelComputingML.R: Was used to get the results for Example 4
    (prediction model)

**Scripts used for the supplementary material:**

1.  Calculation Correlation.R (NOT CHECKED): Was used to calculate the
    correlations for different passive measures and positive affect for
    different participants

2.  ParallelComputingML_supplementary.R: Was used to calculate Example 4
    (prediciton model) for another participant that had missing data
