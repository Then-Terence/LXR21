# LiteXploreR, revised in 2021
R package {LXR21} for concise exploratory data analysis

## Introduction
This package compiles some functions for quick, dirty, and repetitive exploratory analysis which may otherwise lengthen the script unnecessarily. Emphasis is placed on the purpose of credit scoring, where WoE modelling (Semi Naive Bayesian Classifier) is routinely used. The functions caters to the prediction of binary target variable.

## Credits
The development of this package will not be possible without the workflow inspired by Dr. Bernd Engelmann from OSIS.

In addition, Mr. John Mount from Win Vector LLC has provided inputs of tremendous value in the computation of Area Under the ROC Curve.

## How to Download this Package

As {LXR} is hosted on GitHub, you will need to use the function
install_github() for the package {devtools} to install it.

To do that, you need to run the following lines of code in your R console:

install.packages("devtools")

library(devtools)

install_github("Then-Terence/LXR21")
