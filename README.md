This package constitutes an interactive R problem set based on the RTutor package (https://github.com/skranz/RTutor). 

This is an interactive problem set based on the paper "The Opportunity Cost of Debt Aversion", published by Mike Shi and Alejandro Martínez-Marquina in 2024.

Here, you will explore the influence of debt on financial decisions through an interactive learning experience in R.

You can access the paper here: https://doi.org/10.1257/aer.20221509

Please note that you may need to change your version of the R package 'markdown' to display the problem set graphics correctly. Version 1.5 worked for me. You can do this with the 'remotes' package by typing the following in your console:

```s
# Install remotes package if not already installed
install.packages("remotes")

# Install markdown version 1.5 from the CRAN archive
remotes::install_version("markdown", version = "1.5", repos = "http://cran.r-project.org")
```
You should do this after following the steps below: 

## 1. Installation

RTutor and this package is hosted on Github. To install everything, run the following code in your R console.
```s
install.packages("RTutor",repos = c("https://skranz-repo.github.io/drat/",getOption("repos")))

if (!require(devtools))
  install.packages("devtools")

devtools::install_github("NiBre99/RTutorOpportunityCost")
```

## 2. Show and work on the problem set
To start the problem set first create a working directory in which files like the data sets and your solution will be stored. Then adapt and run the following code.
```s
library(RTutorOpportunityCost)

# Adapt your working directory to an existing folder
setwd("C:/problemsets/RTutorOpportunityCost")
# Adapt your user name
run.ps(user.name="Jon Doe", package="RTutorOpportunityCost",
       auto.save.code=TRUE, clear.user=FALSE)
```
If everything works fine, a browser window should open, in which you can start exploring the problem set.
