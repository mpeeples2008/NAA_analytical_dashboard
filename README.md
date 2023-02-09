# NAA_analytical_dashboard
Analytical Dashboard for conducting various analyses to group and assess groupings for Neutron Activation Analytical chemical compositional data, as well as X-ray fluorescence data.

The package can be installed from github:

```
remotes::install_github("mpeeples2008/NAA_analytical_dashboard")
```

The primary tool is a Shiny app that is still a work in progress, but has functional tools for reading in data from csv or Excel, imputing missing data, transforming data, conducting PCA and several versions of cluster analysis, manually assigning groups, visualizing data, and exporting the results. A script to assess group membership probabilities using Mahalanobis distances (following Neff 2002) is also included.

INAA_test.csv - test data for the scripts/app

Group_probs.R - Script for assessing probabilities of group membership using Mahalanobis distances
