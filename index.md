[![R-CMD-check](https://github.com/hsonne/pathlist/workflows/R-CMD-check/badge.svg)](https://github.com/hsonne/pathlist/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/hsonne/pathlist/workflows/pkgdown/badge.svg)](https://github.com/hsonne/pathlist/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/hsonne/pathlist/branch/main/graphs/badge.svg)](https://codecov.io/github/hsonne/pathlist)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/pathlist)]()

This package implements a S4 class pathlist that internally stores
a vector of file paths (as, e.g. received with dir()) as a matrix of 
path segments.  I found out that this is the most compact form to store the
paths.  The main feature of the class is the dollar function that allows
to filter paths for the value of their top-level folder.  Using the dollar
operator subsequently you can easily narrow down the list of paths.  The
class implements functions length(), head(), tail(), summary(), and show().

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'pathlist' from GitHub
remotes::install_github("hsonne/pathlist")
```
