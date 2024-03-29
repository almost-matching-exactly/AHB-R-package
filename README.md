
<!-- Comment hi.  -->
# AHB
A R package for performing matching for observational causal inference on datasets containing discrete and continuous covariates
--------------------------------------------------

## Documentation [here](https://almost-matching-exactly.github.io/AHB-R-package/)

AHB is a R package for performing matching for observational causal inference on datasets containing discrete and continuous covariates. It implements the mixed integer program algorithm for Adaptive Hyper-Boxes (AHB_MIP_matching and approximate fast algorithm for Adaptive Hyper-Boxes (AHB_fast_matching) which match treatment and control units   in unit-specific, hyper-box-shaped regions of the covariate space.  The resulting matched groups are  interpretable, because the matches are made on covariates, and high-quality, because machine learning is used to determine which covariates are important to match on.

### Installation

#### Dependencies
`AHB` requires R version (>=4.0.0). Install from [here](https://www.r-project.org/) if needed.
Required:
- utils (>=4.0.0)
- stats (>=4.0.0)
- Rcpp (>=1.0.4.6)
- bartMachine (>=1.2.5.1) or xgboost (>=1.0.0.2)
- Rcplex (>= 0.3.3) or Rglpk (>=0.6.4)

If your R version does not have these packages, install from [here](https://cran.r-project.org/web/packages/available_packages_by_name.html).



#### User Installation

Download it directly from github

