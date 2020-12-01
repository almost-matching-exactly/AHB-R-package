---
layout: default
title: Getting Started
nav_order: 2
description: ""
permalink: /getting-started
---

# Getting Started
{: .no_toc }

<details open markdown="block">
  <summary>
    Table of contents
  </summary>
  {: .text-delta }
1. TOC
{:toc}
</details>

## Dependencies
This package requires prior installation of
- R (>= 3.3)
- Cplex (todo: check)

## Installation
The AHB R Package can be installed directly from CRAN:
<div class="code-example" markdown="1">
```r
install.packages('AHB')
```
</div>

## Input Data Format
To begin using the R-AHB algorithm, first ensure that your dataset is stored as an **R Data Frame**. Remember, covariates can either be categorical, continuous, or mixed (categorical and continuous). In addition to the covariate columns, your dataset should include a column of *binary* or *logical* data types which specify whether a unit is treated (1) or control (0) and a column of numeric data types which specify unit outcomes. Below is a sample dataset in the required format:

| x_1<br/><span style="font-weight: normal">*(numeric)*</span> | x_2<br/><span style="font-weight: normal">*(numeric)*</span> | ... | x_m<br/><span style="font-weight: normal">*(numeric)*</span> | treated<br/><span style="font-weight: normal">*(binary or logical)*</span> | outcome<br/><span style="font-weight: normal">*(numeric)*</span> |
|:-------------:|:-------------:|:---:|:-------------:|:---------------------------:|:-----------------:|
|       3       |     2.0529    | ... |     4.7905    |              1              |       4.5321      |
|       0       |     3.9932    | ... |     7.6513    |              0              |       3.3348      |
|      ...      |      ...      | ... |      ...      |             ...             |        ...        |
|       1       |     6.9321    | ... |     1.5848    |              1              |       6.9320      |

## Quickstart Example
To generate sample data for exploring AHBs functionality, use the function `gen_data` as shown below. 
Remember to load the `AHB` package a shown in line 1 before calling any of the functions discussed 
in this section. This example generates a data frame with n = 250 units and p = 5 covariates:
<div class="code-example" markdown="1">
```r
library('AHB')

data <- gen_data(n = 250, p = 5)
```
</div>

To run the algorithm, use the `AHB_fast_match` or `AHB_MIP_match` function as shown below. The required data parameter can 
either be a path to a .csv file or a dataframe. In this example, the generated dataframes are used:
<div class="code-example" markdown="1">
```r
library('AHB')

AHB_MIP_out <- AHB_MIP_match(data = data, holdout = 1.0, treated_column_name="treated", outcome_column_name="outcome")
AHB_fast_out <- AHB_fast_match(data = data, holdout = 1.0, treated_column_name="treated", outcome_column_name="outcome")
```
</div>
Take **AHB_fast_out** as an example to illustrate the output of the AHB matching algorithms. The object **AHB_fast_match** is a list of five entries:

| AHB_fast_out$data:     | Data set was matched by AHB_fast_match(). If holdout is not a numeric value, then AHB_fast_out<span>$</span>data is the same as the data input into AHB_fast_match(). If holdout is a numeric scalar between 0 and 1, AHB_fast_out<span>$</span>data is the remaining proportion of data that were matched. |
| AHB_fast_out$units_id: | A integer vector with unit_id for test treated units                                                                                                                                                                                                                             |
| AHB_fast_out$CATE:     | A numeric vector with the conditional average treatment effect estimates for every test treated unit in its matched group in AHB_fast_out$MGs                                                                                                                                    |
| AHB_fast_out$bins:     | A numeric vector with the conditional average treatment effect estimates for every test treated unit in its matched group in AHB_fast_out$MGs                                                                                                                                    |
| AHB_fast_out$MGs:      | A list of all the matched groups formed by AHB_fast_match(). For each test treated unit, each row contains all unit_id of the other units that fall into its box, including itself. 

To find the average treatment effect (ATE) or average treatment effect on the treated (ATT), use 
the functions `ATE` and `ATT`, respectively, as shown below:
<div class="code-example" markdown="1">
```r
ATE(AHB_out = AHB_fast_out)
ATT(AHB_out = AHB_fast_out)
```
</div>