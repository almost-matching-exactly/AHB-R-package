---
layout: default
title: AHB_MIP_match
nav_order: 2
parent: API Documentation
description: ""
permalink: /api-documentation/AHB_MIP_match
---

# AHB_MIP_match

<div class="code-example" markdown="1">
```r
AHB_MIP_match(data, holdout = 0.1, treated_column_name = "treated",
              outcome_column_name = "outcome", black_box = "BART", 
              cv = T, gamma0 = 3, gamma1 = 3, Beta = 2, m = 1, M = 1e+05, 
              n_prune = ifelse(is.numeric(holdout), round(0.1 * (1 - holdout) * nrow(data)), round(0.1 * nrow(data))))
```
</div>

## Parameters

<table class="parameters">
  <tr>
    <td><b>data:</b><br>file, Dataframe, required</td>
    <td>If holdout is not a numeric value, this is the data to be matched. If holdout is a numeric scalar between 0 and 1, that proportion of data will be made into a holdout set and only the remaining proportion of data will be matched.</td>
  </tr>
  <tr>
    <td><b>holdout:</b><br>numeric, file, Dataframe, optional (default = 0.1)</td>
    <td>Holdout data used to train the outcome model. If a numeric scalar, that proportion of data will be made into a holdout set and only the remaining proportion of data will be matched. Otherwise, if a file path or dataframe is provided, that dataset will serve as the holdout data.</td>
  </tr>
  <tr>
    <td><b>treated_column_name:</b><br>string, optional (default = 'treated')</td>
    <td>The name of the column which specifies whether a unit is treated or control.</td>
  </tr>
  <tr>
    <td><b>outcome_column_name:</b><br>string, optional (default = 'outcome')</td>
    <td>The name of the column which specifies each unit outcome.</td>
  </tr>
  <tr>
    <td><b>black_box:</b><br>string, optional (default = 'BART)</td>
    <td>Denotes the method to be used to generate outcome model Y. If "BART" and cv = F, uses dbarts::bart with keeptrees = TRUE, keepevery = 10, verbose = FALSE, k = 2 and ntree =200 and then the default predict method to estimate the outcome. If "BART" and cv = T, k and ntree will be best values from cross validation. Defaults to 'BART'. There will be multiple choices about black_box in the future.</td>
  </tr>
  <tr>
    <td><b>cv:</b><br>logical, optional (default = T)</td>
    <td>If TURE, do cross-validation on the train set to generate outcome model Y</td>
  </tr>
  <tr>
    <td><b>gamma0:</b><br>A numeric scalar, optional (default = 3)</td>
    <td>A numeric value, one of hyperparameters in global MIP that controls the weight placed on the outcome function portion of the loss.</td>
  </tr>
  <tr>
    <td><b>gamma1:</b><br>A numeric scalar, optional (default = 3)</td>
    <td>A numeric value, one of hyperparameters in global MIP that controls the weight placed on the outcome function portion of the loss.</td>
  </tr>
  <tr>
    <td><b>beta:</b><br>A numeric scalar, optional (default = 2)</td>
    <td>A numeric value, one of hyperparameters in global MIP that controls the weight placed on the outcome function portion of the loss.</td>
  </tr>
  <tr>
    <td><b>m:</b><br>A integer scalar, optional (default = 1)</td>
    <td>Determines the at least number of control units that the box contains when estimating causal effects for a single treatment unit.</td>
  </tr>
  <tr>
    <td><b>M:</b><br>A positive integer scalar, optional (default = 1e+5)</td>
    <td>Controls the weight placed on decision variable wij, which is an indicator for whether a unit is in the box.</td>
  </tr>
  <tr>
    <td><b>n_prune:</b><br>A positive inetger scalar, optional (default = 0.1* nrow(dataset to be matched))</td>
    <td>Determines the number of candidate units selected to run the mip on for constructing the box. Dataset mentioned below is refered to the dataset for matching. If you match a small dataset with the number of units smaller than 400, it will run MIP on all dataset for each treated unit. If you match larger dataset and your memory of your computer cannot support such much computation, plase adjust n_prune below 400 or even smaller. The smaller number of candidate units selected to run the mip on for constructing the box, the faster this program runs.</td>
  </tr>
</table>

## Returns

<table>
  <tr>
    <td><b>$data:</b><br/>dataframe</td>
    <td>Data set that was matched by AHB_MIP_match(). If holdout is not a numeric value, then <b><span>$</span>data</b> is the same as the data input into AHB_MIP_match(). If holdout is a numeric scalar between 0 and 1, <b><span>$</span>data</b> is the remaining proportion of data that were matched.</td>
  </tr>
  <tr>
    <td><b>$units_id:</b><br/>integer vector</td>
    <td>A integer vector with unit_id for test treated units</td>
  </tr>
  <tr>
    <td><b>$CATE:</b><br/>numeric vector</td>
    <td>A numeric vector with the conditional average treatment effect estimates for every test treated unit in its matched group in <b><span>$</span>MGs</b></td>
  </tr>
  <tr>
    <td><b>$bins:</b><br/>numeric vector</td>
    <td>A numeric vector with the conditional average treatment effect estimates for every test treated unit in its matched group in <b><span>$</span>MGs</b></td>
  </tr>
  <tr>
    <td><b>$MGs:</b><br/>list</td>
    <td>A list of all the matched groups formed by AHB_MIP_match(). For each test treated unit, each row contains all unit_id of the other units that fall into its box, including itself.</td>
  </tr>
</table>