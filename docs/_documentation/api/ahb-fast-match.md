---
layout: default
title: AHB_fast_match
nav_order: 1
parent: API Documentation
description: ""
permalink: /api-documentation/AHB_fast_match
---

# AHB_fast_match

<div class="code-example" markdown="1">
```r
AHB_fast_match(data, holdout = 0.1, treated_column_name = "treated", 
               outcome_column_name = "outcome", black_box = "BART", 
               cv = T, C = 0.1)
```
</div>

## Parameters

<table class="parameters">
  <tr>
    <td><b>data:</b><br/>file, Dataframe, required</td>
    <td>If holdout is not a numeric value, this is the data to be matched. If holdout is a numeric scalar between 0 and 1, that proportion of data will be made into a holdout set and only the remaining proportion of data will be matched.</td>
  </tr>
  <tr>
    <td><b>holdout:</b><br/>numeric, file, Dataframe, optional (default = 0.1)</td>
    <td>Holdout data used to train the outcome model. If a numeric scalar, that proportion of data will be made into a holdout set and only the remaining proportion of data will be matched. Otherwise, if a file path or dataframe is provided, that dataset will serve as the holdout data.</td>
  </tr>
  <tr>
    <td><b>treated_column_name:</b><br/>string, optional (default = 'treated')</td>
    <td>The name of the column which specifies whether a unit is treated or control.</td>
  </tr>
  <tr>
    <td><b>outcome_column_name:</b><br/>string, optional (default = 'outcome')</td>
    <td>The name of the column which specifies each unit outcome.</td>
  </tr>
  <tr>
    <td><b>black_box</b><br/>string, optional (default = 'BART)</td>
    <td>Denotes the method to be used to generate outcome model Y. If "BART" and cv = F, uses dbarts::bart with keeptrees = TRUE, keepevery = 10, verbose = FALSE, k = 2 and ntree =200 and then the default predict method to estimate the outcome. If "BART" and cv = T, k and ntree will be best values from cross validation. Defaults to 'BART'. There will be multiple choices about black_box in the future.</td>
  </tr>
  <tr>
    <td><b>cv</b><br/>logical, optional (default = T)</td>
    <td>If TURE, do cross-validation on the train set to generate outcome model Y</td>
  </tr>
  <tr>
    <td><b>C</b><br/>A positive scalar, optional (default = 0.1)</td>
    <td>Determines the stopping condition for Fast AHB. When the variance in a newly expanded region exceeds C times the variance in the previous expansion region, the algorithm stops. Thus, higher C encourages coarser bins while lower C encourages finer ones. The user should analyze the data with multiple values of C to see how robust results are to its choice.</td>
  </tr>
</table>

## Returns

<table>
  <tr>
    <td><b>$data:</b><br/>dataframe</td>
    <td>Data set that was matched by AHB_fast_match(). If holdout is not a numeric value, then <b><span>$</span>data</b> is the same as the data input into AHB_fast_match(). If holdout is a numeric scalar between 0 and 1, <b><span>$</span>data</b> is the remaining proportion of data that were matched.</td>
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
    <td>An array of two lists where the first list contains the lower bounds and the second list contains the upper bounds for each hyper-box. Each row of each list corresponds to the hyper-box for a test treated unit in <b><span>$</span>units_id.</b></td>
  </tr>
  <tr>
    <td><b>$MGs:</b><br/>list</td>
    <td>A list of all the matched groups formed by AHB_fast_match(). For each test treated unit, each row contains all unit_id of the other units that fall into its box, including itself.</td>
  </tr>
</table>