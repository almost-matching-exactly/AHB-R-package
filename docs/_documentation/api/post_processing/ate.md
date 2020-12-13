---
layout: default
title: ATE
nav_order: 1
parent: Post Processing
grand_parent: API Documentation
description: ""
permalink: /api-documentation/post-processing/ate
---

# ATE

<div class="code-example" markdown="1">
```r
ATE(AHB_out)
```
</div>

## Parameters

<table class="parameters">
  <tr>
    <td><b>AHB_out:</b><br>file, Dataframe, required</td>
    <td>The output of a call to one of the AHB algorithms.</td>
  </tr>
</table>

## Returns

<table>
  <tr>
    <td><b>ATE:</b><br/>numeric scalar</td>
    <td>A numeric sclar representing the estimated average treatment effect (ATE) for the matching data.</td>
  </tr>
</table>