---
layout: default
title: ATT
nav_order: 2
parent: Post Processing
grand_parent: API Documentation
description: ""
permalink: /api-documentation/post-processing/att
---

# ATT

<div class="code-example" markdown="1">
```r
ATT(AHB_out)
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
    <td>A numeric sclar representing the estimated average treatment effect on the treated units (ATT) for the matching data.</td>
  </tr>
</table>