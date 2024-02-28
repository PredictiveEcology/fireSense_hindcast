---
title: "fireSense_hindcast Manual"
subtitle: "v.0.0.1"
date: "Last updated: 2024-02-28"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
  bibliography: citations/references_fireSense_hindcast.bib
link-citations: true
always_allow_html: true
---

# fireSense_hindcast Module

<!-- the following are text references used in captions for LaTeX compatibility -->
(ref:fireSense-hindcast) *fireSense_hindcast*



<!-- if knitting to pdf remember to add the pandoc_args: ["--extract-media", "."] option to yml in order to get the badge images -->

#### Authors:

Alex M Chubaty <achubaty@for-cast.ca> [aut, cre]
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

### Module summary

Provide a brief summary of what the module does / how to use the module.

Module documentation should be written so that others can use your module.
This is a template for module documentation, and should be changed to reflect your module.

### Module inputs and parameters

Describe input data required by the module and how to obtain it (e.g., directly from online sources or supplied by other modules)

Table \@ref(tab:moduleInputs-fireSense-hindcast) shows the full list of module inputs.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs-fireSense-hindcast)List of (ref:fireSense-hindcast) input objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> historicalClimateRasters </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> named list of `SpatRasters` of historical climate variables. list named after the variable and raster layers named as `year&lt;numeric year&gt;` </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> projectedClimateRasters </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> named list of `SpatRasters` of projected climate variables. list named after the variable and raster layers named as `year&lt;numeric year&gt;` </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

Provide a summary of user-visible parameters (Table \@ref(tab:moduleParams-fireSense-hindcast))


<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams-fireSense-hindcast)List of (ref:fireSense-hindcast) parameters and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> .plots </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> screen </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Used by Plots function, which can be optionally used here </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time at which the first plot event should occur. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time interval between plot events. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time at which the first save event should occur. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time interval between save events. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .studyAreaName </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Human-readable name for the study area used - e.g., a hash of the studyarea obtained using `reproducible::studyAreaName()` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .seed </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Named list of seeds to use for each event (names). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should caching of events or module be used? </td>
  </tr>
</tbody>
</table>

### Events

Describe what happens for each event type.

### Plotting

Write what is plotted.

### Saving

Write what is saved.

### Module outputs

Description of the module outputs (Table \@ref(tab:moduleOutputs-fireSense-hindcast)).

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleOutputs-fireSense-hindcast)List of (ref:fireSense-hindcast) outputs and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> historicalClimateRasters </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> named list of `SpatRasters` of historical climate variables. list named after the variable and raster layers named as `year&lt;numeric year&gt;` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> projectedClimateRasters </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> named list of `SpatRasters` of resampled historic climate variables. list named after the variable and raster layers named as `year&lt;numeric year&gt;` </td>
  </tr>
</tbody>
</table>

### Links to other modules

Describe any anticipated linkages to other modules, such as modules that supply input data or do post-hoc analysis.

### Getting help

-   <https://github.com/PredictiveEcology/fireSense_hindcast/issues>
