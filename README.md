# ivf_mandate_cdc2018

This repository contains files relevant for the demonstration reported in a letter to the editor of the *American Journal of Obstetrics & Gynecology* regarding the analysis conducted by [Peipert et al.](https://pubmed.ncbi.nlm.nih.gov/35283088/) in "Impact of comprehensive state insurance mandates on in vitro fertilization utilization, embryo transfer practices, and outcomes in the United States". The data and code used in trying to replicate the numbers reported in the letter to the editor are included here. Data were downloaded from [this CDC website](https://www.cdc.gov/art/reports/archive.html) on May 25, 2022. 

We could not replicate exactly the percentages and chi-square p-values reported in Peipert et al. This could be due to differences in handling of missing data, as the CDC reports a "\*" for values > 1 and < 5 (e.g., > 1 but < 5 intended retrievals at a given clinic). For the purposes of this demonstration only, those clinics were excluded from the analysis.  


**Live birth rate per intended retrieval, stratified by age group (years)**
<table>
 <thead>
  <tr>
   <th style="text-align:left;">  </th>
   <th style="text-align:left;"> Overall </th>
   <th style="text-align:left;"> &lt;35 </th>
   <th style="text-align:left;"> 35-37 </th>
   <th style="text-align:left;"> 38-40 </th>
   <th style="text-align:left;"> 41-42 </th>
   <th style="text-align:left;"> 43+ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Comprehensive mandate </td>
   <td style="text-align:left;"> 35.9% </td>
   <td style="text-align:left;"> 52.7% </td>
   <td style="text-align:left;"> 40.0% </td>
   <td style="text-align:left;"> 24.2% </td>
   <td style="text-align:left;"> 12.3% </td>
   <td style="text-align:left;"> 4.2% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Noncomprehensive mandate </td>
   <td style="text-align:left;"> 34.4% </td>
   <td style="text-align:left;"> 51.9% </td>
   <td style="text-align:left;"> 37.8% </td>
   <td style="text-align:left;"> 23.4% </td>
   <td style="text-align:left;"> 10.2% </td>
   <td style="text-align:left;"> 2.3% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P value from Chi-square test </td>
   <td style="text-align:left;"> <0.001 </td>
   <td style="text-align:left;"> 0.14 </td>
   <td style="text-align:left;"> <0.001 </td>
   <td style="text-align:left;"> 0.15 </td>
   <td style="text-align:left;"> <0.001 </td>
   <td style="text-align:left;"> <0.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P value from GEE </td>
   <td style="text-align:left;"> 0.01 </td>
   <td style="text-align:left;"> 0.27 </td>
   <td style="text-align:left;"> 0.35 </td>
   <td style="text-align:left;"> 0.21 </td>
   <td style="text-align:left;"> 0.09 </td>
   <td style="text-align:left;"> 0.002 </td>
  </tr>
</tbody>
</table>

![](https://github.com/katcorr/ivf-mandate-cdc2018/blob/main/fig1.png)
