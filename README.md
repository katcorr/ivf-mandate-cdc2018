# ivf_mandate_cdc2018

This repository contains files relevant for the mini-analysis demonstration reported in a letter to the editor of the *American Journal of Obstetrics & Gynecology* regarding the analysis conducted by [Peipert et al.](https://pubmed.ncbi.nlm.nih.gov/35283088/) in "Impact of comprehensive state insurance mandates on in vitro fertilization utilization, embryo transfer practices, and outcomes in the United States". The data and code needed to replicate the numbers reported in the letter to the editor are included here. Data were downloaded from [this CDC website](https://www.cdc.gov/art/reports/archive.html) on May 25, 2022. 

**Live birth rate per retrieval, stratified by age group (years)**
<table>
 <thead>
  <tr>
   <th style="text-align:left;">  </th>
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
   <td style="text-align:left;"> 55.1% </td>
   <td style="text-align:left;"> 42.9% </td>
   <td style="text-align:left;"> 26.8% </td>
   <td style="text-align:left;"> 13.8% </td>
   <td style="text-align:left;"> 4.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Noncomprehensive mandate </td>
   <td style="text-align:left;"> 54.3% </td>
   <td style="text-align:left;"> 39.1% </td>
   <td style="text-align:left;"> 24.5% </td>
   <td style="text-align:left;"> 10.2% </td>
   <td style="text-align:left;"> 2.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> p value from Chi-square test </td>
   <td style="text-align:left;"> 0.14 </td>
   <td style="text-align:left;"> &lt; 0.0001 </td>
   <td style="text-align:left;"> 0.0003 </td>
   <td style="text-align:left;"> &lt; 0.0001 </td>
   <td style="text-align:left;"> &lt; 0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> p value from GEE </td>
   <td style="text-align:left;"> 0.11 </td>
   <td style="text-align:left;"> 0.37 </td>
   <td style="text-align:left;"> 0.22 </td>
   <td style="text-align:left;"> 0.009 </td>
   <td style="text-align:left;"> 0.001 </td>
  </tr>
</tbody>
</table>


![](https://raw.githubusercontent.com/katcorr/ivf-mandate-cdc2018/main/fig1.png)
