# MPShocks

This repository contains files used in the paper "Monetary Policy Shocks and Skill Differences in the U.S. Labor Market" (2020). 

### Paper Abstract
This paper studies the effect of monetary policy shocks on different demographic groups in the U.S. labor market. I look at the effect of a contractionary monetary policy shock on unemployment rates of high and low-skill workers, finding that the low-skill group is more sensitive to these shocks than the high-skill. Further breaking the skill groups down by gender and race, I find that female workers and non-White workers, regardless of their skill type, are more adversely affected by these shocks. Results suggest monetary policy shocks clearly have heterogeneous effects in the labor market, which should be taken into consideration while implementing monetary policy.

Paper available at https://sites.google.com/view/prithachaudhuri/research. 

### Description of files
* RRshocks_single_equation.m: this Matlab file describes how I created the monetary policy shocks following Romer and Romer (2000) "A New Measure of Monetary Policy Shocks: Derivation and Implications". The data used to create this shock measure is included in the file RomerandRomerDataAppendix in the data folder. This m file also describes the single equation regressions carried out in the paper. 
* compute_irf.m: this m file describes how to compute impulse response functions for the single equation regressions carried out by Romer and Romer (2004). 
* create_cps_data.R: 
 
