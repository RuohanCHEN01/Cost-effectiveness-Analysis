# Cost-effectiveness-Analysis
A cost-effectiveness of using Baloxavir Marboxil and Oseltamivir to treat influenza in Hong Kong.

## Java Script - AMUA
We created a decision tree using Amua, and exported the Amua decision tree to R script.

`check_base_usd_Sep11_all.amua`: decision tree model built in AMUA.\
`main.R`: R code of AMUA decision model and a list of input parameters with basecase values.

## Rstudio
We wrote R scripts to analyze cost-effectiveness.

R codes:\
`Param_all.csv`: Max & Min of parameters.\
`01_owsa.R`: Input the model structured on AMUA, calculated basic ICERs, and ran one way sensitivity analysis (OWSA).\
`02_owas_icer.R`: Calculated OWSA ICERs.\
`03_owsa_icer_table.R`: Organized table format.\
`04_tornado.R`: Plotted tornado of OWSA results.\
`05_PSA.R`: Ran probabilistic sensitivity analysis (PSA).\
`06_PSA_ICERs.R`: Ran ICERs of PSA.\
`07_Scenario_cal.R`: Ran scenario analysis.\
`08_O_Resistant_ceac.R`: Calculated result of cost-effectiveness acceptability curves (CEAC).\
`09_O_Resistant_ceac_save.R`: Organized table format.

## Output

The result outputs:\
`ceac_piv_all_save.csv`: The table of CEAC results.\
`ie.csv`: icost-iQALYs result of strategies vs. baseline.\
`ie_BvsO.csv`: icost-iQALYs result of baloxavir vs. oseltamivir.\
`mysamp.csv`: PAS input parameter.\
`owsa_output.csv`: OWSA result.\
`psa_data_result.csv`: PAS result.


## Reference

[Dampack](https://github.com/DARTH-git/dampack);
[AMUA](https://github.com/zward/Amua/wiki);
[Rstudio](https://posit.co/download/rstudio-desktop/);
[Introduction to R for cost-effectiveness analysis - Eva Enns & Zoe Kao](https://syzoekao.github.io/CEAutil/#2_decision_tree);
[Cost-effectiveness analysis](https://cran.r-project.org/web/packages/hesim/vignettes/cea.html).
