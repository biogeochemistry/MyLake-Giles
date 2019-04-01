MyLake 1.12 application for Lake Giles, with implementation in R (2019)

Written by Rachel Pilla (pillarm@miamioh.edu)

This application of MyLake is used for Lake Giles, USA. This pristine, well-protected lake has experienced long-term browning leading to a variety of physio-chemical changes in the lake. Increases in dissolved organic carbon have resulted in surface water warming, deepwater cooling, and increased strength of thermal stratification during summer, leading to deepwater oxygen depletion in recent years.

This application includes an implementation of MyLake in R. The R scripts can be used to run the MyLake application externally via Matlab, for a single run (RunMyLakeScript-ManualParameterCalibration_forGitHub.R) or auto-calibration using the Nelder-Mead method (MyLake-autoCalibrationMethods_forGitHub.R). Additional R code is included to run comparisons of modelled vs. observed water temperature and ice output, with model diagnostics, evaluations, and visualizations (ModelOutput-PlotsCompareObs_forGitHub.R).

Relevant references on long-term browning in Lake Giles:

Williamson CE, Overholt EP, Pilla RM, Leach TH, Brentrup JA, Knoll LB, Mette EM, Moeller RE. (2015). Ecological consequences of long-term browning in lakes. Scientific Reports https://doi.org/10.1038/srep18666

Pilla RM, Williamson CE, Zhang J, Smyth RL, Lenters JD, Brentrup JA, Knoll LB, Fisher TJ. (2018). Browning-related decreases in water transparency lead to long-term increases in surface water temperature and thermal stratification in two small lakes. Journal of Geophysical Research: Biogeosiences https://doi.org/10.1029/2017JG004321

Knoll LB, Williamson CE, Pilla RM, Leach TH, Brentrup JA, Fisher TJ. (2018). Browning-related oxygen depletion in an oligotrophic lake. Inland Waters https://doi.org/10.1080/20442041.2018.1452355
