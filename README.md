Contents of SA_longterm_HTS

This repository contains all the code necessary to reproduce the results in the paper, entitled: 
Future HIV epidemic trajectories in South Africa and projected long-term consequences of reductions in general population HIV testing: a mathematical modelling study by Rautenbach et al., 2024. 

For details about the model please visit thembisa.org. 

The THEMBISAv18 folder contains all the model txt files which define the parameters, the C++ code (.cpp and .h) for the model and the txt outputs (for each model run). These have been edited to be able and will therefore only run on MacOS or a similar OS. 

This version of Thembisa has already been calibrated to the data. To enquire more about the code, thembisa or the calibration process please visit thembisa.org or contact Leigh Johnson (details on the website). 

The model has already been compiled to run using 1000 parameter sets. There is a unix executable file (thembisa) which is the compiled model. Should you want to make any changes to the number of parameter sets or inspect any of the model code then the thembisa code can be found in the THEMBISAv18 folder in the files thembisa.h, thembisa.cpp. 

The markdown, in THEMBISAv18 called running_scenarios.Rmd that will allow for each scenario to be recreated. It is recommended that you change the number of parameter sets to 2 to check that the model can run, this can be done by changing const int ResampleSize = 2; to const int ResampleSize = 2; on line 1030 of thembisa.h. Once this is done, the model will need to be recompiled (see Compile Thembisa in running_scenarios.Rmd.) 

For a simple check to see if the model runs without error then running use the run_thembisa() function within THEMBISAv18/R/read_and_run_orderly.R or run ./thembisa on the command line terminal from within the THEMBISAv18 folder. 

Once the scenarios are then running cleaning_csv_dfs.Rmd will prepare the data for visualisation and the figures and text results can all be reproduced using figures_text_results.Rmd. Note that these markdowns will likely not knit but all chunks can be run.  

Note that the 1000 parameter set will take several hours to run each scenario. Each combination of testing reduction year, magnitude and art/condom usage change will be one scenario and will run a baseline (status quo conditions) then the altered scenario conditions. 

It will output the following while running: 

Read input files
Completed simulation 100
Completed simulation 200
Completed simulation 300
Completed simulation 400
Completed simulation 500
Completed simulation 600
Completed simulation 700
Completed simulation 800
Completed simulation 900
Completed simulation 1000

For further enquiries please email stefan.rautenbach21@imperial.ac.uk or visit thembisa.org. 
