# seminar_case_studies_coolblue
GitHub repository of the code used by group 9 (Coolblue) for their research for the Seminar Case Studies in Quantitative Marketing course. 

The repository contains a total of 8 scripts, below you can find a brief description explaining each.
If you want to replicate all results, you can run all these scripts in numerical order. 
If you want to skip replicating the nested 10-fold CV in the 7th script (which takes a while), you can skip that part of the script. Though in this case you must take care to still run the first part because functions and variables defined in are needed in the 8th script.

# 1_Attribution_Data.R
Script for pre-processing the raw traffic_data and broadcast_data data sets. Must be run first.

# 2_Attribution_Functions.R
Script that defines the functions needed for the Causal impact estimation of the commercials.

# 3_Attribution_Estimation.R
Script that runs the estimation procedure for the Causal Impact of individual commercials on our response variable.
This script is very computationally intensive and uses parallel processing to speed up computation, still it might take a while to run.
This script requires the workspace resulting from running '1_Attribution_Data.R' and '2_Attribution_Functions.R' to run properly.

# 4_Modeling_Data.R
Script that prepares the broadcast data set for modeling the estimated effectiveness of TV commercials.

# 5_Merge_Attribution_Modeling_Data.R
Script that combines the transformed broadcast data set from the 4th script with our attribution results from script 3 to prepare for modeling the estimated effectiveness of TV commercials.
This script requires the workspace resutling from '3_Attribution_Estimation.R' and '4_Modeling_Data.R' to run properly.

# 6_Generate_Plots.R
Script that produces the plot used in the data section of the paper.
This script requires the workspace resulting from running '5_Merge_Attribution_Modeling_Data.R' to run properly.
This script can be skipped if one is not interested in replicating the plots.

# 7_Model_Performance_Comparison.R
Script that performs nested 10-fold Cross validation to compare the performance of three different models. 
This script is very computationally intensive and uses parallel processing to speed up computation, still it might take a while to run.
This script requires the workspace resulting from running '5_Merge_Attribution_Modeling_Data.R' to run properly.
If one is not interested in replicating the nested 10-fold Cross Validation, one does not have to run the entire script to succesfully run the 8th script.

# 8_Lasso_Model_Output.R
Script that estimates the Lasso Linear model on the entire data set.
This script requires the workspace resulting from running '5_Merge_Attribution_Modeling_Data.R' to run properly.
This script also requires at least the following objects defined in '7_Model_Performance_Comparison.R' to run properly: 'linGrid', 'trcontrollin' and functions 'data_pp', 'train_lin', 'transform_dummy' and 'normalise'.

