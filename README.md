# MERWACS
Machineborne Early Renal Warning And Control System


# **Overview**
MERWACS aims to offer an early warning system for Chronic Kidney Diesease screening, with only with simple and accessible parameters. The system is available as an application at https://dtu-quantitative-sustainability-assessment.shinyapps.io/MERWACS/. The application is saved at the shinyapps.io by RStudio, https://www.shinyapps.io/. The code to reproduce the Figures is available here. The code is written in RStudio.

# **System requirement**
Hardware requirements:
MERWACS does not require more than a standard computer.

**R package dependencies**

If you would like to reproduce the figures on your own machine, you need to install few packages.
R version 4.3.2 (Feather Spray), RStudio version 2024.9.0.375.

Packages:
                   
-	tidyverse (version 2.0.0)
-	ggplot2 (version 3.5.1)
-	Boruta (version 8.0.0)
-	parallel (version 4.3.2)
-	doParallel (version 1.0.17)
-	caret (version 6.0-94)
-	geomtextpath (version 0.1.5)
-	ggsci (version 3.2.0)
-	pROC (version 1.18.5)
-	yardstick (version 1.3.1)
-	viridis (version 0.6.5)
-	rsample (version 1.2.1)
-	ggh4x (version 0.3.0)
-	forcats (version 1.0.0) 
-	kernelshap (version 0.7.0)
- shapviz (version 0.9.4)
- patchwork (version 1.3.0)

# **Instructions guide**
Install R and RStudio to run the code.
Install all the required packages if needed. If R and RStudio are already installed, the whole installation time would not be greater than 10 minutes.

Please download all files in the repository.
- nhanes_final_kidney_reducedeGFR.rds file has the dataset for the final analysis.
- all_train_data_EKFC.rds file includes train set split from nhanes_final_kidney_reducedeGFR.rds.
- all_test_data_EKFC.rds file includes  test set split from nhanes_final_kidney_reducedeGFR.rds.
- knhanes_total_imputed.rds file includes imputed external validation (Korea NHANES; KNHANES).
- all_boruta_result*.rds files include the feature selection results.
- all_models*.rds files include final MERWACS models. 
- iso_fun*.rds files include post-processing isotonic regression models.
- dictionary_nhanes.rds file includes NHANES parameters dictionary.

Follow code.R file to reproduce the figures.
