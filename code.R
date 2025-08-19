## Figures made on  R version 4.3.2 ##



##### Install packages #####
# Skip this if you already have installed them
install.packages(c("tidyverse", "ggplot2", "Boruta", "parallel", "doParallel", "caret", "geomtextpath",
                   "ggsci", "pROC", "yardstick", "viridis", "rsample",
                   "ggh4x", "forcats", "kernelshap", "shapviz", "patchwork"))

#### Load packages ####
library(tidyverse)
library(ggplot2)
library(Boruta)
library(parallel)
library(doParallel)
library(caret)
library(geomtextpath)
library(ggsci)
library(pROC)
library(yardstick)
library(rsample)
library(viridis)
library(ggh4x)
library(forcats)
library(patchwork)


#### Load Data and Models ####
nhanes_final_kidney_reducedeGFR <- read_rds("code/nhanes_final_kidney_reducedeGFR.rds")
all_train_data_EKFC <- read_rds("code/all_train_data_EKFC.rds")
all_test_data_EKFC <- read_rds("code/all_test_data_EKFC.rds")
all_boruta_result_EKFC <- read_rds("code/all_boruta_result_EKFC.rds")
all_boruta_result_EPI2021 <- read_rds("code/all_boruta_result_EPI2021.rds")
all_boruta_result_EPI2009 <- read_rds("code/all_boruta_result_EPI2009.rds")
dictionary_nhanes <- read_rds("code/dictionary_nhanes.rds")

all_models_EKFC_XGBOOST_train <- read_rds("code/all_models_EKFC_XGBOOST_train.rds")
all_models_EPI2021_XGBOOST_train <- read_rds("code/all_models_EPI2021_XGBOOST_train.rds")
all_models_EPI2009_XGBOOST_train <- read_rds("code/all_models_EPI2009_XGBOOST_train.rds")
iso_fun_EKFC <- read_rds("code/iso_fun_EKFC.rds")
iso_fun_EPI2021 <- read_rds("code/iso_fun_EPI2021.rds")
iso_fun_EPI2009 <- read_rds("code/iso_fun_EPI2009.rds")
knhanes_total_imputed <- read_rds("code/knhanes_total_imputed.rds")


#### Figure 1. Overview of estimated glomerular filtration rate (eGFR) and prevalence of reduced kidney health in the NHANES derivation dataset ####
nhanes_final_kidney_reducedeGFR %>% 
    gather(key, value, eGFR_EKFC, eGFR_CKD_EPI_2021, eGFR_CKD_EPI_2009) %>% 
    mutate(key = factor(key, 
                        levels = c("eGFR_EKFC", "eGFR_CKD_EPI_2021", "eGFR_CKD_EPI_2009"),
                        labels = c("EKFC", "CKD-EPI 2021", "CKD-EPI 2009"))) %>% 
    dplyr::rename(Formula = key) %>% 
    mutate(RIAGENDR = factor(RIAGENDR,
                             levels = c(2, 1),
                             labels = c("Female", "Male"))) %>%
    ggplot(aes(x = RIDAGEYR, y = value, color = Formula)) +
    geom_labelsmooth(aes(label = Formula),
                     method = "loess", formula = y ~ x,
                     size = 4, linewidth = 2, boxlinewidth = 0.3) +
    facet_wrap(~ RIAGENDR) +
    scale_x_continuous(breaks = seq(50, 100, 5)) +
    scale_y_continuous(breaks = seq(0, 140, 10), limits = c(0, 100)) + 
    scale_colour_viridis_d(option = "turbo") +
    theme_bw() +
    ylab("Estimated Glomerular Filtration Rate (eGFR)") + xlab("Age") + 
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 14, face = "bold")) -> eGFR_final


nhanes_final_kidney_reducedeGFR %>% 
    mutate(RIAGENDR = factor(RIAGENDR,
                             levels = c(2, 1),
                             labels = c("Female", "Male"))) %>%
    count(age_cat, RIAGENDR, reduced_eGFR_EKFC) %>% 
    group_by(age_cat, RIAGENDR) %>%
    mutate(pct = n / sum(n)) %>% 
    ggplot(aes(x = age_cat, y = pct, fill = reduced_eGFR_EKFC)) +
    geom_bar(position = "fill", stat = "identity", alpha = 0.85) +
    geom_text(
        aes(label = n),
        position = position_fill(vjust = 0.5),
        color = "white",
        size = 3.5,
        fontface = "bold"
    ) + 
    facet_wrap(~ RIAGENDR) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(
        name = "Kidney Health Status",
        values = c("Healthy" = "#4682B4", "Reduced" = "#CD5C5C") # Steel Blue and Indian Red
    ) +
    scale_x_discrete(labels = c("less_54" = "50-54", "less_59" = "55-59",
                                "less_64" = "60-64", "less_69" = "65-69",
                                "less_74" = "70-74", "less_79" = "75-79",
                                "less_84" = "80-84", "less_89" = "85-89",
                                "great_90" = "90+")) +
    labs(
        x = "Age Group (years)",
        y = "Percentage"
    ) +
    theme_bw() +
    theme(
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14, face = "bold")
    ) -> age_sex_group_outcome_proportion

eGFR_final / age_sex_group_outcome_proportion + 
    plot_annotation(tag_levels = 'A') &
    theme(plot.tag = element_text(face = 'bold', size = 18)) -> outcome_plots

ggsave(plot = outcome_plots, "Figure_1_outcome.png", width = 12, height = 12)


#### Figure 2. Predictor importance for reduced kidney health as determined by the Boruta algorithm ####
#1. EKFC
all_boruta_result_EKFC_fixed <- TentativeRoughFix(all_boruta_result_EKFC)
all_confirmed_features_EKFC <- names(all_boruta_result_EKFC_fixed$finalDecision)[all_boruta_result_EKFC_fixed$finalDecision == "Confirmed"]

all_boruta_result_EKFC_fixed$ImpHistory %>% 
    as_tibble() %>%
    select(all_of(all_confirmed_features_EKFC)) %>% 
    gather(key = "parameters", value = "importance") -> all_boruta_result_EKFC_detail

all_old_names_boruta_EKFC <- unique(all_boruta_result_EKFC_detail$parameters)


all_new_names_boruta_EKFC <- 
    dictionary_nhanes %>% 
    filter(variable_codename_use %in% all_old_names_boruta_EKFC) %>%
    distinct(variable_codename_use, .keep_all = T) %>% 
    mutate(variable_description_use = ifelse(variable_codename_use == "RIAGENDR", "Gender of the participant",
                                             ifelse(variable_codename_use == "RIDAGEYR", "Age in years of the participant at the time of screening",
                                                    ifelse(variable_codename_use == "BMXBMI", "Body Mass Index (kg/m²)", variable_description_use))))

# CHECK TO MAKE SURE that you have average DBP or not
if (any(grepl(pattern = "average_DBP", all_old_names_boruta_EKFC))) {
    all_new_names_boruta_EKFC <-
        all_new_names_boruta_EKFC %>% 
        bind_rows(tibble(variable_codename_use = "average_DBP", variable_description_use = "Diastolic: Average blood pressure (mm Hg)"))
}


all_boruta_result_EKFC_detail %>% 
    left_join(all_new_names_boruta_EKFC %>% select(variable_codename_use, variable_description_use), by = c("parameters" = "variable_codename_use")) %>% 
    dplyr::rename(parameters_description = variable_description_use) %>% 
    mutate(parameters_description = fct_reorder(parameters_description, importance)) -> all_boruta_result_EKFC_detail

# median importance
all_boruta_result_EKFC_detail %>% 
    group_by(parameters_description, parameters) %>% 
    dplyr::summarise(median_imp = median(importance)) %>% 
    arrange(-median_imp) %>% 
    ungroup() -> in_order_importance_EKFC


all_boruta_result_EKFC_detail %>% 
    ggplot(aes(x = parameters_description, y = importance)) +
    geom_violin(aes(color = parameters_description, fill = parameters_description), position = position_dodge(), alpha = 0.7, scale = "width") +
    xlab("Exposome") + ylab("Importance") +
    scale_colour_viridis_d(option = "turbo") +
    scale_fill_viridis_d(option = "turbo") +
    coord_flip() + 
    theme_bw() +
    theme(
        legend.position = "none"
    ) -> p_features_boruta_EKFC


#2. CKD-EPI 2021
all_boruta_result_EPI2021_fixed <- TentativeRoughFix(all_boruta_result_EPI2021)
all_confirmed_features_EPI2021 <- names(all_boruta_result_EPI2021_fixed$finalDecision)[all_boruta_result_EPI2021_fixed$finalDecision == "Confirmed"]

all_boruta_result_EPI2021_fixed$ImpHistory %>% 
    as_tibble() %>%
    select(all_of(all_confirmed_features_EPI2021)) %>% 
    gather(key = "parameters", value = "importance") -> all_boruta_result_EPI2021_detail

all_old_names_boruta_EPI2021 <- unique(all_boruta_result_EPI2021_detail$parameters)

# CHECK TO MAKE SURE that you have average DBP or not
all_new_names_boruta_EPI2021 <- 
    dictionary_nhanes %>% 
    filter(variable_codename_use %in% all_old_names_boruta_EPI2021) %>%
    distinct(variable_codename_use, .keep_all = T) %>% 
    mutate(variable_description_use = ifelse(variable_codename_use == "RIAGENDR", "Gender of the participant",
                                             ifelse(variable_codename_use == "RIDAGEYR", "Age in years of the participant at the time of screening",
                                                    ifelse(variable_codename_use == "BMXBMI", "Body Mass Index (kg/m²)", variable_description_use))))

if (any(grepl(pattern = "average_DBP", all_old_names_boruta_EPI2021))) {
    all_new_names_boruta_EPI2021 <-
        all_new_names_boruta_EPI2021 %>% 
        bind_rows(tibble(variable_codename_use = "average_DBP", variable_description_use = "Diastolic: Average blood pressure (mm Hg)"))   
}


all_boruta_result_EPI2021_detail %>% 
    left_join(all_new_names_boruta_EPI2021 %>% select(variable_codename_use, variable_description_use), by = c("parameters" = "variable_codename_use")) %>% 
    dplyr::rename(parameters_description = variable_description_use) %>% 
    mutate(parameters_description = fct_reorder(parameters_description, importance)) -> all_boruta_result_EPI2021_detail

# median importance
all_boruta_result_EPI2021_detail %>% 
    group_by(parameters_description, parameters) %>% 
    dplyr::summarise(median_imp = median(importance)) %>% 
    arrange(-median_imp) %>% 
    ungroup() -> in_order_importance_EPI2021


all_boruta_result_EPI2021_detail %>% 
    ggplot(aes(x = parameters_description, y = importance)) +
    geom_violin(aes(color = parameters_description, fill = parameters_description), position = position_dodge(), alpha = 0.7, scale = "width") +
    xlab("Exposome") + ylab("Importance") +
    scale_colour_viridis_d(option = "turbo") +
    scale_fill_viridis_d(option = "turbo") +
    coord_flip() + 
    theme_bw() +
    theme(
        legend.position = "none"
    ) -> p_features_boruta_EPI2021


#3. CKD-EPI 2009
all_boruta_result_EPI2009_fixed <- TentativeRoughFix(all_boruta_result_EPI2009)
all_confirmed_features_EPI2009 <- names(all_boruta_result_EPI2009_fixed$finalDecision)[all_boruta_result_EPI2009_fixed$finalDecision == "Confirmed"]

all_boruta_result_EPI2009_fixed$ImpHistory %>% 
    as_tibble() %>%
    select(all_of(all_confirmed_features_EPI2009)) %>% 
    gather(key = "parameters", value = "importance") -> all_boruta_result_EPI2009_detail

all_old_names_boruta_EPI2009 <- unique(all_boruta_result_EPI2009_detail$parameters)

all_new_names_boruta_EPI2009 <- 
    dictionary_nhanes %>% 
    filter(variable_codename_use %in% all_old_names_boruta_EPI2009) %>%
    distinct(variable_codename_use, .keep_all = T) %>% 
    mutate(variable_description_use = ifelse(variable_codename_use == "RIAGENDR", "Gender of the participant",
                                             ifelse(variable_codename_use == "RIDAGEYR", "Age in years of the participant at the time of screening",
                                                    ifelse(variable_codename_use == "BMXBMI", "Body Mass Index (kg/m²)", variable_description_use))))

# CHECK TO MAKE SURE that you have average DBP or not
if (any(grepl(pattern = "average_DBP", all_old_names_boruta_EPI2009))) {
    all_new_names_boruta_EPI2009 <-
        all_new_names_boruta_EPI2009 %>% 
        bind_rows(tibble(variable_codename_use = "average_DBP", variable_description_use = "Diastolic: Average blood pressure (mm Hg)"))
}


all_boruta_result_EPI2009_detail %>% 
    left_join(all_new_names_boruta_EPI2009 %>% select(variable_codename_use, variable_description_use), by = c("parameters" = "variable_codename_use")) %>% 
    dplyr::rename(parameters_description = variable_description_use) %>% 
    mutate(parameters_description = fct_reorder(parameters_description, importance)) -> all_boruta_result_EPI2009_detail

# median importance
all_boruta_result_EPI2009_detail %>% 
    group_by(parameters_description, parameters) %>% 
    dplyr::summarise(median_imp = median(importance)) %>% 
    arrange(-median_imp) %>% 
    ungroup() -> in_order_importance_EPI2009


all_boruta_result_EPI2009_detail %>% 
    ggplot(aes(x = parameters_description, y = importance)) +
    geom_violin(aes(color = parameters_description, fill = parameters_description), position = position_dodge(), alpha = 0.7, scale = "width") +
    xlab("Exposome") + ylab("Importance") +
    scale_colour_viridis_d(option = "turbo") +
    scale_fill_viridis_d(option = "turbo") +
    coord_flip() + 
    theme_bw() +
    theme(
        legend.position = "none"
    ) -> p_features_boruta_EPI2009


# Merge the three figures
p_features_boruta_EKFC / p_features_boruta_EPI2021 / p_features_boruta_EPI2009 +
    plot_annotation(tag_levels = 'A') &
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 20, face = "bold"),
          plot.tag = element_text(face = 'bold', size = 25))

ggsave("Figure_2_features.png", width = 15, height = 20)


#### Figure 3. Discrimination and calibration performance of MERWACS for the EKFC-defined outcome ####

# First, define a function to assess performance
multi_boot_warning_EKFC <- function(splits) {
    
    validation_pred <- analysis(splits)
    pr_auc_result <- 
        validation_pred %>%
        yardstick::pr_auc(truth = reduced_eGFR_EKFC, predict_test_EKFC)
    prevalence_outcome <- mean(validation_pred$reduced_eGFR_EKFC == "Reduced")
    
    total_results <- 
        pr_auc_result %>% 
        bind_rows(validation_pred %>% yardstick::roc_auc(truth = reduced_eGFR_EKFC, predict_test_EKFC),
                  validation_pred %>% yardstick::brier_class(truth = reduced_eGFR_EKFC, predict_test_EKFC) %>% mutate(.estimate = 1 - .estimate),
                  validation_pred %>% yardstick::sens(truth = reduced_eGFR_EKFC, estimate = pred_youden_EKFC),
                  validation_pred %>% yardstick::spec(truth = reduced_eGFR_EKFC, estimate = pred_youden_EKFC),
                  validation_pred %>% yardstick::accuracy(truth = reduced_eGFR_EKFC, estimate = pred_youden_EKFC),
                  validation_pred %>% yardstick::bal_accuracy(truth = reduced_eGFR_EKFC, estimate = pred_youden_EKFC))
    
    tibble(term = paste0(total_results$.metric, "_perc"),
           estimate = total_results$.estimate * 100) %>% 
        bind_rows(tibble(term = "prauc_preval_ratio", estimate = pr_auc_result$.estimate/prevalence_outcome))
}


# Find Youden's cutoff (EKFC)
library(pROC); library(yardstick)
train_probs_EKFC <- predict(all_models_EKFC_XGBOOST_train, all_train_data_EKFC, type = "prob")[, "Reduced"]

all_train_data_EKFC %>% 
    bind_cols(predict(all_models_EKFC_XGBOOST_train, all_train_data_EKFC, type = "prob") %>% 
                  dplyr::rename(probs_Reduced_EKFC = Reduced,
                                probs_Healthy_EKFC = Healthy),
              probs_calibrated_Reduced_EKFC = iso_fun_EKFC(train_probs_EKFC)) %>% 
    dplyr::mutate(probs_calibrated_Healthy_EKFC = 1 - probs_calibrated_Reduced_EKFC) -> train_to_youden_EKFC

train_to_youden_EKFC %>% roc(reduced_eGFR_EKFC, probs_calibrated_Reduced_EKFC, levels = c("Reduced", "Healthy")) -> tmp_train_youden_obj_EKFC
train_threshold_EKFC <- coords(tmp_train_youden_obj_EKFC, x = "best", 
                               ret = c("threshold", "specificity", "sensitivity")) %>% as_tibble()
train_threshold_EKFC # 0.352 cutoff

## Prepare the predicted dataset
all_test_data_wPred <- 
    all_test_data_EKFC %>%
    dplyr::mutate(
        predict_test_EKFC = predict(all_models_EKFC_XGBOOST_train, ., type = "prob")$Reduced,
        pred_raw_EKFC = predict(all_models_EKFC_XGBOOST_train, .),
        predict_test_EKFC_cal = iso_fun_EKFC(predict_test_EKFC),
        threshold_EKFC = train_threshold_EKFC$threshold,
        pred_youden_EKFC = ifelse(predict_test_EKFC_cal >= threshold_EKFC, "Reduced", "Healthy"),
        pred_youden_EKFC = factor(pred_youden_EKFC, levels = c("Reduced", "Healthy"))
    )


## Test set ##
set.seed(27)
boots_system_test_EKFC <- bootstraps(all_test_data_wPred, times = 1000, apparent = TRUE)
result_system_test_EKFC <- boots_system_test_EKFC %>% mutate(result = map(splits, multi_boot_warning_EKFC))
result_system_test_multi_EKFC <- int_pctl(result_system_test_EKFC, result)
result_system_test_multi_EKFC %>% 
    arrange(factor(term, levels = c(
        "roc_auc_perc", "pr_auc_perc", "brier_class_perc",
        "sens_perc", "spec_perc",
        "bal_accuracy_perc", "accuracy_perc",
        "prauc_preval_ratio"
    ))) %>% 
    mutate(all_together = paste0(formatC(signif(.estimate, 3), digits = 3, format = "fg", flag = "#"), " (",
                                 formatC(signif(.lower, 3), digits = 3, format = "fg", flag = "#"), "-",
                                 formatC(signif(.upper, 3), digits = 3, format = "fg", flag = "#"), ")")) -> result_system_test_multi_EKFC_clean

result_system_test_multi_EKFC_clean


result_detail_EKFC <- 
    bind_rows(result_system_test_EKFC$result[1:1000]) %>% 
    dplyr::rename(metric = term,
                  performance = estimate) %>% 
    mutate(metric = factor(metric, levels = c("roc_auc_perc", "pr_auc_perc", "brier_class_perc",
                                              "sens_perc", "spec_perc",
                                              "bal_accuracy_perc", "accuracy_perc",
                                              "prauc_preval_ratio"),
                           labels = c("100 X ROCAUC", "pr_auc_perc", "1 - Brier Score (%)",
                                      "Sensitivity (%)", "Specificity (%)",
                                      "Balanced accuracy (%)", "Accuracy (%)",
                                      "prauc_prev_ratio")))


## External dataset (KNHANES) ##
knhanes_total_imputed

## Prepare the predicted dataset (KNHANES)
knhanes_all_final_wPred <- 
    knhanes_total_imputed %>%
    mutate(
        predict_test_EKFC = predict(all_models_EKFC_XGBOOST_train, ., type = "prob")$Reduced,
        pred_raw_EKFC = predict(all_models_EKFC_XGBOOST_train, .),
        predict_test_EKFC_cal = iso_fun_EKFC(predict_test_EKFC),
        threshold_EKFC = train_threshold_EKFC$threshold,
        pred_youden_EKFC = ifelse(predict_test_EKFC_cal >= threshold_EKFC, "Reduced", "Healthy"),
        pred_youden_EKFC = factor(pred_youden_EKFC, levels = c("Reduced", "Healthy"))
    )

set.seed(27)
boots_system_KNHANES_EKFC <- bootstraps(knhanes_all_final_wPred, times = 1000, apparent = TRUE)
result_system_KNHANES_EKFC <- boots_system_KNHANES_EKFC %>% mutate(result = map(splits, multi_boot_warning_EKFC))
result_system_KNHANES_multi_EKFC <- int_pctl(result_system_KNHANES_EKFC, result)
result_system_KNHANES_multi_EKFC %>% 
    arrange(factor(term, levels = c(
        "pr_auc_perc", "roc_auc_perc", "brier_class_perc",
        "sens_perc", "spec_perc",
        "bal_accuracy_perc", "accuracy_perc",
        "prauc_preval_ratio"
    ))) %>% 
    mutate(all_together = paste0(formatC(signif(.estimate, 3), digits = 3, format = "fg", flag = "#"), " (",
                                 formatC(signif(.lower, 3), digits = 3, format = "fg", flag = "#"), "-",
                                 formatC(signif(.upper, 3), digits = 3, format = "fg", flag = "#"), ")")) -> result_system_KNHANES_multi_EKFC_clean


result_detail_KNHANES_EKFC <- 
    bind_rows(result_system_KNHANES_EKFC$result[1:1000]) %>% 
    dplyr::rename(metric = term,
                  performance = estimate) %>% 
    mutate(metric = factor(metric, levels = c("roc_auc_perc", "pr_auc_perc", "brier_class_perc",
                                              "sens_perc", "spec_perc",
                                              "bal_accuracy_perc", "accuracy_perc",
                                              "prauc_preval_ratio"),
                           labels = c("100 X ROCAUC", "pr_auc_perc", "1 - Brier Score (%)",
                                      "Sensitivity (%)", "Specificity (%)",
                                      "Balanced accuracy (%)", "Accuracy (%)",
                                      "prauc_prev_ratio")))


## Merge Internal and External
nrow_test_set <- format(nrow(all_test_data_EKFC), big.mark = ",", scientific = F)
nrow_KNHANES <- format(nrow(knhanes_total_imputed), big.mark = ",", scientific = F)
result_detail_EKFC %>%
    mutate(validation = "Internal") %>% 
    bind_rows(result_detail_KNHANES_EKFC %>% mutate(validation = "External")) %>% 
    mutate(validation = factor(validation, levels = c("Internal", "External"),
                               labels = c(paste0("Internal test dataset\n(NHANES 1998-2018; N=", nrow_test_set, ")"),
                                          paste0("External dataset\n(KNHANES 2021-2023; N=", nrow_KNHANES, ")"))),
           metric = fct_recode(metric, 
                               "100 X PRAUC" = "pr_auc_perc",
                               "Sens. (%)" = "Sensitivity (%)",
                               "Spec. (%)" = "Specificity (%)",
                               "Bal. Acc. (%)" = "Balanced accuracy (%)", 
                               "Acc. (%)" = "Accuracy (%)"),
           metric = factor(metric, levels = c("100 X ROCAUC", "100 X PRAUC", 
                                              "Sens. (%)", "Spec. (%)", 
                                              "Bal. Acc. (%)", "Acc. (%)", 
                                              "1 - Brier Score (%)", 
                                              "prauc_prev_ratio"))) -> result_detail_int_ext_EKFC


result_detail_int_ext_EKFC %>%
    filter(!metric %in% c("prauc_prev_ratio")) %>% 
    ggplot(aes(x = metric, y = performance)) +
    geom_violin(aes(color = validation, fill = validation), position = position_dodge(0.4), alpha = 0.7) +
    geom_boxplot(aes(group = interaction(metric, validation)), position = position_dodge(0.4), width = 0.1,
                 fill = "transparent", color = "black", alpha = 0.2, show.legend = F) + 
    xlab("Metric") + ylab("Performance (%)") +
    scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
    scale_colour_viridis_d(option = "turbo", name = "Validation") +
    scale_fill_viridis_d(option = "turbo", name = "Validation") +
    theme_bw() +
    theme(
        legend.position = "none", # IMPORTANT: Turn off legend for individual plot
        plot.margin = ggplot2::margin(t = 5, r = 15, l = 5, unit = "pt") # Increased 'r' (right) margin
    ) -> final_results_internal_external_EKFC


# PRAUC / Prevalence ratio plot
plot_data_EKFC <- 
    result_detail_int_ext_EKFC %>%
    filter(metric %in% c("prauc_prev_ratio")) %>%
    mutate(metric_label = factor(metric,
                                 levels = c("prauc_prev_ratio"),
                                 labels = c("PRAUC / Prevalence")))

# Create the ratio plot
final_results_PR_ratio_EKFC_custom <- 
    plot_data_EKFC %>%
    filter(metric_label == "PRAUC / Prevalence") %>% 
    ggplot(aes(x = validation, y = performance)) +
    geom_violin(aes(color = validation, fill = validation), 
                position = position_dodge(0.9), alpha = 0.7, show.legend = TRUE) +
    geom_boxplot(position = position_dodge(0.9), width = 0.1, 
                 fill = "transparent", 
                 color = "black", 
                 show.legend = FALSE) +
    
    ylab("Ratio") + 
    xlab("PRAUC / Prevalence") +
    scale_colour_viridis_d(option = "turbo", name = "Validation") +
    scale_fill_viridis_d(option = "turbo", name = "Validation") +
    scale_y_continuous(
        limits = c(0, 3.0), 
        breaks = seq(0, 3.0, 0.5)
    ) + 
    theme_bw() +
    theme(
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.x = element_blank(), # Remove x-axis text (labels for validation groups)
        axis.ticks.x = element_blank(), # Remove x-axis ticks
        axis.text.y = element_text(size = 14),
        plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt") # Increased 'l' (left) margin
    )


## Calibration
test_probs_EKFC <- predict(all_models_EKFC_XGBOOST_train, all_test_data_EKFC, type = "prob")[, "Reduced"]
KNHANES_probs_EKFC <- predict(all_models_EKFC_XGBOOST_train, knhanes_total_imputed, type = "prob")[, "Reduced"]

# Calibrated probabilities for test set
calibrated_test_probs_EKFC <- iso_fun_EKFC(test_probs_EKFC)
calibrated_test_probs_EKFC <- pmin(pmax(calibrated_test_probs_EKFC, 0), 1)
# Calibrated probabilities for KNHANES set
calibrated_test_probs_KNHANES_EKFC <- iso_fun_EKFC(KNHANES_probs_EKFC)
calibrated_test_probs_KNHANES_EKFC <- pmin(pmax(calibrated_test_probs_KNHANES_EKFC, 0), 1)

# Add this calibrated probabilities
all_test_data_wPred %>% 
    mutate(calibrated_predict_test_EKFC = calibrated_test_probs_EKFC) -> all_test_data_wPred
knhanes_all_final_wPred %>% 
    mutate(calibrated_predict_KNHANES_EKFC = calibrated_test_probs_KNHANES_EKFC) -> knhanes_all_final_wPred

# Calculate calibration
calPlotData_test_EKFC <- calibration(reduced_eGFR_EKFC ~ calibrated_predict_test_EKFC, all_test_data_wPred, cuts = 10)
calPlotData_test_EKFC <- calPlotData_test_EKFC$data %>% as_tibble()
calPlotData_test_EKFC %>% 
    dplyr::mutate(across(c(Percent:Upper, midpoint), ~ .x / 100)) -> calPlotData_test_EKFC

calPlotData_KNHANES_EKFC <- calibration(reduced_eGFR_EKFC ~ calibrated_predict_KNHANES_EKFC, knhanes_all_final_wPred, cuts = 10)
calPlotData_KNHANES_EKFC <- calPlotData_KNHANES_EKFC$data %>% as_tibble()
calPlotData_KNHANES_EKFC %>% 
    dplyr::mutate(across(c(Percent:Upper, midpoint), ~ .x / 100)) -> calPlotData_KNHANES_EKFC


calPlotData_test_EKFC %>% 
    mutate(validation = "Internal") %>% 
    bind_rows(calPlotData_KNHANES_EKFC %>% mutate(validation = "External")) %>% 
    mutate(validation = factor(validation, levels = c("Internal", "External"),
                               labels = c(paste0("Internal test dataset\n(NHANES 1998-2018; N=", nrow_test_set, ")"),
                                          paste0("External dataset\n(KNHANES 2021-2023; N=", nrow_KNHANES, ")")))) -> calPlotData_int_ext_EKFC


ggplot(calPlotData_int_ext_EKFC %>% filter(Count > 0 & !is.na(Percent)),
       aes(x = midpoint, y = Percent, color = validation)) +
    geom_smooth(se = F, method = "loess", linewidth = 1.5, show.legend = F) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    theme_bw() +
    scale_x_continuous(limits = c(0, 1.0), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1, 0.2)) +
    scale_colour_viridis_d(option = "turbo") +
    xlab("Predicted probabilities") + ylab("Observed probabilities") -> CALIB_curve_int_ext_EKFC


## Merge all figures
layout <- "
AAAAAA
BBBBBB
CCDDDD
"

guide_area() +
    (final_results_internal_external_EKFC +
         theme(axis.text.x = element_text(size = 20),
               axis.text.y = element_text(size = 20),
               axis.title.x = element_text(size = 22, face = "bold"),
               axis.title.y = element_text(size = 22, face = "bold"))) +
    (final_results_PR_ratio_EKFC_custom +
         theme(axis.text.y = element_text(size = 20),
               axis.title.x = element_text(size = 22, face = "bold"),
               axis.title.y = element_text(size = 22, face = "bold"))) +
    (CALIB_curve_int_ext_EKFC + 
         theme(axis.text.x = element_text(size = 20),
               axis.text.y = element_text(size = 20),
               axis.title.x = element_text(size = 22, face = "bold"),
               axis.title.y = element_text(size = 22, face = "bold"))) +
    plot_annotation(tag_levels = 'A') +
    plot_layout(guides = "collect", 
                design = layout,
                heights = c(0.8, 4, 4)) &
    theme(legend.position = "top",
          legend.box.margin = ggplot2::margin(b = 20, unit = "pt"),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20, face = "bold"),
          plot.tag = element_text(size = 30, face = 'bold')) -> final_results_performance_EKFC


ggsave(filename = "Figure_3_performance.png", plot = final_results_performance_EKFC, width = 16, height = 16)

#### Figure 4. Global predictor importance as visualized by SHAP summary plots ####
###### SHAP (1. EKFC) ######
library(kernelshap); library(shapviz)
all_final_features_EKFC <- all_models_EKFC_XGBOOST_train$trainingData %>% select(-.outcome) %>% names()

cores <- detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)


set.seed(123)
all_sample_for_SHAP_system_EKFC <- sample(1:nrow(all_train_data_EKFC), 2000, replace = F)

all_train_data_EKFC %>% 
    dplyr::select(all_of(all_final_features_EKFC)) %>% 
    dplyr::slice(all_sample_for_SHAP_system_EKFC) %>% 
    as.data.frame() -> all_train_data_for_SHAP_system_EKFC

set.seed(123)
all_sample_for_SHAP_system_bg_EKFC <- sample(1:nrow(all_train_data_for_SHAP_system_EKFC), 200, replace = F)

all_train_data_for_SHAP_system_EKFC %>% 
    dplyr::slice(all_sample_for_SHAP_system_bg_EKFC) %>% 
    as.data.frame() -> all_train_data_for_SHAP_system_bg_EKFC


set.seed(1)
all_shap_result_system_EKFC <- kernelshap(all_models_EKFC_XGBOOST_train, 
                                          X = all_train_data_for_SHAP_system_EKFC,
                                          bg_X = all_train_data_for_SHAP_system_bg_EKFC,
                                          type = "prob",
                                          parallel = T,
                                          verbose = T)


all_sv_system_EKFC <- shapviz(all_shap_result_system_EKFC)
all_old_names_system_EKFC <- colnames(all_sv_system_EKFC$Healthy)
dictionary_nhanes %>% 
    filter(variable_codename_use %in% all_old_names_system_EKFC) %>%
    distinct(variable_codename_use, .keep_all = T) %>% 
    select(variable_codename_use:variable_description_use) %>% 
    bind_rows(tibble(variable_codename_use = "average_DBP",
                     variable_description_use = "Diastolic: Average blood pressure (mm Hg)")) %>% 
    mutate(variable_description_use = ifelse(variable_codename_use == "RIDAGEYR", "Age in years of the participant at the time of screening",
                                             ifelse(variable_codename_use == "RIAGENDR", "Gender of the participant",
                                                    ifelse(variable_codename_use == "BMXBMI", "Body Mass Index (kg/m²)", variable_description_use)))) %>% 
    arrange(match(variable_codename_use, all_old_names_system_EKFC)) %>% 
    pull(variable_description_use) -> all_new_names_system_EKFC


colnames(all_sv_system_EKFC$Reduced) <- all_new_names_system_EKFC
colnames(all_sv_system_EKFC$Healthy) <- all_new_names_system_EKFC


all_sv_imp_system_EKFC <- sv_importance(all_sv_system_EKFC, kind = "bee", max_display = 20, viridis_args = list(option = "H"))

all_sv_system_EKFC$Healthy <- NULL
all_sv_imp_system_EKFC <- sv_importance(all_sv_system_EKFC, kind = "bee", max_display = 20, viridis_args = list(option = "H"))

all_sv_imp_system_EKFC &
    theme_classic() &
    xlab("SHAP value (prediction probability)") &
    ggtitle("Reduced Kidney Health") &
    scale_x_continuous(breaks = seq(-0.1, 0.3, 0.05)) &
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 15, face = "bold", vjust = -1),
          legend.title = element_text(size = 15, face = "bold"),
          plot.title = element_text(size = 15, face = "bold")) -> p_shap_sytem_kidney_EKFC


###### SHAP (2. CKD EPI 2021) ######
all_final_features_EPI2021 <- all_models_EPI2021_XGBOOST_train$trainingData %>% select(-.outcome) %>% names()

cores <- detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)


set.seed(123)
all_sample_for_SHAP_system_EPI2021 <- sample(1:nrow(all_train_data_EKFC), 2000, replace = F)

all_train_data_EKFC %>% 
    dplyr::select(all_of(all_final_features_EPI2021)) %>% 
    dplyr::slice(all_sample_for_SHAP_system_EPI2021) %>% 
    as.data.frame() -> all_train_data_for_SHAP_system_EPI2021

set.seed(123)
all_sample_for_SHAP_system_bg_EPI2021 <- sample(1:nrow(all_train_data_for_SHAP_system_EPI2021), 200, replace = F)

all_train_data_for_SHAP_system_EPI2021 %>% 
    dplyr::slice(all_sample_for_SHAP_system_bg_EPI2021) %>% 
    as.data.frame() -> all_train_data_for_SHAP_system_bg_EPI2021


set.seed(1)
all_shap_result_system_EPI2021 <- kernelshap(all_models_EPI2021_XGBOOST_train, 
                                             X = all_train_data_for_SHAP_system_EPI2021,
                                             bg_X = all_train_data_for_SHAP_system_bg_EPI2021,
                                             type = "prob",
                                             parallel = T,
                                             verbose = T)


all_sv_system_EPI2021 <- shapviz(all_shap_result_system_EPI2021)
all_old_names_system_EPI2021 <- colnames(all_sv_system_EPI2021$Healthy)


dictionary_nhanes %>% 
    filter(variable_codename_use %in% all_old_names_system_EPI2021) %>%
    distinct(variable_codename_use, .keep_all = T) %>% 
    select(variable_codename_use:variable_description_use) -> tmp
if (any(grepl(pattern = "average_DBP", all_old_names_system_EPI2021))) {
    tmp %>% 
        bind_rows(tibble(variable_codename_use = "average_DBP",
                         variable_description_use = "Diastolic: Average blood pressure (mm Hg)")) %>% 
        mutate(variable_description_use = ifelse(variable_codename_use == "RIDAGEYR", "Age in years of the participant at the time of screening",
                                                 ifelse(variable_codename_use == "RIAGENDR", "Gender of the participant",
                                                        ifelse(variable_codename_use == "BMXBMI", "Body Mass Index (kg/m²)", variable_description_use)))) %>% 
        arrange(match(variable_codename_use, all_old_names_system_EPI2021)) %>% 
        pull(variable_description_use) -> all_new_names_system_EPI2021
    rm(tmp)
} else {
    all_new_names_system_EPI2021 <- tmp; rm(tmp)
}


colnames(all_sv_system_EPI2021$Reduced) <- all_new_names_system_EPI2021
colnames(all_sv_system_EPI2021$Healthy) <- all_new_names_system_EPI2021


all_sv_imp_system_EPI2021 <- sv_importance(all_sv_system_EPI2021, kind = "bee", max_display = 20, viridis_args = list(option = "H"))

all_sv_system_EPI2021$Healthy <- NULL
all_sv_imp_system_EPI2021 <- sv_importance(all_sv_system_EPI2021, kind = "bee", max_display = 20, viridis_args = list(option = "H"))

all_sv_imp_system_EPI2021 &
    theme_classic() &
    xlab("SHAP value (prediction probability)") &
    ggtitle("Reduced Kidney Health") &
    scale_x_continuous(breaks = seq(-0.1, 0.3, 0.05)) &
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 15, face = "bold", vjust = -1),
          legend.title = element_text(size = 15, face = "bold"),
          plot.title = element_text(size = 15, face = "bold")) -> p_shap_sytem_kidney_EPI2021


###### SHAP (3. CKD EPI 2009) ######
all_final_features_EPI2009 <- all_models_EPI2009_XGBOOST_train$trainingData %>% select(-.outcome) %>% names()

cores <- detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

set.seed(123)
all_sample_for_SHAP_system_EPI2009 <- sample(1:nrow(all_train_data_EKFC), 2000, replace = F)

all_train_data_EKFC %>% 
    dplyr::select(all_of(all_final_features_EPI2009)) %>% 
    dplyr::slice(all_sample_for_SHAP_system_EPI2009) %>% 
    as.data.frame() -> all_train_data_for_SHAP_system_EPI2009

set.seed(123)
all_sample_for_SHAP_system_bg_EPI2009 <- sample(1:nrow(all_train_data_for_SHAP_system_EPI2009), 200, replace = F)

all_train_data_for_SHAP_system_EPI2009 %>% 
    dplyr::slice(all_sample_for_SHAP_system_bg_EPI2009) %>% 
    as.data.frame() -> all_train_data_for_SHAP_system_bg_EPI2009


set.seed(1)
all_shap_result_system_EPI2009 <- kernelshap(all_models_EPI2009_XGBOOST_train, 
                                             X = all_train_data_for_SHAP_system_EPI2009,
                                             bg_X = all_train_data_for_SHAP_system_bg_EPI2009,
                                             type = "prob",
                                             parallel = T,
                                             verbose = T)


all_sv_system_EPI2009 <- shapviz(all_shap_result_system_EPI2009)
all_old_names_system_EPI2009 <- colnames(all_sv_system_EPI2009$Healthy)


dictionary_nhanes %>% 
    filter(variable_codename_use %in% all_old_names_system_EPI2009) %>%
    distinct(variable_codename_use, .keep_all = T) %>% 
    select(variable_codename_use:variable_description_use) -> tmp
if (any(grepl(pattern = "average_DBP", all_old_names_system_EPI2009))) {
    tmp %>% 
        bind_rows(tibble(variable_codename_use = "average_DBP",
                         variable_description_use = "Diastolic: Average blood pressure (mm Hg)")) %>% 
        mutate(variable_description_use = ifelse(variable_codename_use == "RIDAGEYR", "Age in years of the participant at the time of screening",
                                                 ifelse(variable_codename_use == "RIAGENDR", "Gender of the participant",
                                                        ifelse(variable_codename_use == "BMXBMI", "Body Mass Index (kg/m²)", variable_description_use)))) %>% 
        arrange(match(variable_codename_use, all_old_names_system_EPI2009)) %>% 
        pull(variable_description_use) -> all_new_names_system_EPI2009
    rm(tmp)
} else {
    all_new_names_system_EPI2009 <- tmp; rm(tmp)
}


colnames(all_sv_system_EPI2009$Reduced) <- all_new_names_system_EPI2009
colnames(all_sv_system_EPI2009$Healthy) <- all_new_names_system_EPI2009


all_sv_imp_system_EPI2009 <- sv_importance(all_sv_system_EPI2009, kind = "bee", max_display = 20, viridis_args = list(option = "H"))

all_sv_system_EPI2009$Healthy <- NULL
all_sv_imp_system_EPI2009 <- sv_importance(all_sv_system_EPI2009, kind = "bee", max_display = 20, viridis_args = list(option = "H"))

all_sv_imp_system_EPI2009 &
    theme_classic() &
    xlab("SHAP value (prediction probability)") &
    ggtitle("Reduced Kidney Health") &
    scale_x_continuous(breaks = seq(-0.1, 0.3, 0.05)) &
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 15, face = "bold", vjust = -1),
          legend.title = element_text(size = 15, face = "bold"),
          plot.title = element_text(size = 15, face = "bold")) -> p_shap_sytem_kidney_EPI2009

## Merge all figures
p_shap_sytem_kidney_EKFC /
    p_shap_sytem_kidney_EPI2021 / 
    p_shap_sytem_kidney_EPI2009 +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(face = 'bold', size = 25))

ggsave(filename = "Figure_4_SHAP.png", width = 15, height = 20)



#### Figure 5. Case-specific interpretations of MERWACS predictions using LIME ####
library(lime)

## Find Youden's cutoff (#1 EKFC) - if you have not done this already above ##
train_probs_EKFC <- predict(all_models_EKFC_XGBOOST_train, all_train_data_EKFC, type = "prob")[, "Reduced"]

all_train_data_EKFC %>% 
    bind_cols(predict(all_models_EKFC_XGBOOST_train, all_train_data_EKFC, type = "prob") %>% 
                  dplyr::rename(probs_Reduced_EKFC = Reduced,
                                probs_Healthy_EKFC = Healthy),
              probs_calibrated_Reduced_EKFC = iso_fun_EKFC(train_probs_EKFC)) %>% 
    dplyr::mutate(probs_calibrated_Healthy_EKFC = 1 - probs_calibrated_Reduced_EKFC) -> train_to_youden_EKFC

train_to_youden_EKFC %>% roc(reduced_eGFR_EKFC, probs_calibrated_Reduced_EKFC, levels = c("Reduced", "Healthy")) -> tmp_train_youden_obj_EKFC
train_threshold_EKFC <- coords(tmp_train_youden_obj_EKFC, x = "best", 
                               ret = c("threshold", "specificity", "sensitivity")) %>% as_tibble()
train_threshold_EKFC # 0.352 cutoff

## Find Youden's cutoff (#2 CKD-EPI 2021) ##
library(pROC); library(yardstick)
train_probs_EPI2021 <- predict(all_models_EPI2021_XGBOOST_train, all_train_data_EKFC, type = "prob")[, "Reduced"]

all_train_data_EKFC %>% 
    bind_cols(predict(all_models_EPI2021_XGBOOST_train, all_train_data_EKFC, type = "prob") %>% 
                  dplyr::rename(probs_Reduced_EPI2021 = Reduced,
                                probs_Healthy_EPI2021 = Healthy),
              probs_calibrated_Reduced_EPI2021 = iso_fun_EPI2021(train_probs_EPI2021)) %>% 
    dplyr::mutate(probs_calibrated_Healthy_EPI2021 = 1 - probs_calibrated_Reduced_EPI2021) -> train_to_youden_EPI2021

train_to_youden_EPI2021 %>% roc(reduced_eGFR_EPI2021, probs_calibrated_Reduced_EPI2021, levels = c("Reduced", "Healthy")) -> tmp_train_youden_obj_EPI2021
train_threshold_EPI2021 <- coords(tmp_train_youden_obj_EPI2021, x = "best", 
                                  ret = c("threshold", "specificity", "sensitivity")) %>% as_tibble()
train_threshold_EPI2021 # 0.252 cutoff

## Find Youden's cutoff (#3 CKD-EPI 2009) ##
library(pROC); library(yardstick)
train_probs_EPI2009 <- predict(all_models_EPI2009_XGBOOST_train, all_train_data_EKFC, type = "prob")[, "Reduced"]

all_train_data_EKFC %>% 
    bind_cols(predict(all_models_EPI2009_XGBOOST_train, all_train_data_EKFC, type = "prob") %>% 
                  dplyr::rename(probs_Reduced_EPI2009 = Reduced,
                                probs_Healthy_EPI2009 = Healthy),
              probs_calibrated_Reduced_EPI2009 = iso_fun_EPI2009(train_probs_EPI2009)) %>% 
    dplyr::mutate(probs_calibrated_Healthy_EPI2009 = 1 - probs_calibrated_Reduced_EPI2009) -> train_to_youden_EPI2009

train_to_youden_EPI2009 %>% roc(reduced_eGFR_EPI2009, probs_calibrated_Reduced_EPI2009, levels = c("Reduced", "Healthy")) -> tmp_train_youden_obj_EPI2009
train_threshold_EPI2009 <- coords(tmp_train_youden_obj_EPI2009, x = "best", 
                                  ret = c("threshold", "specificity", "sensitivity")) %>% as_tibble()

train_threshold_EPI2009 # 0.284 cutoff

# Get the features' names
features_EKFC <- all_models_EKFC_XGBOOST_train$trainingData %>% select(-.outcome) %>% names()
features_EPI2021 <- all_models_EPI2021_XGBOOST_train$trainingData %>% select(-.outcome) %>% names()
features_EPI2009 <- all_models_EPI2009_XGBOOST_train$trainingData %>% select(-.outcome) %>% names()

# prepare the predicted dataset
all_test_data_wPred <- 
    all_test_data_EKFC %>%
    dplyr::mutate(
        predict_test_EKFC = predict(all_models_EKFC_XGBOOST_train, ., type = "prob")$Reduced,
        pred_raw_EKFC = predict(all_models_EKFC_XGBOOST_train, .),
        predict_test_EKFC_cal = iso_fun_EKFC(predict_test_EKFC),
        threshold_EKFC = train_threshold_EKFC$threshold,
        pred_youden_EKFC = ifelse(predict_test_EKFC_cal >= threshold_EKFC, "Reduced", "Healthy"),
        pred_youden_EKFC = factor(pred_youden_EKFC, levels = c("Reduced", "Healthy")),
        
        predict_test_EPI2021 = predict(all_models_EPI2021_XGBOOST_train, ., type = "prob")$Reduced,
        pred_raw_EPI2021 = predict(all_models_EPI2021_XGBOOST_train, .),
        predict_test_EPI2021_cal = iso_fun_EPI2021(predict_test_EPI2021),
        threshold_EPI2021 = train_threshold_EPI2021$threshold,
        pred_youden_EPI2021 = ifelse(predict_test_EPI2021_cal >= threshold_EPI2021, "Reduced", "Healthy"),
        pred_youden_EPI2021 = factor(pred_youden_EPI2021, levels = c("Reduced", "Healthy")),
        
        predict_test_EPI2009 = predict(all_models_EPI2009_XGBOOST_train, ., type = "prob")$Reduced,
        pred_raw_EPI2009 = predict(all_models_EPI2009_XGBOOST_train, .),
        predict_test_EPI2009_cal = iso_fun_EPI2009(predict_test_EPI2009),
        threshold_EPI2009 = train_threshold_EPI2009$threshold,
        pred_youden_EPI2009 = ifelse(predict_test_EPI2009_cal >= threshold_EPI2009, "Reduced", "Healthy"),
        pred_youden_EPI2009 = factor(pred_youden_EPI2009, levels = c("Reduced", "Healthy"))
    )

# Examples
all_test_data_wPred %>% 
    filter(predict_test_EKFC_cal < 0.1) %>% 
    dplyr::slice(1) %>% 
    select(all_of(features_EKFC)) -> lime_EKFC_exp_low

all_test_data_wPred %>% 
    filter(predict_test_EKFC_cal > 0.9) %>% 
    dplyr::slice(1) %>% 
    select(all_of(features_EKFC)) -> lime_EKFC_exp_high

all_test_data_wPred %>% 
    filter(predict_test_EPI2021_cal < 0.1) %>% 
    dplyr::slice(1) %>% 
    select(all_of(features_EPI2021)) -> lime_EPI2021_exp_low

all_test_data_wPred %>% 
    filter(predict_test_EPI2021_cal > 0.9) %>% 
    dplyr::slice(1) %>% 
    select(all_of(features_EPI2021)) -> lime_EPI2021_exp_high

all_test_data_wPred %>% 
    filter(predict_test_EPI2009_cal < 0.1) %>% 
    dplyr::slice(1) %>% 
    select(all_of(features_EPI2009)) -> lime_EPI2009_exp_low

all_test_data_wPred %>% 
    filter(predict_test_EPI2009_cal > 0.9) %>% 
    dplyr::slice(1) %>% 
    select(all_of(features_EPI2009)) -> lime_EPI2009_exp_high

# Model
lime_to_input_EKFC <- 
    all_models_EKFC_XGBOOST_train$trainingData %>% 
    rename(reduced_eGFR_EKFC = .outcome) %>% 
    dplyr::relocate(reduced_eGFR_EKFC, .after = last_col())

lime_to_input_EPI2021 <- 
    all_models_EPI2021_XGBOOST_train$trainingData %>% 
    rename(reduced_eGFR_EPI2021 = .outcome) %>% 
    dplyr::relocate(reduced_eGFR_EPI2021, .after = last_col())

lime_to_input_EPI2009 <- 
    all_models_EPI2009_XGBOOST_train$trainingData %>% 
    rename(reduced_eGFR_EPI2009 = .outcome) %>% 
    dplyr::relocate(reduced_eGFR_EPI2009, .after = last_col())


# Explainer
set.seed(1)
explainer_EKFC <- lime(lime_to_input_EKFC, 
                       all_models_EKFC_XGBOOST_train, 
                       bin_continuous = T,
                       n_bins = 4)
set.seed(1)
explanation_EKFC_low <- lime::explain(lime_EKFC_exp_low,
                                      explainer_EKFC, 
                                      labels = "Reduced",
                                      n_features = ncol(lime_EKFC_exp_low))
set.seed(1)
explanation_EKFC_high <- lime::explain(lime_EKFC_exp_high,
                                       explainer_EKFC, 
                                       labels = "Reduced",
                                       n_features = ncol(lime_EKFC_exp_high))


set.seed(1)
explainer_EPI2021 <- lime(lime_to_input_EPI2021, 
                          all_models_EPI2021_XGBOOST_train, 
                          bin_continuous = T,
                          n_bins = 4)
set.seed(1)
explanation_EPI2021_low <- lime::explain(lime_EPI2021_exp_low,
                                         explainer_EPI2021, 
                                         labels = "Reduced",
                                         n_features = ncol(lime_EPI2021_exp_low))
set.seed(1)
explanation_EPI2021_high <- lime::explain(lime_EPI2021_exp_high,
                                          explainer_EPI2021, 
                                          labels = "Reduced",
                                          n_features = ncol(lime_EPI2021_exp_high))

set.seed(1)
explainer_EPI2009 <- lime(lime_to_input_EPI2009, 
                          all_models_EPI2009_XGBOOST_train, 
                          bin_continuous = T,
                          n_bins = 4)
set.seed(1)
explanation_EPI2009_low <- lime::explain(lime_EPI2009_exp_low,
                                         explainer_EPI2009, 
                                         labels = "Reduced",
                                         n_features = ncol(lime_EPI2009_exp_low))
set.seed(1)
explanation_EPI2009_high <- lime::explain(lime_EPI2009_exp_high,
                                          explainer_EPI2009, 
                                          labels = "Reduced",
                                          n_features = ncol(lime_EPI2009_exp_high))


explanation_EKFC_low %>% 
    dplyr::mutate(
        label_prob = iso_fun_EKFC(label_prob),
        across(feature_desc, ~gsub("RIDAGEYR", "Age (year)", .x)),
        across(feature_desc, ~gsub("DIQ010 = 1", "History of diabetes", .x)),
        across(feature_desc, ~gsub("DIQ010 = 2", "No history of diabetes", .x)),
        across(feature_desc, ~gsub("VNAVEBPXSY", "Systolic Blood Pressure (mm Hg)", .x)),
        across(feature_desc, ~gsub("BPQ020 = 1", "History of hypertension", .x)),
        across(feature_desc, ~gsub("BPQ020 = 2", "No history of hypertension", .x)),
        across(feature_desc, ~gsub("average_DBP", "Diastolic Blood Pressure (mm Hg)", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 1", "Ethnicity: Mexican American", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 2", "Ethnicity: Other Hispanic", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 3", "Ethnicity: Non-Hispanic White", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 4", "Ethnicity: Non-Hispanic Black", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 5", "Ethnicity: Other Race - Including Multi-Racial", .x)),
        across(feature_desc, ~gsub("INDFMPIR", "Poverty Income Ratio (PIR)", .x)),
        across(feature_desc, ~gsub("BMXWAIST", "Waist Circumference (cm)", .x)),
        across(feature_desc, ~gsub("MCQ160B = 1", "History of heart failure", .x)),
        across(feature_desc, ~gsub("MCQ160B = 2", "No history of heart failure", .x)),
        across(feature_desc, ~gsub("BMXBMI", "BMI (kg/m²)", .x)),
        across(feature_desc, ~gsub("BPXPLS", "60 seconds pulse", .x)),
        across(feature_desc, ~gsub("BMXARMC", "Arm Circumference (cm)", .x)),
        across(feature_desc, ~gsub("BMXARML", "Upper Arm Length (cm)", .x)),
        across(feature_desc, ~gsub("BMXWT", "Weight (kg)", .x))
    ) %>% 
    dplyr::mutate(label = "Reduced Kidney Health") -> explanation_EKFC_low_clean

explanation_EKFC_high %>% 
    mutate(
        label_prob = iso_fun_EKFC(label_prob),
        across(feature_desc, ~gsub("RIDAGEYR", "Age (year)", .x)),
        across(feature_desc, ~gsub("DIQ010 = 1", "History of diabetes", .x)),
        across(feature_desc, ~gsub("DIQ010 = 2", "No history of diabetes", .x)),
        across(feature_desc, ~gsub("VNAVEBPXSY", "Systolic Blood Pressure (mm Hg)", .x)),
        across(feature_desc, ~gsub("BPQ020 = 1", "History of hypertension", .x)),
        across(feature_desc, ~gsub("BPQ020 = 2", "No history of hypertension", .x)),
        across(feature_desc, ~gsub("average_DBP", "Diastolic Blood Pressure (mm Hg)", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 1", "Ethnicity: Mexican American", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 2", "Ethnicity: Other Hispanic", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 3", "Ethnicity: Non-Hispanic White", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 4", "Ethnicity: Non-Hispanic Black", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 5", "Ethnicity: Other Race - Including Multi-Racial", .x)),
        across(feature_desc, ~gsub("INDFMPIR", "Poverty Income Ratio (PIR)", .x)),
        across(feature_desc, ~gsub("BMXWAIST", "Waist Circumference (cm)", .x)),
        across(feature_desc, ~gsub("MCQ160B = 1", "History of heart failure", .x)),
        across(feature_desc, ~gsub("MCQ160B = 2", "No history of heart failure", .x)),
        across(feature_desc, ~gsub("BMXBMI", "BMI (kg/m²)", .x)),
        across(feature_desc, ~gsub("BPXPLS", "60 seconds pulse", .x)),
        across(feature_desc, ~gsub("BMXARMC", "Arm Circumference (cm)", .x)),
        across(feature_desc, ~gsub("BMXARML", "Upper Arm Length (cm)", .x)),
        across(feature_desc, ~gsub("BMXWT", "Weight (kg)", .x))
    ) %>% 
    mutate(label = "Reduced Kidney Health") -> explanation_EKFC_high_clean



explanation_EPI2021_low %>% 
    mutate(
        label_prob = iso_fun_EPI2021(label_prob),
        across(feature_desc, ~gsub("RIDAGEYR", "Age (year)", .x)),
        across(feature_desc, ~gsub("DIQ010 = 1", "History of diabetes", .x)),
        across(feature_desc, ~gsub("DIQ010 = 2", "No history of diabetes", .x)),
        across(feature_desc, ~gsub("VNAVEBPXSY", "Systolic Blood Pressure (mm Hg)", .x)),
        across(feature_desc, ~gsub("BPQ020 = 1", "History of hypertension", .x)),
        across(feature_desc, ~gsub("BPQ020 = 2", "No history of hypertension", .x)),
        across(feature_desc, ~gsub("average_DBP", "Diastolic Blood Pressure (mm Hg)", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 1", "Ethnicity: Mexican American", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 2", "Ethnicity: Other Hispanic", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 3", "Ethnicity: Non-Hispanic White", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 4", "Ethnicity: Non-Hispanic Black", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 5", "Ethnicity: Other Race - Including Multi-Racial", .x)),
        across(feature_desc, ~gsub("INDFMPIR", "Poverty Income Ratio (PIR)", .x)),
        across(feature_desc, ~gsub("BMXWAIST", "Waist Circumference (cm)", .x)),
        across(feature_desc, ~gsub("MCQ160B = 1", "History of heart failure", .x)),
        across(feature_desc, ~gsub("MCQ160B = 2", "No history of heart failure", .x)),
        across(feature_desc, ~gsub("BMXBMI", "BMI (kg/m²)", .x)),
        across(feature_desc, ~gsub("BPXPLS", "60 seconds pulse", .x)),
        across(feature_desc, ~gsub("BMXARMC", "Arm Circumference (cm)", .x)),
        across(feature_desc, ~gsub("BMXARML", "Upper Arm Length (cm)", .x)),
        across(feature_desc, ~gsub("BMXWT", "Weight (kg)", .x))
    ) %>% 
    mutate(label = "Reduced Kidney Health") -> explanation_EPI2021_low_clean


explanation_EPI2021_high %>% 
    mutate(
        label_prob = iso_fun_EPI2021(label_prob),
        across(feature_desc, ~gsub("RIDAGEYR", "Age (year)", .x)),
        across(feature_desc, ~gsub("DIQ010 = 1", "History of diabetes", .x)),
        across(feature_desc, ~gsub("DIQ010 = 2", "No history of diabetes", .x)),
        across(feature_desc, ~gsub("VNAVEBPXSY", "Systolic Blood Pressure (mm Hg)", .x)),
        across(feature_desc, ~gsub("BPQ020 = 1", "History of hypertension", .x)),
        across(feature_desc, ~gsub("BPQ020 = 2", "No history of hypertension", .x)),
        across(feature_desc, ~gsub("average_DBP", "Diastolic Blood Pressure (mm Hg)", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 1", "Ethnicity: Mexican American", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 2", "Ethnicity: Other Hispanic", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 3", "Ethnicity: Non-Hispanic White", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 4", "Ethnicity: Non-Hispanic Black", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 5", "Ethnicity: Other Race - Including Multi-Racial", .x)),
        across(feature_desc, ~gsub("INDFMPIR", "Poverty Income Ratio (PIR)", .x)),
        across(feature_desc, ~gsub("BMXWAIST", "Waist Circumference (cm)", .x)),
        across(feature_desc, ~gsub("MCQ160B = 1", "History of heart failure", .x)),
        across(feature_desc, ~gsub("MCQ160B = 2", "No history of heart failure", .x)),
        across(feature_desc, ~gsub("BMXBMI", "BMI (kg/m²)", .x)),
        across(feature_desc, ~gsub("BPXPLS", "60 seconds pulse", .x)),
        across(feature_desc, ~gsub("BMXARMC", "Arm Circumference (cm)", .x)),
        across(feature_desc, ~gsub("BMXARML", "Upper Arm Length (cm)", .x)),
        across(feature_desc, ~gsub("BMXWT", "Weight (kg)", .x))
    ) %>% 
    mutate(label = "Reduced Kidney Health") -> explanation_EPI2021_high_clean


explanation_EPI2009_low %>% 
    mutate(
        label_prob = iso_fun_EPI2009(label_prob),
        across(feature_desc, ~gsub("RIDAGEYR", "Age (year)", .x)),
        across(feature_desc, ~gsub("DIQ010 = 1", "History of diabetes", .x)),
        across(feature_desc, ~gsub("DIQ010 = 2", "No history of diabetes", .x)),
        across(feature_desc, ~gsub("VNAVEBPXSY", "Systolic Blood Pressure (mm Hg)", .x)),
        across(feature_desc, ~gsub("BPQ020 = 1", "History of hypertension", .x)),
        across(feature_desc, ~gsub("BPQ020 = 2", "No history of hypertension", .x)),
        across(feature_desc, ~gsub("average_DBP", "Diastolic Blood Pressure (mm Hg)", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 1", "Ethnicity: Mexican American", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 2", "Ethnicity: Other Hispanic", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 3", "Ethnicity: Non-Hispanic White", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 4", "Ethnicity: Non-Hispanic Black", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 5", "Ethnicity: Other Race - Including Multi-Racial", .x)),
        across(feature_desc, ~gsub("INDFMPIR", "Poverty Income Ratio (PIR)", .x)),
        across(feature_desc, ~gsub("BMXWAIST", "Waist Circumference (cm)", .x)),
        across(feature_desc, ~gsub("MCQ160B = 1", "History of heart failure", .x)),
        across(feature_desc, ~gsub("MCQ160B = 2", "No history of heart failure", .x)),
        across(feature_desc, ~gsub("BMXBMI", "BMI (kg/m²)", .x)),
        across(feature_desc, ~gsub("BPXPLS", "60 seconds pulse", .x)),
        across(feature_desc, ~gsub("BMXARMC", "Arm Circumference (cm)", .x)),
        across(feature_desc, ~gsub("BMXARML", "Upper Arm Length (cm)", .x)),
        across(feature_desc, ~gsub("BMXWT", "Weight (kg)", .x))
    ) %>% 
    mutate(label = "Reduced Kidney Health") -> explanation_EPI2009_low_clean


explanation_EPI2009_high %>% 
    mutate(
        label_prob = iso_fun_EPI2009(label_prob),
        across(feature_desc, ~gsub("RIDAGEYR", "Age (year)", .x)),
        across(feature_desc, ~gsub("DIQ010 = 1", "History of diabetes", .x)),
        across(feature_desc, ~gsub("DIQ010 = 2", "No history of diabetes", .x)),
        across(feature_desc, ~gsub("VNAVEBPXSY", "Systolic Blood Pressure (mm Hg)", .x)),
        across(feature_desc, ~gsub("BPQ020 = 1", "History of hypertension", .x)),
        across(feature_desc, ~gsub("BPQ020 = 2", "No history of hypertension", .x)),
        across(feature_desc, ~gsub("average_DBP", "Diastolic Blood Pressure (mm Hg)", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 1", "Ethnicity: Mexican American", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 2", "Ethnicity: Other Hispanic", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 3", "Ethnicity: Non-Hispanic White", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 4", "Ethnicity: Non-Hispanic Black", .x)),
        across(feature_desc, ~gsub("RIDRETH1 = 5", "Ethnicity: Other Race - Including Multi-Racial", .x)),
        across(feature_desc, ~gsub("INDFMPIR", "Poverty Income Ratio (PIR)", .x)),
        across(feature_desc, ~gsub("BMXWAIST", "Waist Circumference (cm)", .x)),
        across(feature_desc, ~gsub("MCQ160B = 1", "History of heart failure", .x)),
        across(feature_desc, ~gsub("MCQ160B = 2", "No history of heart failure", .x)),
        across(feature_desc, ~gsub("BMXBMI", "BMI (kg/m²)", .x)),
        across(feature_desc, ~gsub("BPXPLS", "60 seconds pulse", .x)),
        across(feature_desc, ~gsub("BMXARMC", "Arm Circumference (cm)", .x)),
        across(feature_desc, ~gsub("BMXARML", "Upper Arm Length (cm)", .x)),
        across(feature_desc, ~gsub("BMXWT", "Weight (kg)", .x))
    ) %>% 
    mutate(label = "Reduced Kidney Health") -> explanation_EPI2009_high_clean


explanation_EKFC_low_clean <- 
    explanation_EKFC_low_clean %>%
    mutate(type = factor(
        ifelse(feature_weight >= 0, "Supports", "Contradicts"),
        levels = c("Contradicts", "Supports")
    )) %>%
    mutate(feature_desc = reorder(feature_desc, abs(feature_weight)))

explanation_EKFC_high_clean <- 
    explanation_EKFC_high_clean %>%
    mutate(type = factor(
        ifelse(feature_weight >= 0, "Supports", "Contradicts"),
        levels = c("Contradicts", "Supports")
    )) %>%
    mutate(feature_desc = reorder(feature_desc, abs(feature_weight)))

explanation_EPI2021_low_clean <- 
    explanation_EPI2021_low_clean %>%
    mutate(type = factor(
        ifelse(feature_weight >= 0, "Supports", "Contradicts"),
        levels = c("Contradicts", "Supports")
    )) %>%
    mutate(feature_desc = reorder(feature_desc, abs(feature_weight)))

explanation_EPI2021_high_clean <- 
    explanation_EPI2021_high_clean %>%
    mutate(type = factor(
        ifelse(feature_weight >= 0, "Supports", "Contradicts"),
        levels = c("Contradicts", "Supports")
    )) %>%
    mutate(feature_desc = reorder(feature_desc, abs(feature_weight)))

explanation_EPI2009_low_clean <- 
    explanation_EPI2009_low_clean %>%
    mutate(type = factor(
        ifelse(feature_weight >= 0, "Supports", "Contradicts"),
        levels = c("Contradicts", "Supports")
    )) %>%
    mutate(feature_desc = reorder(feature_desc, abs(feature_weight)))

explanation_EPI2009_high_clean <- 
    explanation_EPI2009_high_clean %>%
    mutate(type = factor(
        ifelse(feature_weight >= 0, "Supports", "Contradicts"),
        levels = c("Contradicts", "Supports")
    )) %>%
    mutate(feature_desc = reorder(feature_desc, abs(feature_weight)))

# Plots
ggplot(explanation_EKFC_low_clean, aes(x = feature_desc, y = feature_weight, fill = type)) +
    geom_col(show.legend = T) +
    coord_flip() +
    scale_fill_manual(
        "Likelihood of Reduced Kidney Health",
        values = c("Contradicts" = "cornflowerblue", "Supports" = "tomato"),
        labels = c("Decreases", "Increases"),
        drop = FALSE # Force the scale to include all levels, even if unused
    ) +
    labs(
        title = "MERWACS",
        subtitle = paste0(
            "Predicting: ", explanation_EKFC_low_clean$label[1],
            "\nProbability: ", round(explanation_EKFC_low_clean$label_prob[1], 2),
            "\nExplanation Fit: ", round(explanation_EKFC_low_clean$model_r2[1], 2)
        ),
        x = "Feature",
        y = "Weight"
    ) +
    theme_minimal() -> p_lime_EKFC_low


ggplot(explanation_EKFC_high_clean, aes(x = feature_desc, y = feature_weight, fill = type)) +
    geom_col(show.legend = T) +
    coord_flip() +
    scale_y_continuous(breaks = seq(-0.05, 0.1, 0.05)) + 
    scale_fill_manual(
        "Likelihood of Reduced Kidney Health",
        values = c("Contradicts" = "cornflowerblue", "Supports" = "tomato"),
        labels = c("Decreases", "Increases"),
        drop = FALSE # Force the scale to include all levels, even if unused
    ) +
    labs(
        title = "MERWACS",
        subtitle = paste0(
            "Predicting: ", explanation_EKFC_high_clean$label[1],
            "\nProbability: ", round(explanation_EKFC_high_clean$label_prob[1], 2),
            "\nExplanation Fit: ", round(explanation_EKFC_high_clean$model_r2[1], 2)
        ),
        x = "Feature",
        y = "Weight"
    ) +
    theme_minimal() -> p_lime_EKFC_high



ggplot(explanation_EPI2021_low_clean, aes(x = feature_desc, y = feature_weight, fill = type)) +
    geom_col(show.legend = T) +
    coord_flip() +
    scale_fill_manual(
        "Likelihood of Reduced Kidney Health",
        values = c("Contradicts" = "cornflowerblue", "Supports" = "tomato"),
        labels = c("Decreases", "Increases"),
        drop = FALSE # Force the scale to include all levels, even if unused
    ) +
    labs(
        title = "MERWACS",
        subtitle = paste0(
            "Predicting: ", explanation_EPI2021_low_clean$label[1],
            "\nProbability: ", round(explanation_EPI2021_low_clean$label_prob[1], 2),
            "\nExplanation Fit: ", round(explanation_EPI2021_low_clean$model_r2[1], 2)
        ),
        x = "Feature",
        y = "Weight"
    ) +
    theme_minimal() -> p_lime_EPI2021_low


ggplot(explanation_EPI2021_high_clean, aes(x = feature_desc, y = feature_weight, fill = type)) +
    geom_col(show.legend = T) +
    coord_flip() +
    scale_fill_manual(
        "Likelihood of Reduced Kidney Health",
        values = c("Contradicts" = "cornflowerblue", "Supports" = "tomato"),
        labels = c("Decreases", "Increases"),
        drop = FALSE # Force the scale to include all levels, even if unused
    ) +
    labs(
        title = "MERWACS",
        subtitle = paste0(
            "Predicting: ", explanation_EPI2021_high_clean$label[1],
            "\nProbability: ", round(explanation_EPI2021_high_clean$label_prob[1], 2),
            "\nExplanation Fit: ", round(explanation_EPI2021_high_clean$model_r2[1], 2)
        ),
        x = "Feature",
        y = "Weight"
    ) +
    theme_minimal() -> p_lime_EPI2021_high


ggplot(explanation_EPI2009_low_clean, aes(x = feature_desc, y = feature_weight, fill = type)) +
    geom_col(show.legend = T) +
    coord_flip() +
    scale_fill_manual(
        "Likelihood of Reduced Kidney Health",
        values = c("Contradicts" = "cornflowerblue", "Supports" = "tomato"),
        labels = c("Decreases", "Increases"),
        drop = FALSE # Force the scale to include all levels, even if unused
    ) +
    labs(
        title = "MERWACS",
        subtitle = paste0(
            "Predicting: ", explanation_EPI2009_low_clean$label[1],
            "\nProbability: ", round(explanation_EPI2009_low_clean$label_prob[1], 2),
            "\nExplanation Fit: ", round(explanation_EPI2009_low_clean$model_r2[1], 2)
        ),
        x = "Feature",
        y = "Weight"
    ) +
    theme_minimal() -> p_lime_EPI2009_low


ggplot(explanation_EPI2009_high_clean, aes(x = feature_desc, y = feature_weight, fill = type)) +
    geom_col(show.legend = T) +
    coord_flip() +
    scale_fill_manual(
        "Likelihood of Reduced Kidney Health",
        values = c("Contradicts" = "cornflowerblue", "Supports" = "tomato"),
        labels = c("Decreases", "Increases"),
        drop = FALSE # Force the scale to include all levels, even if unused
    ) +
    labs(
        title = "MERWACS",
        subtitle = paste0(
            "Predicting: ", explanation_EPI2009_high_clean$label[1],
            "\nProbability: ", round(explanation_EPI2009_high_clean$label_prob[1], 2),
            "\nExplanation Fit: ", round(explanation_EPI2009_high_clean$model_r2[1], 2)
        ),
        x = "Feature",
        y = "Weight"
    ) +
    theme_minimal() -> p_lime_EPI2009_high


library(patchwork)
(p_lime_EKFC_low + p_lime_EKFC_high) / 
    (p_lime_EPI2021_low + p_lime_EPI2021_high) / 
    (p_lime_EPI2009_low + p_lime_EPI2009_high) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = 'A') &
    theme(
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 17, lineheight = 1.1),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text = element_text(size = 17),
        legend.title = element_text(size = 17, face = "bold"),
        legend.position = "bottom",
        legend.box.just = "center",
        legend.text = element_text(size = 17),
        plot.tag = element_text(face = 'bold', size = 25)) -> p_lime_total


ggsave(filename = "Figure_5_LIME.png", plot = p_lime_total, width = 20, height = 15)


