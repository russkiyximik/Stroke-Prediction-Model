# load the relevant tidymodels libraries
library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)


# load the Pima Indians dataset from the mlbench dataset
library(mlbench)
data(PimaIndiansDiabetes)
# rename dataset to have shorter name because lazy
diabetes_orig <- PimaIndiansDiabetes


diabetes_clean <- diabetes_orig %>%
	mutate(across(c(triceps, glucose, pressure, insulin, mass), function(i) {
		if_else(i==0, as.numeric(NA), i)
	}))


set.seed(234589)
# split the data into training (75%) and testing (25%)
diabetes_split <- initial_split(diabetes_clean, 
				prop = 3/4)

# extract training and testing sets
diabetes_train <- training(diabetes_split)
diabetes_test <- testing(diabetes_split)
# create CV object from training data
diabetes_cv <- vfold_cv(diabetes_train)

# Constructs a model (formula). FYI, data = can be any of the diabetes datasets, 
# since it just takes names and roles of variables.
# step_normalize normalizes numeric data.
# step_impute_knn imputes missing values using k-nearest neighbors w/ default k=0
diabetes_recipe <- recipe(diabetes ~ ., data=diabetes_clean) %>% 
	step_normalize(all_numeric()) %>% step_impute_knn(all_predictors()) 


# HERE ARE SOME EXAMPLES OF HOW MODELS WORK IN R
# ----------------------------------------------
# ranger is an R package that efficiently does random forests.
# If you want to be able to examine the variable importance of your final model 
# later, you will need to set importance argument when setting the engine. For 
# ranger, the importance options are "impurity" or "permutation".
# classification chooses between continuous regression or binary classification
install.packages('ranger')
library(ranger)
rf_model <- rand_forest() %>% set_args(mtry=tune()) %>% set_engine('ranger', 
	importance = 'impurity') %>% set_mode("classification")
lr_model <- logistic_reg() %>% set_engine('glm') %>% set_mode('classification')
# setting a parameter to tune() means that it will be tuned later in the tune 
# stage of the pipeline (i.e. the value of the parameter that yields the best 
# performance will be chosen). You could also just specify a particular value 
# of the parameter if you don’t want to tune it e.g. using set_args(mtry = 4).
# ----------------------------------------------------------------------------


# Put it all together in a workflow
rf_workflow <- workflow() %>% add_recipe(diabetes_recipe) %>% add_model(rf_model)


# Tune the parameters (mtry)
# tuning will be done using cross-validation object
rf_grid <- expand.grid(mtry=c(3,4,5))
rf_tune_results <- rf_workflow %>% tune_grid(resamples=diabetes_cv, 
	grid=rf_grid, metrics=metric_set(accuracy, roc_auc))
rf_tune_results %>% collect_metrics()
# We see that mtry=3 yields the best results.


param_final <- rf_tune_results %>% select_best(metric = 'accuracy')
param_final
rf_workflow <- rf_workflow %>% finalize_workflow(param_final)
rf_workflow


# Recipe, model/workflow, parameters are good to go
rf_fit <- rf_workflow %>% last_fit(diabetes_split)
rf_fit
test_perforamance <- rf_fit %>% collect_metrics()
test_perforamance
# Look into confusion matrix, calibration plot, and feature importance!
# ps although feature importance might have already been used with "importance" before


test_predictions <- rf_fit %>% collect_predictions()
test_predictions
test_predictions %>% conf_mat(truth=diabetes, estimate= .pred_class) # Makes a confusion matrix
test_predictions %>% ggplot() + geom_density(aes(x=.pred_pos, fill=diabetes, alpha=0.5))
# Or could have done rf_fit %>% pull(.prediction (or .metrics)) instead of collect_whatever()


# Finally, fit model on a target dataset
final_model <- fit(rf_workflow, diabetes_clean)
final_model
predict(final_model, new_data = 
	tribble(~pregnant, ~glucose, ~pressure, ~triceps, ~insulin, ~mass, 
		~pedigree, ~age, 2, 95, 70, 31, 102, 28.2, 0.67, 47))


# Extract variable importance
extract_fit_parsnip(final_model)
extract_fit_parsnip(final_model)$fit$variable.importance
# variable.importance is a specific object contained within ranger output - 
# this will need to be adapted for the specific object type of other models.

