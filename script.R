# Idea for the future: redo the whole thing, use PCA!
# Idea for future: account for correlation, interaction (step_corr & _interact)



# STEP ONE: LIBRARY & DATA LOADING
# --------------------------------
library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)
library(ggplot2)
library(ranger)


unzip('archive.zip')
orig_data <- read_csv('heart.csv')



# STEP TWO: PRELIMINARY EDA, DATA IMPUTATION
# ------------------------------------------
glimpse(orig_data)

# Let's take a look at factor variables
orig_data %>% select(where(is.character)) %>% apply(2, table)

# Let's take a look at non-factor variables
# Making a loop to graph numeric variables was a huge pain.
# ggplot to the rescue //Go through the steps in the RShiny//
orig_data %>% select(where(is.double)) %>% pivot_longer(cols=everything()) %>% 
	ggplot(aes(x=value, fill=name)) + 
	facet_wrap(~ name, scales = 'free') + geom_histogram(bins=30)

# First, we should convert the outcome & binary predictors to factors to ensure 
# our model (ranger) runs a classification model, not regression.
finalData <- orig_data %>% mutate(HeartDisease=as.factor(HeartDisease), 
	FastingBS=as.factor(FastingBS)) %>% 
	mutate(across(where(is.character), as.factor))

# There is a huge spike in the Cholesterol graph for x = 0. These values 
# represent missing/unknown values.
finalData <- finalData %>% mutate(across(Cholesterol, function(i) {
		if_else(i==0, as.numeric(NA), i)
	}))



# STEP THREE: CREATING THE MODEL FRAMEWORK
# ----------------------------------------
set.seed(12345)
dataSplit <- initial_split(finalData, prop = 3/4)
dataTraining <- training(dataSplit)
dataTesting <- testing(dataSplit)
# We will test the following parameters: mtry, min_n //Explain why//
dataCV <- vfold_cv(dataTraining)

dataRecipe <- recipe(HeartDisease ~ ., data=finalData) %>% 
	step_normalize(all_numeric()) %>% step_impute_knn(Cholesterol)
rfModel <- rand_forest() %>% set_args(mtry=tune(), min_n=tune()) %>% 
	set_engine('ranger', importance = 'permutation') %>% 
	set_mode('classification')
rfWorkflow <- workflow() %>% add_recipe(dataRecipe) %>% add_model(rfModel)



# STEP FOUR: TESTING DIFFERENT PARAMETERS
# ---------------------------------------
# Good mtry is sqrt(total vars) = sqrt(11) = 3
rfGrid <- expand.grid(mtry=2:6, min_n=c(1, 3, 5, 10, 15, 20, 30))
rfTuneResults <- rfWorkflow %>% tune_grid(resamples=dataCV, grid=rfGrid, 
	metrics=metric_set(accuracy, roc_auc))
rfTuneResults %>% collect_metrics()
# Comparing mtry's for each min_n, it seems like there is a weak negative 
# correlation between mtry and model accuracy.
# However, min_n seems a lot more interesting. Let's keep experimenting.
rfGrid2 <- expand.grid(mtry=1:2, min_n=c(1, 3, 5, seq(5, 50, 5)))
rfTuneResults2 <- rfWorkflow %>% tune_grid(resamples=dataCV, grid=rfGrid2, 
	metrics=metric_set(accuracy, roc_auc))
rfTuneResults2 %>% collect_metrics() %>% filter(.metric=='accuracy') %>% 
	arrange(desc(mean))
rfTuneResults2 %>% collect_metrics() %>% filter(.metric=='roc_auc') %>% 
	arrange(desc(mean))
# mtry=2, min_n=3 comes in at first place for accuracy, first (tied) for 
# roc_auc, first for lowest std err (accuracy) and fifth for lowest (roc_auc)
paramFinal <- rfTuneResults2 %>% select_best(metric='accuracy')
rfWorkflow <- rfWorkflow %>% finalize_workflow(paramFinal)



# STEP FIVE: FIT THE MODEL
# ------------------------
rfFit <- rfWorkflow %>% last_fit(dataSplit)
testPerformance <- rfFit %>% collect_metrics()
testPerformance
# Accuracy of 87.8%, not bad!
rfFit %>% collect_predictions() %>% conf_mat(truth=HeartDisease, 
	estimate=.pred_class)
rfFit %>% collect_predictions() %>% ggplot() + 
	geom_density(aes(x=.pred_0, fill=HeartDisease, alpha=0.5))
# Nice seperation!



# STEP SIX: FIT ON A TARGET DATASET
# ---------------------------------
finalModel <- fit(rfWorkflow, finalData)
extract_fit_parsnip(finalModel)
extract_fit_parsnip(finalModel)$fit$variable.importance
# ST_Slope seems to be the most important variable, accounting for ~9% of the 
# model's predictive power