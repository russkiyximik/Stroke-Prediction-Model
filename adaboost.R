# STEP ONE: CREATING THE MODEL FRAMEWORK
# --------------------------------------

# We will be making an AdaBoost model for our data.
library(C50)
set.seed(123456)
# Note: lines 6-16 do not need to be added to the .qml, since they were already 
# run during the random forest part.
dataSplit <- initial_split(finalData, prop = 3/4)
dataTraining <- training(dataSplit)
dataTesting <- testing(dataSplit)
# We will test the following parameters: 
dataCV <- vfold_cv(dataTraining)

dataRecipe <- recipe(HeartDisease ~ ., data=finalData) %>% 
	step_normalize(all_numeric()) %>% step_impute_knn(Cholesterol)
# Tuning min_n (if we get a high value) is basically the same as pre-pruning.
abModel <- boost_tree() %>% set_args(trees=tune(), min_n=tune()) %>% 
	set_engine('C5.0') %>% set_mode('classification')
abWorkflow <- workflow() %>% add_recipe(dataRecipe) %>% add_model(abModel)



# STEP TWO: TESTING DIFFERENT PARAMETERS
# --------------------------------------
abGrid <- expand.grid(trees=seq(5,50,5), 
		      min_n=c(2,seq(5,25,10)))
abTuneResults <- abWorkflow %>% tune_grid(resamples=dataCV, grid=abGrid, 
					  metrics=metric_set(accuracy, roc_auc),
					  control=control_grid(verbose = TRUE))
abTuneResults %>% collect_metrics() %>% filter(.metric=='accuracy') %>% 
	arrange(desc(mean)) %>% select(trees, min_n, mean) %>% 
	pivot_longer(cols=c(trees, min_n)) %>% ggplot(aes(x=value,y=mean)) + 
	geom_point() + facet_wrap(~name,scales='free')
# Trees doesn't really matter, but min_n seems to get better the higher it gets.
abGrid2 <- expand.grid(trees=50, min_n=seq(25,100,5))
abTuneResults2 <- abWorkflow %>% tune_grid(resamples=dataCV, grid=abGrid2, 
					  metrics=metric_set(accuracy, roc_auc),
					  control=control_grid(verbose = TRUE))
abTuneResults2 %>% collect_metrics() %>% filter(.metric=='accuracy') %>% 
	arrange(desc(mean)) %>% select(trees, min_n, mean) %>% 
	pivot_longer(cols=c(trees, min_n)) %>% ggplot(aes(x=value,y=mean)) + 
	geom_point() + facet_wrap(~name,scales='free')
# Interestingly, it seems like the accuracy falls.
abTuneResults %>% collect_metrics() %>% filter(.metric=='accuracy') %>% 
	arrange(desc(mean))
# Don't include lines 35-47 in an actual chunk for the sake of saving time
paramFinal <- abTuneResults %>% select_best(metric='accuracy')
abWorkflow <- abWorkflow %>% finalize_workflow(paramFinal)



# STEP THREE: FIT THE MODEL
# -------------------------
abFit <- abWorkflow %>% last_fit(dataSplit)
testPerformance <- abFit %>% collect_metrics()
testPerformance
# Our model has 86.1% accuracy, coincidentally the same as RF from before

abFit %>% collect_predictions() %>% conf_mat(truth=HeartDisease, 
					     estimate=.pred_class)
abFit %>% collect_predictions() %>% ggplot() + 
	geom_density(aes(x=.pred_1, fill=HeartDisease, alpha=0.5))
