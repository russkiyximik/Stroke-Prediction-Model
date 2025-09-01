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


# STEP THREE: CREATING THE MODEL
# ------------------------------
set.seed(12345)
dataSplit <- initial_split(finalData, prop = 3/4)
dataTraining <- training(dataSplit)
dataTesting <- testing(dataSplit)
# We will test the following parameters: mtry
dataCV <- vfold_cv(dataTraining)

dataRecipe <- recipe(HeartDisease ~ ., data=finalData) %>% 
	step_normalize(all.numeric()) %>% step_impute_knn(Cholesterol)
rfModel <- rand_forest() %>% set_args(mtry=tune()) %>% set_engine('ranger',
	importance = 'impurity') %>% set_mode('classification')
