# STEP ONE: LIBRARY & DATA LOADING
# --------------------------------
library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)
library(ggplot2)

unzip('archive.zip')
orig_data <- read_csv('heart.csv')


# STEP TWO: PRELIMINARY EDA, DATA IMPUTATION
# ------------------------------------------
glimpse(orig_data)

# Let's take a look at factor variables
orig_data %>% select(where(is.character)) %>% apply(2, table)


# Let's take a look at non-factor variables
# Making a loop to graph numeric variables was a huge pain.
# //Go through the steps in the RShiny//
orig_data %>% select(where(is.double)) %>% pivot_longer(cols=everything()) %>% 
	ggplot(aes(x=value, fill=name)) + 
	facet_wrap(~ name, scales = 'free') + geom_histogram(bins=30)
# ggplot to the rescue.


# //Mention huge spike at chol. = 0 - values must be imputed//

# Next steps for me: figure out if converting outcome to factor is necessary, 
# figure out if uncorrelating variables and creating dummies is necessary 
# (step_corr and step_dummy respectively). Look at a quick tutorial about other
# params I can play around with.