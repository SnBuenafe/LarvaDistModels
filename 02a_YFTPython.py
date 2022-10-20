from sklearn.ensemble import GradientBoostingRegressor
from sklearn.model_selection import StratifiedKFold
from sklearn.model_selection import cross_val_predict
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import train_test_split
import os as os
import pandas as pd
import numpy as numpy

wd = os.getcwd()
os.chdir(os.path.join(wd,'GitHub/LarvaDistModels/'))

def callDF(spName):
    file = 'Output/' + spName + '_full.csv'
    df = pd.read_csv(file)
    df_filtered = df[df['abundance'].notnull()] # Filter for rows with abundance values
    df_filtered = df_filtered.drop('geometry', axis=1)

    # Change the seasons to numeric?
    def season(row):
        if row['season'] == "jan-mar":
            return 0
        elif row['season'] == "apr-jun":
            return 1
        elif row['season'] == "jul-sept":
            return 2
        else:
            return 3
    
    df_filtered['season_mut'] = df_filtered.apply(season, axis=1) # Change seasons to numeric factors instead of strings
    numpy.unique(df_filtered['season_mut']) # Check if it does what we want it to do

    # Create column for presence/absence
    df_filtered = df_filtered.assign(abundance_mut = [1 if abundance > 0 else 0 for abundance in df_filtered['abundance']])
    numpy.unique(df_filtered['abundance_mut']) # Seems right

    # Define the predictors (X) and the response (Y)
    df_filtered.columns.values # Check column names
    X,y = df_filtered.drop(['cellID','species','abundance', 'abundance_mut', 'season'], axis=1),df_filtered['abundance_mut']
    
    return X,y

# Function for saving predictions and calculating metrics
def createPreds(gbr, skf, save):
    # Calculate cross-validated predictions
    y_pred = cross_val_predict(gbr, X, y, cv = skf)
    preds = pd.DataFrame(y_pred)
    
    # Save predictions
    saveFile = wd + "/GitHub/LarvaDistModels/Output/python/" + save
    preds.to_csv(saveFile)
    print("Saved predictions.")
    
    # Calculate cross-validated AUC
    cv_score = (cross_val_score(gbr, X, y, cv = skf, scoring = 'roc_auc'))
    mean_auc_gbr = numpy.mean(cv_score, axis = 0)
    print(mean_auc_gbr)
    
    # Calculate cross-validated R2
    r2_score = (cross_val_score(gbr, X, y, cv = skf, scoring = 'r2'))
    mean_r2_gbr = numpy.mean(r2_score, axis = 0)
    print(mean_r2_gbr)
    
# Define the CV parameters
skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=42) #Stratified is to ensure that each fold of dataset has the same proportion of observations with a given label

######### YELLOWFIN TUNA #########
df = callDF('YFT')
X = df[0]
y = df[1]

######### Model 0 #########
# Define the BRT
gbr = GradientBoostingRegressor(loss='squared_error', n_estimators = 1000, verbose = 1) 
createPreds(gbr, skf, "YFT_model0.csv")

######### Model 1 #########
# Define the BRT
gbr = GradientBoostingRegressor(loss='squared_error', n_estimators = 2000, verbose = 1) 
createPreds(gbr, skf, "YFT_model1.csv")

######### Model 2 #########
# Define the BRT
gbr = GradientBoostingRegressor(loss='squared_error', n_estimators = 1000, learning_rate = 0.05, verbose = 1) 
createPreds(gbr, skf, "YFT_model2.csv")

######### Model 3 #########
# Define the BRT
gbr = GradientBoostingRegressor(loss='squared_error', n_estimators = 2000, learning_rate = 0.05, verbose = 1)
createPreds(gbr, skf, "YFT_model3.csv")

## See this for tuning: https://www.analyticsvidhya.com/blog/2016/02/complete-guide-parameter-tuning-gradient-boosting-gbm-python/

########### GRID SEARCH ############
# Learning rate = 0.05; # Estimators = 1000
# Vary max_depth (# of nodes) and min_samples_split (min. samples required to split an internal node)
gbr = GradientBoostingRegressor(loss='squared_error', n_estimators = 1000, learning_rate = 0.05) 
param_test = {'max_depth':range(3,16,3), 'min_samples_split':range(2,103,10)}

# Just do a 70-30 split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.30, random_state=42)

gsearch = GridSearchCV(estimator = gbr, param_grid = param_test, scoring='roc_auc', n_jobs=4)
gsearch.fit(X_train,y_train)
gsearch.best_params_, gsearch.best_score_

# Let's try a higher min_samples_split but keep the max_depth the same
gbr = GradientBoostingRegressor(loss='squared_error', n_estimators = 1000, learning_rate = 0.05, max_depth = 3) 
param_test = {'min_samples_split':range(100,501,50), 'min_samples_leaf':range(30,71,10)}

gsearch = GridSearchCV(estimator = gbr, param_grid = param_test, scoring='roc_auc', n_jobs=4)
gsearch.fit(X_train,y_train)
gsearch.best_params_, gsearch.best_score_

######### Model 4 #########
gbr = GradientBoostingRegressor(loss='squared_error', n_estimators = 1000, learning_rate = 0.05, max_depth = 3, min_samples_split = 150, min_samples_leaf = 50) 
createPreds(gbr, skf, "YFT_model4.csv")
