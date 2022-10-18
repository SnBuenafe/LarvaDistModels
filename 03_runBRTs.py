from sklearn.ensemble import GradientBoostingRegressor
from sklearn.model_selection import StratifiedKFold
from sklearn.model_selection import cross_val_predict
from sklearn.model_selection import cross_val_score
from sklearn.metrics import roc_auc_score
#from sklearn.metrics import accuracy_score
#from sklearn.metrics import f1_score
from sklearn.metrics import r2_score
from os import chdir, getcwd
import pandas as pd
import numpy as numpy

wd = getcwd()
chdir(wd)

df = pd.read_csv ('Output/YFT_full.csv')
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

# Define the BRT
gbr = GradientBoostingRegressor(loss='squared_error', n_estimators = 2000) 

# Define the CV parameters
skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=42) #Stratified is to ensure that each fold of dataset has the same proportion of observations with a given label

# Calculate cross-validated predictions
y_pred = cross_val_predict(gbr, X, y, cv = skf)
preds = pd.DataFrame(y_pred)
preds.to_csv("Output/YFT_preds_python.csv")

# Calculate cross-validated AUC
cv_score = (cross_val_score(gbr, X, y, cv = skf, scoring = 'roc_auc'))
mean_auc_gbr = numpy.mean(cv_score, axis = 0)

# Calculate cross-validated R2
r2_score = (cross_val_score(gbr, X, y, cv = skf, scoring = 'r2'))
mean_r2_gbr = numpy.mean(r2_score, axis = 0)

