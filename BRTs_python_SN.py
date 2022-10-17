# -*- coding: utf-8 -*-
"""
Created on Mon Oct 17 09:30:37 2022

@author: sandra neubert
"""

#from sklearn.datasets import make_regression
from sklearn.ensemble import GradientBoostingRegressor
#from sklearn.model_selection import KFold
import pandas as pd
import os #to set path: os.chdir("")
import numpy as np
from sklearn.metrics import roc_auc_score
from sklearn.model_selection import StratifiedKFold

df = pd.read_csv ('LarvaDistModels\\Output\\YFT_full.csv')
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
    
df_filtered['season_mut'] = df_filtered.apply(season, axis=1)
np.unique(df_filtered['season_mut'])

# Create column for presence/absence
df_filtered = df_filtered.assign(abundance_mut = [1 if abundance > 0 else 0 for abundance in df_filtered['abundance']])
np.unique(df_filtered['abundance_mut']) # Seems right

# Define the predictors (X) and the response (Y)
df_filtered.columns.values # Check column names
X,y = df_filtered.drop(['cellID','species','abundance', 'abundance_mut', 'season'], axis=1),df_filtered['abundance_mut']

auc_list = []

skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=42)#Stratified is to ensure that each fold of dataset has the same proportion of observations with a given label
for train_index, test_index in skf.split(X, y):

   X_train, X_test = X.iloc[train_index], X.iloc[test_index]
   y_train, y_test = y.iloc[train_index], y.iloc[test_index]
   
   gbr = GradientBoostingRegressor(loss='squared_error', n_estimators = 1000)
   gbr.fit(X_train, y_train.values.ravel())
   y_pred = gbr.predict(X_test)
   
   auc = roc_auc_score(y_test, y_pred, average = "macro")
   auc_list.append(auc)
   
   
mean_auc_gbr = np.mean(auc_list, axis = 0)

