#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 20 09:11:14 2022

@author: tinbuenafe
"""

from sklearn.ensemble import GradientBoostingRegressor
from sklearn.model_selection import StratifiedKFold
from sklearn.model_selection import cross_val_predict
from sklearn.model_selection import cross_val_score
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

######### SKIPJACK TUNA #########
df = callDF('SKP')
X = df[0]
y = df[1]

######### Model 0 #########
# Define the BRT
gbr = GradientBoostingRegressor(loss='squared_error', n_estimators = 1000, verbose = 1) 
createPreds(gbr, skf, "SKP_model0.csv")

######### Model 1 #########
# Define the BRT
gbr = GradientBoostingRegressor(loss='squared_error', n_estimators = 2000, verbose = 1) 
createPreds(gbr, skf, "SKP_model1.csv")

######### Model 2 #########
# Define the BRT
gbr = GradientBoostingRegressor(loss='squared_error', n_estimators = 1000, learning_rate = 0.05, verbose = 1) 
createPreds(gbr, skf, "SKP_model2.csv")

######### Model 3 #########
# Define the BRT
gbr = GradientBoostingRegressor(loss='squared_error', n_estimators = 2000, learning_rate = 0.05, verbose = 1)
createPreds(gbr, skf, "SKP_model3.csv")
