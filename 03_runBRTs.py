from sklearn.ensemble import GradientBoostingRegressor
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import roc_auc_score
from sklearn.metrics import accuracy_score
from sklearn.metrics import f1_score
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

auc_list = []
accuracy_list = []
f_list = []
r2_list = []

skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=42) #Stratified is to ensure that each fold of dataset has the same proportion of observations with a given label
for train_index, test_index in skf.split(X, y):
    
    # Split into training and testing data
   X_train, X_test = X.iloc[train_index], X.iloc[test_index]
   y_train, y_test = y.iloc[train_index], y.iloc[test_index]
   
   gbr = GradientBoostingRegressor(loss='squared_error', n_estimators = 1000) # 
   gbr.fit(X_train, y_train.values.ravel())
   y_pred = gbr.predict(X_test)
   
   # Calculate AUC per fold
   auc = roc_auc_score(y_test, y_pred, average = "macro")
   auc_list.append(auc)
   
   # Calculate accuracy score per fold
   #accuracy = accuracy_score(y_test, y_pred)
   #accuracy_list.append(accuracy)
   
   # Calculate F1 score per fold
   #fscore = f1_score(y_test, y_pred, average = "macro")
   #f_list.append(fscore)
   
   # Calculate R2 per fold
   r2 = r2_score(y_test, y_pred)
   r2_list.append(r2)
   
   
mean_auc_gbr = numpy.mean(auc_list, axis = 0)
mean_accuracy_gbr = numpy.mean(accuracy_list, axis = 0)
mean_fscore_gbr = numpy.mean(f_list, axis = 0)

# Accuracy score
