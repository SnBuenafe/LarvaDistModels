from sklearn.datasets import make_regression
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.model_selection import KFold
import pandas as pd
import numpy as numpy

X, y = make_regression(random_state=0)

df = pd.read_csv ('/Users/tinbuenafe/GitHub/LarvaDistModels/Output/YFT_full.csv')
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
numpy.unique(df_filtered['season_mut'])

# Create column for presence/absence
df_filtered = df_filtered.assign(abundance_mut = [1 if abundance > 0 else 0 for abundance in df_filtered['abundance']])
numpy.unique(df_filtered['abundance_mut']) # Seems right

# Define the predictors (X) and the response (Y)
df_filtered.columns.values # Check column names
X,y = df_filtered.drop(['cellID','species','abundance', 'abundance_mut', 'season'], axis=1),df_filtered['abundance_mut']

# Split the dataset to training and testing values using 5-fold crossvalidation
kf = KFold(n_splits=5,random_state=42,shuffle=True)
kf.get_n_splits(X)

for train_index,val_index in kf.split(X):
    X_train,X_val = X.iloc[train_index],X.iloc[val_index],
    y_train,y_val = y.iloc[train_index],y.iloc[val_index]

gradient_booster = GradientBoostingRegressor(loss='squared_error',learning_rate=0.01)
gradient_booster.get_params()

gradient_booster.fit(X_train,y_train)
gradient_booster.score(X_train,y_train) # Gets the R2

# Get the R2 on the validation set
gradient_booster.score(X_val,y_val)
