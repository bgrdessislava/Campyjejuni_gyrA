import os
import sys
import glob
import numpy as np
import pandas as pd
import catboost
from pathlib import Path
from itertools import combinations
from catboost import CatBoostClassifier
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split
from sklearn.metrics.cluster import adjusted_rand_score


labelledData = pd.read_pickle("labelledData-allLoci.pkl")
target = 'Group'
locus = sys.argv[1:]

name = "-".join(locus)
file = f'{out}/{name}.txt'
if os.path.exists(file) and os.path.getsize(file) > 0:
	continue
with open(file, 'w') as fh:
    X = labelledData[locus + [target]].copy().astype('category')
    y = X.pop(target)

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, random_state=42, train_size=0.8, test_size=0.2)

    #If you want to run the model this is the one
    model = CatBoostClassifier(verbose=0)
    model = model.fit(X_train, y_train, cat_features=list(X.columns))

    y_pred = model.predict(X_test)
    score = adjusted_rand_score(y_test, y_pred.flatten())
    fh.write(f'{name}\t{score}\n')



