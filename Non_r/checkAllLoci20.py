import os
import sys
import numpy as np
import pandas as pd
import catboost
from catboost import CatBoostClassifier
from sklearn.metrics.cluster import adjusted_rand_score

ngroup = 20

loci = sys.argv[1:]
out = '.'
name = "-".join(loci)
file = f'{out}/{name}.txt'
if os.path.exists(file) and os.path.getsize(file) > 0:
	sys.exit(0)

print(f'Processing {loci}')

X_trainAll = pd.read_excel(f"../../NG_Catboost_35/NG_CheckClusterNumber/X_train_{ngroup}.xlsx").rename({'Unnamed: 0': 'ID'}, axis=1)
X_testAll = pd.read_excel(f"../../NG_Catboost_35/NG_CheckClusterNumber/X_test_{ngroup}.xlsx").rename({'Unnamed: 0': 'ID'}, axis=1)

labelledData = pd.read_pickle("../../NG_Catboost_35/labelledData-allLoci.pkl").drop(['Group'],axis=1)

target = f'trainNG-{ngroup}'

X_trainAll  = pd.merge(X_trainAll, labelledData, left_on='ID', right_on='ID')
X_testAll  = pd.merge(X_trainAll, labelledData, left_on='ID', right_on='ID')

y_train = X_trainAll.pop(target)
X_train = X_trainAll[loci].copy().astype('category')
X_test = X_testAll[loci].copy().astype('category')

#If you want to run the model this is the one
model = CatBoostClassifier(verbose=0)
model = model.fit(X_train, y_train, cat_features=list(X_train.columns))

X_testAll['prediction'] = model.predict(X_test)
X_testAll['predictionProb'] = model.predict_proba(X_test).max(axis=1)

score = adjusted_rand_score(X_testAll['prediction'], X_testAll[f'fullNG-{ngroup}'])

with open(file, 'w') as fh:
	fh.write(f'{name}\t{score}\n')