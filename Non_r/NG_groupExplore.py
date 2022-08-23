import sys
import numpy as np
import pandas as pd
import catboost
from catboost import CatBoostClassifier
from ete3 import ClusterTree
from scipy.spatial.distance import pdist
from scipy.cluster.hierarchy import linkage, fcluster
from sklearn.metrics.cluster import adjusted_rand_score
from sklearn.model_selection import train_test_split

# https://stackoverflow.com/questions/31033835/newick-tree-representation-to-scipy-cluster-hierarchy-linkage-matrix-format

def newick_to_linkage(newick: str) -> (np.ndarray, [str]):
    """
    Convert newick tree into scipy linkage matrix

    :param newick: newick string, e.g. '(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);'
    :returns: linkage matrix and list of labels
    """
    # newick string -> cophenetic_matrix
    tree = ClusterTree(newick)
    cophenetic_matrix, newick_labels = tree.cophenetic_matrix()
    cophenetic_matrix = pd.DataFrame(
        cophenetic_matrix, columns=newick_labels, index=newick_labels)

    # reduce square distance matrix to condensed distance matrices
    pairwise_distances = pdist(cophenetic_matrix)
    
    # return linkage matrix and labels
    return linkage(pairwise_distances), list(cophenetic_matrix.columns)


def readNewick(nwk):
    with open(nwk) as fh:
        return fh.readline()
        
cols = ([
    'id', 'aspA', 'glnA', 'gltA', 'glyA', 'pgm', 'tkt', 
    'uncA', 'ST (MLST)', 'clonal_complex (MLST)'
])
df = pd.read_excel('10359_Dataframe.xlsx')[cols]
df['id'] = df['id'].astype(str)
df = df.set_index('id')

nwk = readNewick('BIGSdb_024808_1423149961_99700_tree.nwk')

linkageMatrix, labels = newick_to_linkage(newick=nwk)

ngroup = sys.argv[1]

clusters = fcluster(linkageMatrix, t=ngroup, criterion='maxclust', depth=2, R=None, monocrit=None)
labelsID = pd.DataFrame(clusters, labels, columns=[f'fullNG-{ngroup}'])

df = pd.merge(df, labelsID, left_index=True, right_index=True).fillna('Unknown')

y = list(range(len(df)))
X_train, X_test, y_train, y_test = train_test_split(df, y, test_size=0.2, random_state=42)

nwk = readNewick('BIGSdb_008959_0106465177_07730_tree.nwk')
linkageMatrix, labels = newick_to_linkage(newick=nwk)
clusters = fcluster(linkageMatrix, t=ngroup, criterion='maxclust', depth=2, R=None, monocrit=None)
labelsID = pd.DataFrame(clusters, labels, columns=[f'trainNG-{ngroup}'])

X_train = pd.merge(X_train, labelsID, left_index=True, right_index=True).fillna('Unknown')

X_train.to_excel(f"X_train_{ngroup}.xlsx")
X_test.to_excel(f"X_test_{ngroup}.xlsx")

X_trainAll = pd.read_excel(f"X_train_{ngroup}.xlsx")
X_testAll = pd.read_excel(f"X_test_{ngroup}.xlsx")

target = f'trainNG-{ngroup}'

y_train = X_trainAll.pop(target)
X_train = X_trainAll[['aspA', 'glnA', 'gltA', 'glyA', 'pgm', 'tkt', 'uncA']].copy().astype('category')
X_test = X_testAll[['aspA', 'glnA', 'gltA', 'glyA', 'pgm', 'tkt', 'uncA']].copy().astype('category')

#If you want to run the model this is the one
model1 = CatBoostClassifier(verbose=0)
model1 = model.fit(X_train, y_train, cat_features=list(X_train.columns))

X_testAll['prediction'] = model.predict(X_test)
X_testAll['predictionProb'] = model.predict_proba(X_test).max(axis=1)

score = adjusted_rand_score(X_testAll['prediction'], X_testAll[f'fullNG-{ngroup}'])
print(score,ngroup)

X_testAll.to_excel(f"X_test_{ngroup}_results.xlsx")



