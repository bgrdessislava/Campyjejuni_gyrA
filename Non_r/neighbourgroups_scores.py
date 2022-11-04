
import sys
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from ete3 import ClusterTree
from scipy.spatial.distance import pdist
from scipy.cluster.hierarchy import linkage, fcluster
from sklearn.metrics.cluster import adjusted_rand_score
from sklearn.model_selection import train_test_split

NGroup_scores = pd.read_excel("../../Data/NG_numbers_score.xlsx")
score = float(NGroup_scores.loc[NGroup_scores['Ngroup'] == 20, 'Score'])

fig,ax = plt.subplots()
sns.lineplot(data = NGroup_scores, x = "Ngroup" , y = "Score", ax = ax)
ax.set_xlabel('NGroup size')
ax.set_ylabel('Adjusted Rand Index')
ax.axvline(20, ymax=score, color='black', alpha=0.2)
ax.set_xlim(1, 100,10)
ax.set_ylim(0, 1)
fig.tight_layout()
fig.savefig('../../Figures/NGroup_score_linegraph.pdf')
