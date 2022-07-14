import sys
import glob
import numpy as np
import pandas as pd
#import Pyreadstat
from ete3 import ClusterTree
from scipy.spatial.distance import pdist
from scipy.cluster.hierarchy import fcluster

# https://stackoverflow.com/questions/31033835/newick-tree-representation-to-scipy-cluster-hierarchy-linkage-matrix-format

def newick_to_linkage(newick: str, label_order: [str] = None) -> (np.ndarray, [str]):
    """
    Convert newick tree into scipy linkage matrix

    :param newick: newick string, e.g. '(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);'
    :param label_order: list of labels, e.g. ['A', 'B', 'C']
    :returns: linkage matrix and list of labels
    """
    # newick string -> cophenetic_matrix
    tree = ClusterTree(newick)
    cophenetic_matrix, newick_labels = tree.cophenetic_matrix()
    cophenetic_matrix = pd.DataFrame(cophenetic_matrix, columns=newick_labels, index=newick_labels)

    if label_order is not None:
        # sanity checks
        missing_labels = set(label_order).difference(set(newick_labels))
        superfluous_labels = set(newick_labels).difference(set(label_order))
        assert len(missing_labels) == 0, f'Some labels are not in the newick string: {missing_labels}'
        if len(superfluous_labels) > 0:
            print(f'Newick string contains unused labels: {superfluous_labels}', file=sys.stderr)

        # reorder the cophenetic_matrix
        cophenetic_matrix = cophenetic_matrix.reindex(index=label_order, columns=label_order)

    # reduce square distance matrix to condensed distance matrices
    pairwise_distances = pdist(cophenetic_matrix)

    return pairwise_distances, list(cophenetic_matrix.columns)


def readNewick(nwk):
    with open(nwk) as fh:
        return fh.readline()
        
file = sys.argv[1]
nwk = readNewick(file)

pairwise, labels = newick_to_linkage(newick=nwk)

np.save(file + '.npy', pairwise)