import numpy as np
import sys
from scipy.spatial.distance import squareform

files = sys.argv[1:]

data = 0
with open('MeanBranchLength.csv', 'w') as fh:
    for file in files:
        a = squareform(np.load(file))
        data += a
        print(file.split('-')[0], a.sum() / a.size, sep=',', file=fh)
    average = data / len(files) 
    np.save('Average_matrix.npy', average)