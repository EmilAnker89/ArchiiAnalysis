#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import nltk
import MySQLdb as mdb
import numpy as np
import re
sys.path.append('/home/emil/Desktop/Archii-repos/background/archii')
from background.ml.classification.feature_extractor import FeatureExtractor


con = mdb.connect('localhost', 'root', 'Christin4', 'docs')
with con:
	cur = con.cursor()
	cur.execute("SELECT text FROM document limit 100;")

	rows = cur.fetchall()

#considering lowering amount of characters: output is 300+Mb as is
list_docs = [re.sub(r"\\n","",str(i))[:1000] for i in rows]
feat = FeatureExtractor()
sparse_matrix = feat.fit_transform(list_docs)
full_matrix = sparse_matrix.toarray()
print(full_matrix.shape)
print(len(list_docs))
print(len(rows))
np.savetxt("/home/emil/Desktop/test_test.csv",full_matrix,delimiter=";")
