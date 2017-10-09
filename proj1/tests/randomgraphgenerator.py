import networkx as nx
from numpy.random import random_integers as random
from random import choice

if __name__ == "__main__":
	import sys
	if len(sys.argv) < 2:
		print "Usage <max_vertices>"

	#Generate random graph (probability of edge generation calculated so the graph is sparse)
	num_vertices = random(int(sys.argv[1]))
	graph = nx.fast_gnp_random_graph(num_vertices, p=1.0/float(num_vertices/2), directed=False)

	#If the graph is not connected, connect the the connected subgrafs
	if not nx.is_connected(graph):
		comps = list(nx.connected_components(graph))
		for i in xrange(len(comps)-1):
			graph.add_edge(choice(list(comps[i])), choice(list(comps[i+1])))

	assert nx.is_connected(graph), "Y U DO THIS?"

	#Print to output format
	edges = nx.to_edgelist(graph)
	print "%s %s" % (num_vertices, len(edges))
	for v1, v2, _ in edges:
		print "%s %s" % (v1+1, v2+1)
