import networkx as nx
from numpy.random import random_integers as random
from numpy.random import binomial
from random import choice

if __name__ == "__main__":
	import sys
	if len(sys.argv) < 2:
		print "Usage <max_vertices>"

	MAX_WEIGHT = 20
	SOURCES_RATIO = 0.1

	#Generate random graph (probability of edge generation calculated so the graph is sparse)
	num_vertices = random(int(sys.argv[1]))
	graph = nx.fast_gnp_random_graph(num_vertices, p=1.0/float(num_vertices/2), directed=True)

	#If the graph is not connected, connect the the connected subgrafs
	if not nx.is_weakly_connected(graph):
		comps = list(nx.weakly_connected_components(graph))
		for i in xrange(len(comps)-1):
			graph.add_edge(choice(list(comps[i])), choice(list(comps[i+1])))

	weights = {}
	for edge in graph.edges_iter():
		weights[edge] = random(MAX_WEIGHT)

	nx.set_edge_attributes(graph, 'weight', weights)

	assert nx.is_weakly_connected(graph), "Y U DO THIS?"

	sources = []
	for i in xrange(num_vertices):
		if binomial(1, SOURCES_RATIO):
			sources.append(i+1)

	#Print to output format
	edges = nx.to_edgelist(graph)
	print "%s %s %s" % (num_vertices, len(sources), len(edges))
	for source in sources:
		print "%s" % source, 
	print ""

	for v1, v2, attr in edges:
		print "%s %s %s" % (v1+1, v2+1, attr['weight'])
