#include <iostream>
#include <string>
#include <vector> 
#include "graph.hpp"

/**
 * Program that recives a graph and returns to std::cout information related to the graph articulations
 */

int main() {

	int in1, in2, numEdges ,higherArti , lowerArti;
	std::cin >> in1 >> numEdges;
	Graph g = Graph(in1);
	std::vector<int> articulations;

	for(int i = 0; i < numEdges; i++) {
		std::cin >> in1 >> in2;
		g.addEdge(in1, in2);
	}

	articulations = g.getArticulations();
	if(articulations.empty()) {
		higherArti = -1;
		lowerArti = -1;
	}
	else {
		lowerArti = articulations.front();
		higherArti = articulations.back();
	}

	std::cout << articulations.size() << "\n";
	std::cout << lowerArti << " " << higherArti << std::endl;
	return 0;
}
