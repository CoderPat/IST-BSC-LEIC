#include <iostream>
#include <string>
#include <vector> 
#include "graph.hpp"

/**
 * Program that recives a graph and prints the number graph articulations and the first and last articulation to the standard output
 */

int main() {

	//Get edges and vertices
	int num_vertices, num_edges;
	std::cin >> num_vertices >> num_edges;

	//Create graph and get edges
	Graph graph = Graph(num_vertices);
	for(int vertex1, vertex2, i = 0; i < num_edges; i++) {
		std::cin >> vertex1 >> vertex2;
		graph.addEdge(vertex1, vertex2);
	}

	//Get the articulations
	std::vector<int> articulations;
	articulations = graph.getArticulations();

	//Finally extract the info and print it
	if(articulations.empty()) {
		higherArti = -1;
		lowerArti = -1;
	}
	else {
		lowerArti = articulations.front();
		higherArti = articulations.back();
	}

	std::cout << articulations.size() << std::endl;
	std::cout << lowerArti << " " << higherArti << std::endl;
	return 0;
}
