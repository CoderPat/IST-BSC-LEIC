#include <iostream>
#include <string>
#include <vector> 
#include "graph.hpp"


/**
 * Program that receives a graph and prints the number of graph articulations and the first and last articulation to the standard output
 */

int main() {

	//Eliminate overhead of using c++ i/o
	std::ios::sync_with_stdio(false);

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
	std::list<int> articulations;
	articulations = graph.getArticulations();

	//Finally extract the info and print it
	int higher_artic, lower_artic;
	if(articulations.empty()) {
		higher_artic = -1;
		lower_artic = -1;
	}
	else {
		lower_artic = articulations.front();
		higher_artic = articulations.back();
	}

	std::cout << articulations.size() << std::endl;
	std::cout << lower_artic << " " << higher_artic << std::endl;
	return 0;
}
