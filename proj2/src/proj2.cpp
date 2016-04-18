#include "graph.hpp"
#include <vector>
#include <limits>
#include <numeric>
#include <iostream>

int main() {

	//Eliminate overhead of using c++ i/o
	std::cout.sync_with_stdio(false);

	//Get number of vertices, branches, connections and locations;
	int num_vertices, num_branches, num_connections;
	std::cin >> num_vertices >> num_branches >> num_connections;

	//Get branches
	std::vector<int> branches(num_branches, 0);
	for(int i = 0; i < num_branches; i++) 
		std::cin >> branches[i];
	

	//Create graph and get edges
	Graph graph = Graph(num_vertices);
	for(int i = 0; i < num_connections; i++) {
		int vertex1, vertex2, weight;
		std::cin >> vertex1 >> vertex2 >> weight;
		graph.add_edge(vertex1, vertex2, weight);
	}

	//Create distances matrix
	std::vector<int> distances(num_branches, 0);	
	int distance, location;

	//Apply Johnson algorithm to vertices in branches and update distances
	graph.find_shortest_paths(branches, distances, location, distance);

	if(distance == std::numeric_limits<int>::max()) {
		std::cout << "N" << std::endl;
		return 0;
	}

	//Calculate best meeting location correct id
	location++;

	//Prints best meeting location and its distance from all branches
	std::cout << location << " " << distance << std::endl;

	//Prints minimal distance between all branches and best meeting location
	location--;
	for(int i = 0; i < num_branches; i++) 
		std::cout << distances[i] << " ";
	std::cout << std::endl;

	return 0;
}