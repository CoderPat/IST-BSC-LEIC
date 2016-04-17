#include "graph.hpp"
#include <vector>
#include <limits>
#include <iostream>

int main() {

	//Eliminate overhead of using c++ i/o
	std::cout.sync_with_stdio(false);

	//Get number of vertices, branches, connections and locations;
	int num_vertices, num_branches, num_connections;
	std::cin >> num_vertices >> num_branches >> num_connections;

	//Get branches
	std::vector<int> branches;
	for(int i = 0; i < num_branches; i++) {
		int branch;
		std::cin >> branch;
		branches.push_back(branch);
	}

	//Create graph and get edges
	Graph graph = Graph(num_vertices);
	for(int i = 0; i < num_connections; i++) {
		int vertex1, vertex2, weight;
		std::cin >> vertex1 >> vertex2 >> weight;
		graph.add_edge(vertex1, vertex2, weight);
	}

	//Create distances matrix
	std::vector<std::vector<int> > distances(num_branches, std::vector<int>(num_vertices, 0));	

	//Apply Johnson algorithm to vertices in branches and update distances
	graph.find_shortest_paths(branches, distances);

	//Find location with lowest distance to all branches
	int distance = std::numeric_limits<int>::max(), location = 0;
	for(int i = 0; i < num_vertices; i++){
		int temp_distance = 0;
		for(int j = 0; j < num_branches; j++) {
			if(distances[j][1] == std::numeric_limits<int>::max()) {
				temp_distance = std::numeric_limits<int>::max();
				break;
			}
			temp_distance += distances[j][i];
		}
		if(distance > temp_distance) {
			distance = temp_distance;
			location = i; 
		}
		temp_distance = 0;
	}

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
		std::cout << distances[i][location] << " ";
	std::cout << std::endl;

	return 0;
}