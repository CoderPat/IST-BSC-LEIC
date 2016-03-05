#include <vector>
#include <list>
#include <iterator>
#include <algorithm>
#include "graph.hpp"

/** Constructor to simply size the vertix vector */
Graph::Graph(int number_of_vertices) : _number_of_vertices(number_of_vertices){
	for(int i=0; i < number_of_vertices; i++)
		_graph_lists.push_back( std::list<int>() );
}

/** Adds the each vertix to the internal lists of the other one */
void Graph::addEdge(int vertix1, int vertix2){
	//Change 1-based identation to 0-based identation
	vertix1--; vertix2--;

	if (std::find(std::begin(_graph_lists[vertix1]), std::end(_graph_lists[vertix1]), vertix2) == std::end(_graph_lists[vertix1])){
		_graph_lists[vertix1].push_back(vertix2);
		_graph_lists[vertix2].push_back(vertix1);
	}
}

/** 
 * Gets the articulations bu doing a depth-first search, keeping the discovery time and the lowpoints.
 * After that, returns the vertices in which the lowpoint of the child vertix are higher than the vertix itself
 */
std::vector<int> Graph::getArticulations(){
	//DFS variables
	int disc_time = 1;
	int discovery_time[_number_of_vertices] = {};
	int parent[_number_of_vertices];
	std::list<int> reverse_order;
	std::list<int> vertices_to_visit;

	//Perform the DFS, getting the discovery times and the order
	vertices_to_visit.push_back(0);
	parent[0] = -1;
	while(!vertices_to_visit.empty()){

		int current_vertix = vertices_to_visit.back();
		discovery_time[current_vertix] = disc_time++;
		reverse_order.push_front(current_vertix);
		vertices_to_visit.pop_back();

		for(int adj_vertix : _graph_lists[current_vertix])
			if(discovery_time[adj_vertix] != 0)
				vertices_to_visit.push_back(adj_vertix);
	}

	//Get the lowpoints for the vertices
	int lowpoint[_number_of_vertices];
	for(int vertix : reverse_order){
		int min_discovery = discovery_time[vertix];
		for(int adj_vertix : _graph_lists[vertix]){
			if (discovery_time[adj_vertix] > discovery_time[vertix] && lowpoint[adj_vertix] < min_discovery)
				min_discovery = lowpoint[adj_vertix];
			if (discovery_time[adj_vertix] < discovery_time[vertix] && discovery_time[adj_vertix] < min_discovery)
				min_discovery = discovery_time[adj_vertix];
		}
	}

	//Finally check for articulations by findinf vertices in which the children lowpoint are higher than the parent's
	std::vector<int> articulations;
	for(int vertix = 0; vertix < _number_of_vertices; vertix++){
		for(int adj_vertix : _graph_lists[vertix])
			if(lowpoint[adj_vertix] <= lowpoint[adj_vertix])
				continue;
		articulations.push_back(vertix+1);
	}

	return articulations;
}