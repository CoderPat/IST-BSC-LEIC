#include <vector>
#include <list>
#include <utility>
#include <iterator>
#include <algorithm>
#include "graph.hpp"

/** Constructor to simply size the vertex vector */
Graph::Graph(int number_of_vertices) : _number_of_vertices(number_of_vertices){
	for(int i=0; i < number_of_vertices; i++)
		_graph_lists.push_back( std::list<int>() );
}

/** Adds the each vertex to the internal lists of the other one */
void Graph::addEdge(int vertex1, int vertex2){
	//Change 1-based identation to 0-based identation
	vertex1--; vertex2--;

	if (std::find(std::begin(_graph_lists[vertex1]), std::end(_graph_lists[vertex1]), vertex2) == std::end(_graph_lists[vertex1])){
		_graph_lists[vertex1].push_back(vertex2);
		_graph_lists[vertex2].push_back(vertex1);
	}
}

/** 
 * Performs iterative depth first traversal using a stack to order the visits and keeping the path visited
 * After that performs the back traversal to find the lowpoints
 */
void Graph::depthFirstTraversal(int root, int discovery_time[], int parent[], int lowpoint[]){
	int disc_time = 1;
	std::list<int> reverse_order;
	std::list<std::pair<int,int>> vertices_to_visit;

	//Perform the down traversal, getting the discovery times, parent and the order (uses stack with parent/vertex pair)
	vertices_to_visit.push_back(std::make_pair(-1, root));
	while(!vertices_to_visit.empty()){

		auto pair = vertices_to_visit.back();
		vertices_to_visit.pop_back();

		int vertex_parent = pair.first;
		int current_vertex = pair.second;
		if(discovery_time[current_vertex] == 0){
			discovery_time[current_vertex] = disc_time++;
			parent[current_vertex] = vertex_parent;
			reverse_order.push_front(current_vertex);

			for(int adj_vertex : _graph_lists[current_vertex])
				if(discovery_time[adj_vertex] == 0)
					vertices_to_visit.push_back(std::make_pair(current_vertex, adj_vertex));
		}
	}

	//Get the lowpoints for the vertices, by doing the back traversal
	for(int vertex : reverse_order){
		int min_discovery = discovery_time[vertex];
		for(int adj_vertex : _graph_lists[vertex]){
			if (discovery_time[adj_vertex] > discovery_time[vertex] &&  lowpoint[adj_vertex] < min_discovery)
				min_discovery = lowpoint[adj_vertex];
			if (discovery_time[adj_vertex] < discovery_time[vertex] && parent[vertex] != adj_vertex && discovery_time[adj_vertex] < min_discovery)
				min_discovery = discovery_time[adj_vertex];
		}
		lowpoint[vertex] = min_discovery;
	}
}

/** 
 * Gets the articulations bu doing a depth-first search, keeping the discovery time and the lowpoints.
 * After that, returns the vertices in which the lowpoint of the child vertex are higher than the vertex itself
 */
std::vector<int> Graph::getArticulations(){

	int discovery_time[_number_of_vertices] = {};
	int parent[_number_of_vertices];
	int lowpoint[_number_of_vertices];

	//Sets the root to the first vertex and calls the depth first search
	int root = 0;
	depthFirstTraversal(root, discovery_time, parent, lowpoint);

	//Finally check for articulations by finding vertices in which a childs lowpoint are higher than the parent's
	std::vector<int> articulations;
	for(int vertex = 0; vertex < _number_of_vertices; vertex++){

		//root case
		if(vertex==root){

			int root_children = 0;
			for(int adj_vertex : _graph_lists[vertex])
				if(parent[adj_vertex]==root){
					root_children++;
					if(root_children > 1){
						articulations.push_back(root+1);
						break;
					}
				}
			continue;
		}

		//other cases
		for(int adj_vertex : _graph_lists[vertex])
			if(parent[adj_vertex]==vertex && lowpoint[adj_vertex] >= discovery_time[vertex]){
				articulations.push_back(vertex+1);
				break;
			}
	}
	return articulations;
}