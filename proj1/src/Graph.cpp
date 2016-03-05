#include <vector>
#include <list>
#include <algorithm>

/** Constructor to simply size the vertix vector */
Graph::Graph(int number_of_vertixes){
	for(int i=0; i < number_of_vertixes; i++)
		_graph_lists.push_back( std::list() );
}

/** Adds the each vertix to the internal lists of the other one */
void Graph::addEdge(int vertix1, int vertix2){
	if (std::find(std::begin(_graph_lists[vertix1]), std::end(_graph_lists[vertix1]), vertix2) == std::end(my_list)){
		_graph_lists[vertix1].push_back(vertix2);
		_graph_lists[vertix2].push_back(vertix1);
	}
}

/** 
 * Gets the articulations bu doing a depth-first search, keeping the discovery time and the lowpoint.
 * After that, returns the vertixes in which the lowpoint of the child vertix are higher than the vertix itself
 */
std::vector<int> Graph::getArticulations(){
	//TODO: Implement the algorithm
}