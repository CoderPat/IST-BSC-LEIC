#include <list>
#include <vector>

/**
 * A class to represent a graph
 */
class Graph{
private:
	int _number_of_vertices;
	std::vector<std::list<int>> _graph_lists;

public:
	/**
	 * Initialize a disconected graph with a certain number of edges
	 *
	 * @param number_of_edges
	 *			the number of edges of the graph
	 */
	Graph(int number_of_edges);

	/**
	 * Add a edge to the graph
	 *
	 * @param vertix1
	 *			the index of the first vertix
	 * @param vertix2
	 *			the index of the second vertix
	 */
	void addEdge(int vertix1, int vertix2);

	/**
	 * Gets the articulations of the current graph
	 * (an articulation is a vertix that, if removed, would split the graph into two separate graph)
	 *
	 * @return a ordered vector with the indexes of the articulation nodes
	 */
	std::vector<int> getArticulations();
};