#include <list>
#include <vector>

/**
 * A class to represent a graph
 */
class Graph{
private:

	/** The total number of vertices*/
	int _number_of_vertices;

	/** The internal graph representation, as a vector of lists */
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
	 * Performs a depth first traversal of the tree, getting the discovery times, parents and lowpoints for the vertices
	 * 
	 * @param root
	 * 			where to start the traversal
	 * @param discovery_time
	 *			array to save the discovery times
	 * @param parent
	 *			array to save the parents in the traversal
	 * @param lowpoint
	 *			array to save the lowpoints
	 */
	void depthFirstTraversal(int root, int discovery_time[], int parent[], int lowpoint[]);

	/**
	 * Gets the articulations of the current graph
	 * (an articulation is a vertix that, if removed, would split the graph into two separate graph)
	 *
	 * @return a ordered vector with the indexes of the articulation nodes
	 */
	std::vector<int> getArticulations();
};