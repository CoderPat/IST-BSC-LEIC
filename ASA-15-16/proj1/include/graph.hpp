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
	std::vector<std::list<int> > _graph_lists;

public:
	/**
	 * Initialize a disconected graph with a certain number of edges
	 *
	 * @param number_of_vertices
	 *			the number of vertices of the graph
	 */
	Graph(int number_of_vertices);

	/**
	 * Add a edge to the graph
	 *
	 * @param vertex1
	 *			the index of the first vertex
	 * @param vertex2
	 *			the index of the second vertex
	 */
	void addEdge(int vertex1, int vertex2);

	/**
	 * Performs a depth first traversal of the tree, getting the discovery times, parents and lowpoints for the vertices
	 * 
	 * @param root
	 * 			where to start the traversal
	 * @param discovery_time
	 *			vector to save the discovery times
	 * @param parent
	 *			vector to save the parents in the traversal
	 * @param lowpoint
	 *			vector to save the lowpoints
	 */
	void depthFirstTraversal(int root, std::vector<int>& discovery_time, std::vector<int>& parent, std::vector<int>& lowpoint);

	/**
	 * Gets the articulations of the current graph
	 * (an articulation is a vertix that, if removed, would split the graph into two separate graph)
	 *
	 * @return a ordered vector with the indexes of the articulation nodes
	 */
	std::list<int> getArticulations();
};

