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

    /** The weight matrix for the edges */
    std::vector<std::vector<int> > _edges_weights;


public:
    /**
     * Initialize a disconected graph with a certain number of edges
     *
     * @param number_of_vertices
     *          the number of vertices of the graph
     */
    Graph(int number_of_vertices);

    /**
     * Add a edge to the graph
     *w
     * @param vertex1
     *          the index of the first vertex
     * @param vertex2
     *          the index of the second vertex
     */
    void add_edge(int vertex1, int vertex2);

    void __bellman_ford_reweight(std::vector<int>& distances);
    void dijkstras(int source_vertex, std::vector<int>& distances, const std::vector<std::vector<int>& weights);
    void find_shortest_paths(std::vector<std::vector<int> >& dist_matrix);


};