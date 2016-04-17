#ifndef GRAPH_H
#define GRAPH_H

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
     *
     * @param vertex1
     *          the index of the first vertex
     * @param vertex2
     *          the index of the second vertex
     */
    void add_edge(int vertex1, int vertex2, int weight);

    /**
     * Executes the bellman-ford algorithm, with a virtual vertex, 
     * and returns the shortest distances from that vertex
     *
     * @param distances
     *          the vector to save the distances
     */
    void __bellman_ford_reweight(std::vector<int>& distances);

    /**
     * Executes the dijkstra's algorithm for the shortest path from a source to all vertex
     *
     * @param source_vertex
     *          the source vertex for the paths
     * @param weights
     *          the weight matrix to use (must only have positive weights)
     * @param distances
     *          the vector to save the distances calculated
     */
    void __dijkstras(int source_vertex, const std::vector<std::vector<int> >& weights, std::vector<int>& distances);

     /**
     * Finds the shortest distance from a list of vertex, to all verteces
     *
     * @param origins
     *          the list of vertex to calculate the distances
     * @param dist_matrix
     *          the matrix to save the distances (num_sources x num_vertex)
     */
    void find_shortest_paths(const std::vector<int>& origins,
                             std::vector<std::vector<int> >& dist_matrix);


};

#endif