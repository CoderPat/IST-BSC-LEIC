#ifndef GRAPH_H
#define GRAPH_H

#include <vector>

/**
 * A class to represent a graph
 */
class Graph{
private:

    /** The total number of vertices*/
    int _number_of_vertices;

    /** The internal graph representation, as a vector of lists */
    std::vector<std::vector<std::pair<int, int> > > _graph_lists;



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
     * Invertes all the edges in the graph 
     */
    void __invert_graph();

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
     * @param distances
     *          the vector to save the distances calculated
     */
    void __dijkstras(int source_vertex, std::vector<int>& distances);

     /**
     * Finds the shortest distance from a list of vertex, to all verteces
     *
     * @param origins
     *          the list of vertex to calculate the distances
     * @param origins_distances
     *          the vector to save the distances to the best meetup
     * @param location
     *          the variable to save the best meetup location
     * @param dist
     *          the variable to save the overall sum of the distances
     *
     */
    void find_best_meetup(const std::vector<int>& origins,
                               std::vector<int>& origins_distances,
                               int& location,
                               int& dist);


};

#endif