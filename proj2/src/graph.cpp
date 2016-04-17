#include <vector>
#include <list>
#include <iterator>
#include "graph.hpp"
#include "vertexheap.hpp"

/** Constructor to simply size the vertex vector */
Graph::Graph(int number_of_vertices) : _number_of_vertices(number_of_vertices), 
                                       _graph_lists(number_of_vertices, std::list<int>() ),
                                       _edges_weights(number_of_vertices, std::vector<int>(number_of_vertices, 0)){}


/** Adds the each vertex to the internal lists of the other one */
void Graph::addEdge(int vertex1, int vertex2, int weight){
    //Change 1-based identation to 0-based identation
    vertex1--; vertex2--;

    _graph_lists[vertex1].push_back(vertex2);
    _edges_weights[vertex1][vertex2]=weight;
}


void Graph::__bellman_ford_reweight(std::vector<int>& distances){
    //TODO
}

void Graph::__dijkstra(int source_vertex, 
                       std::vector<int>& distance_vector, 
                       const std::vector<std::vector<int> >& weights){

    VertexHeap vertex_heap(source_vertex, _number_of_vertices);
    //TODO
}

void Graph::find_shortest_paths(const std::vector<int>& origins, 
                                std::vector<std::vector<int> >& distances){

    std::vector<int> bell_distances;
    std::vector<std::vector<int> > positive_weigths (std::vector<int>(_number_of_vertices, 0) );
    __bellman_ford(bell_distances);

    for(int vertex = 0; vertex < _number_of_vertices; vertex++)
        for(std::list<int>::iterator adj_vertex_it = _graph_lists[vertex].begin(); adj_vertex_it != _graph_lists[vertex].end(); adj_vertex_it++)
            positive_weigths[vertex][*adj_vertex_it] = _edges_weights[vertex][*adj_vertex_it] + 
                                                       bell_distances[vertex] - bell_distances[*adj_vertex_it];



    
}