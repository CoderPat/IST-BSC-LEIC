#include <vector>
#include <list>
#include <iterator>
#include <limits>
#include "graph.hpp"
#include "vertexheap.hpp"

/** Constructor to simply size the vertex vector */
Graph::Graph(int number_of_vertices) : _number_of_vertices(number_of_vertices), 
                                       _graph_lists(number_of_vertices, std::list<int>() ),
                                       _edges_weights(number_of_vertices, std::vector<int>(number_of_vertices, 0)){}


/** Adds the each vertex to the internal lists of the other one */
void Graph::add_edge(int vertex1, int vertex2, int weight){
    //Change 1-based identation to 0-based identation
    vertex1--; vertex2--;

    _graph_lists[vertex1].push_back(vertex2);
    _edges_weights[vertex1][vertex2]=weight;
}


void Graph::__bellman_ford_reweight(std::vector<int>& distances){

    for(int i = 0; i < _number_of_vertices; i++)
        distances[i] = 0;
    
    for(int i = 0; i < _number_of_vertices; i++)
        for(int j = 0; j < _number_of_vertices; j++)
            for(std::list<int>::iterator vertex_it = _graph_lists[j].begin(); vertex_it != _graph_lists[j].end(); vertex_it++)
                if(distances[*vertex_it] > distances[j] + _edges_weights[j][*vertex_it])
                    distances[*vertex_it] = distances[j] + _edges_weights[j][*vertex_it];
}

void Graph::__dijkstras(int source_vertex, 
                       const std::vector<std::vector<int> >& weights,
                       std::vector<int>& distance_vector){

    VertexHeap vertex_heap(source_vertex, _number_of_vertices);
    std::vector<bool> visited(_number_of_vertices, false);
    for(int i = 0; (unsigned) i < distance_vector.size(); i++)
        distance_vector[i] = std::numeric_limits<int>::max();
    distance_vector[source_vertex] = 0;

    while(!vertex_heap.empty()){
        int vertex, distance;
        vertex_heap.pop(vertex, distance);

        visited[vertex] = true;

        for(std::list<int>::iterator adj_vertex_it = _graph_lists[vertex].begin(); adj_vertex_it != _graph_lists[vertex].end(); adj_vertex_it++){
            int new_distance = distance + weights[vertex][*adj_vertex_it];
            if(!visited[*adj_vertex_it] && distance_vector[*adj_vertex_it] > new_distance){
                vertex_heap.has_vertex(*adj_vertex_it) ? 
                                    vertex_heap.update_value(*adj_vertex_it , new_distance) :
                                    vertex_heap.insert_vertex(*adj_vertex_it, new_distance);
                distance_vector[*adj_vertex_it] = new_distance;
            }
        }
        
    }
}

void Graph::find_shortest_paths(const std::vector<int>& origins, 
                                std::vector<std::vector<int> >& distances){

    std::vector<int> bell_distances(_number_of_vertices, 0);
    std::vector<std::vector<int> > positive_weigths(_number_of_vertices, std::vector<int>(_number_of_vertices, 0) );
    
    __bellman_ford_reweight(bell_distances);

    
    for(int vertex = 0; vertex < _number_of_vertices; vertex++)
        for(std::list<int>::iterator adj_vertex_it = _graph_lists[vertex].begin(); adj_vertex_it != _graph_lists[vertex].end(); adj_vertex_it++)
            positive_weigths[vertex][*adj_vertex_it] = _edges_weights[vertex][*adj_vertex_it] + 
                                                       bell_distances[vertex] - bell_distances[*adj_vertex_it];

    for(int i = 0 ; i < (int) origins.size(); i++){
        __dijkstras(origins[i]-1, positive_weigths, distances[i]);
        for(int j=0; j < _number_of_vertices; j++)
            distances[i][j] += bell_distances[j] - bell_distances[origins[i]-1];;
    }
    
}

