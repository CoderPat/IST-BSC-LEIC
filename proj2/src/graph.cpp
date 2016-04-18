#include <vector>
#include <list>
#include <iterator>
#include <limits>
#include "graph.hpp"
#include "vertexheap.hpp"

/** Constructor to simply size the vertex vector */
Graph::Graph(int number_of_vertices) : _number_of_vertices(number_of_vertices), 
                                       _graph_lists(number_of_vertices, std::list<std::pair<int, int> >()) {}


/** Adds the each vertex to the internal lists of the other one */
void Graph::add_edge(int vertex1, int vertex2, int weight){
    //Change 1-based identation to 0-based identation
    vertex1--; vertex2--;
    _graph_lists[vertex1].push_back(std::make_pair(vertex2, weight));
}


void Graph::__bellman_ford_reweight(std::vector<int>& distances){

    for(int i = 0; i < _number_of_vertices; i++)
        distances[i] = 0;
    
    for(int i = 0; i < _number_of_vertices; i++)
        for(int j = 0; j < _number_of_vertices; j++)
            for(std::list<std::pair<int, int> >::iterator vertex_pair_it = _graph_lists[j].begin(); vertex_pair_it != _graph_lists[j].end(); vertex_pair_it++)
                if(distances[(*vertex_pair_it).first] > distances[j] + (*vertex_pair_it).second)
                    distances[(*vertex_pair_it).first] = distances[j] + (*vertex_pair_it).second;
}

void Graph::__dijkstras(int source_vertex, 
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

        for(std::list<std::pair<int, int> >::iterator vertex_pair_it = _graph_lists[vertex].begin(); vertex_pair_it != _graph_lists[vertex].end(); vertex_pair_it++){
            int new_distance = distance + (*vertex_pair_it).second;
            if(!visited[(*vertex_pair_it).first] && distance_vector[(*vertex_pair_it).first] > new_distance){
                vertex_heap.has_vertex((*vertex_pair_it).first) ? 
                                    vertex_heap.update_value((*vertex_pair_it).first , new_distance) :
                                    vertex_heap.insert_vertex((*vertex_pair_it).first, new_distance);
                distance_vector[(*vertex_pair_it).first] = new_distance;
            }
        }
        
    }
}


void Graph::find_shortest_paths(const std::vector<int>& origins,
                               std::vector<int>& origin_distances,
                               int& location,
                               int& dist){

    std::vector<int> bell_distances(_number_of_vertices, 0);
    std::vector<int> distances(_number_of_vertices, 0);
    std::vector<int> sums(_number_of_vertices, 0);

    __bellman_ford_reweight(bell_distances);

    for(int vertex = 0; vertex < _number_of_vertices; vertex++)
        for(std::list<std::pair<int, int> >::iterator vertex_pair_it = _graph_lists[vertex].begin(); vertex_pair_it != _graph_lists[vertex].end(); vertex_pair_it++)
            (*vertex_pair_it).second = (*vertex_pair_it).second + bell_distances[vertex] - bell_distances[(*vertex_pair_it).first];

    for(int i = 0 ; i < (int) origins.size(); i++){
        __dijkstras(origins[i]-1, distances);
        for(int j=0; j < _number_of_vertices; j++)
            if(sums[j] != std::numeric_limits<int>::max() && distances[j] != std::numeric_limits<int>::max())
                sums[j] += distances[j] + bell_distances[j] - bell_distances[origins[i]-1];
            else
                sums[j] = std::numeric_limits<int>::max();
    }

    location = -1;
    dist = std::numeric_limits<int>::max();
    for(int i = 0 ; i < (int) sums.size(); i++)
        if(sums[i] < dist){
            location = i;
            dist = sums[i];
        }

    if(location==-1)
        return;


    std::vector<std::list<std::pair<int, int> > > inverted_graph (_number_of_vertices, std::list<std::pair<int, int> >());
    for(int vertex = 0; vertex < _number_of_vertices; vertex++)
        for(std::list<std::pair<int, int> >::iterator vertex_pair_it = _graph_lists[vertex].begin(); vertex_pair_it != _graph_lists[vertex].end(); vertex_pair_it++)
            inverted_graph[(*vertex_pair_it).first].push_back(std::make_pair(vertex, (*vertex_pair_it).second));


    _graph_lists = inverted_graph;
    __dijkstras(location, distances);
    for(int i = 0 ; i < (int) origins.size(); i++)
        origin_distances[i] = distances[origins[i]-1] + bell_distances[location] - bell_distances[origins[i]-1];
}

