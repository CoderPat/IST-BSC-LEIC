#include <vector>
#include <iterator>
#include <limits>
#include "graph.hpp"
#include "vertexheap.hpp"

/** Constructor to simply size the vertex vector */
Graph::Graph(int number_of_vertices) : _number_of_vertices(number_of_vertices), 
                                       _graph_lists(number_of_vertices, std::vector<std::pair<int, int> >()) {}


/** Adds the each vertex to the internal lists of the other one */
void Graph::add_edge(int vertex1, int vertex2, int weight){
    //Change 1-based identation to 0-based identation
    vertex1--; vertex2--;
    _graph_lists[vertex1].push_back(std::make_pair(vertex2, weight));
}

/** Executes the bellman-ford algorithm, with a virtual vertex,  and returns the shortest distances from that vertex */
void Graph::__bellman_ford_reweight(std::vector<int>& distances){

    for(int i = 0; i < _number_of_vertices; i++)
        distances[i] = 0;
    
    for(int i = 0; i < _number_of_vertices; i++)
        for(int j = 0; j < _number_of_vertices; j++)
            for(std::vector<std::pair<int, int> >::iterator vertex_pair_it = _graph_lists[j].begin(); vertex_pair_it != _graph_lists[j].end(); vertex_pair_it++)
                if(distances[(*vertex_pair_it).first] > distances[j] + (*vertex_pair_it).second)
                    distances[(*vertex_pair_it).first] = distances[j] + (*vertex_pair_it).second;
}

/** Reverse all the edges in the graph */
void Graph::__invert_graph(){

    std::vector<std::vector<std::pair<int, int> > > inverted_graph (_number_of_vertices, std::vector<std::pair<int, int> >());
    for(int vertex = 0; vertex < _number_of_vertices; vertex++)
        for(std::vector<std::pair<int, int> >::iterator vertex_pair_it = _graph_lists[vertex].begin(); vertex_pair_it != _graph_lists[vertex].end(); vertex_pair_it++)
            inverted_graph[(*vertex_pair_it).first].push_back(std::make_pair(vertex, (*vertex_pair_it).second));

    _graph_lists = inverted_graph;
}

/** Executes the dijsktras algorithm, based on the graph weights */
void Graph::__dijkstras(int source_vertex, std::vector<int>& distance_vector){

    VertexHeap vertex_heap(source_vertex, _number_of_vertices);
    std::vector<bool> visited(_number_of_vertices, false);

    for(int i = 0; (unsigned) i < distance_vector.size(); i++)
        distance_vector[i] = std::numeric_limits<int>::max();
    distance_vector[source_vertex] = 0;

    while(!vertex_heap.empty()){
        int vertex, distance;
        vertex_heap.pop(vertex, distance);

        visited[vertex] = true;

        for(std::vector<std::pair<int, int> >::iterator vertex_pair_it = _graph_lists[vertex].begin(); vertex_pair_it != _graph_lists[vertex].end(); vertex_pair_it++){
            int adj_vertex = (*vertex_pair_it).first, weight = (*vertex_pair_it).second;
            int new_distance = distance + weight;
            if(!visited[adj_vertex] && distance_vector[adj_vertex] > new_distance){
                vertex_heap.has_vertex(adj_vertex) ? 
                                    vertex_heap.update_value(adj_vertex , new_distance) :
                                    vertex_heap.insert_vertex(adj_vertex, new_distance);
                distance_vector[adj_vertex] = new_distance;
            }
        }
    }
}

/** Finds best vertex for a list of the vertices to meetup */
void Graph::find_best_meetup(const std::vector<int>& origins,
                               std::vector<int>& origin_distances,
                               int& location,
                               int& dist){

    std::vector<int> bell_distances(_number_of_vertices, 0);
    std::vector<int> distances(_number_of_vertices, 0);
    std::vector<int> sums(_number_of_vertices, 0);

    // Reweight the graph so it only has positive weights
    __bellman_ford_reweight(bell_distances);

    for(int vertex = 0; vertex < _number_of_vertices; vertex++)
        for(std::vector<std::pair<int, int> >::iterator vertex_pair_it = _graph_lists[vertex].begin(); vertex_pair_it != _graph_lists[vertex].end(); vertex_pair_it++)
            (*vertex_pair_it).second = (*vertex_pair_it).second + bell_distances[vertex] - bell_distances[(*vertex_pair_it).first];

    // Use dijkstras to find the distances to all points from all origins
    for(int i = 0 ; i < (int) origins.size(); i++){
        __dijkstras(origins[i]-1, distances);
        for(int j=0; j < _number_of_vertices; j++)
            if(sums[j] != std::numeric_limits<int>::max() && distances[j] != std::numeric_limits<int>::max())
                sums[j] += distances[j] + bell_distances[j] - bell_distances[origins[i]-1];
            else
                sums[j] = std::numeric_limits<int>::max();
    }

    // Find best vertex to meetup 
    location = -1;
    dist = std::numeric_limits<int>::max();
    for(int i = 0 ; i < (int) sums.size(); i++)
        if(sums[i] < dist){
            location = i;
            dist = sums[i];
        }

    if(location==-1)
        return;


    //Invert graph and calculate again the distances from all origins to the meetup
    __invert_graph();
    __dijkstras(location, distances);
    for(int i = 0 ; i < (int) origins.size(); i++)
        origin_distances[i] = distances[origins[i]-1] + bell_distances[location] - bell_distances[origins[i]-1];

    location++;
}

