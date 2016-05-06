#include <vector>
#include <limits>
#include <numeric>
#include <iostream>

#define LEFT true
#define RIGHT false

//------------------------------------------------------------------------
//----------------------- DECLARATION ------------------------------------
//------------------------------------------------------------------------

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

/** A min heap to store vertices */
class VertexHeap{
private:

    int _heap_size;
    std::vector<int> _heap_vector;

    /* Vectors for vertex to index and reverse translation*/
    std::vector<int> _vertex_to_pos;
    std::vector<int> _pos_to_vertex;

    /* Heap navegation functions */
    inline void switch_vertices(int i1, int i2);
    inline int parent(int i);
    inline int child(int i, int side);

    /*Heap correcting functions */
    void __push_up(int index);
    void __push_down(int index);


public:

    /**
     * Initialize the heap with the root
     *
     * @param root
     *          the root vertix, to initialise the heap
     * @param number_of_vertices
     *          the number of vertices of the graph
     *
     */
    VertexHeap(int root, int number_of_vertices);
      
    /**
    * Checks if the heap still has vertices
    *  
    * @returns a boolen representing the emptiness
    */
    bool empty();

    /**
    * Checks if vertex is in the heap
    *  
    * @param vertex
    *           the vertex to check
    * @returns a boolen representing the presence
    */
    bool has_vertex(int vertex);

    /**
    * Pops the vertex with the minimun value
    *  
    * @param vertex
    *           the variable to save the vertex popped
    * @param value
    *           the variable to save the value associated with the vertex popped
    */
    void pop(int& vertex, int& value);

    /**
    * Inserts a vertex in the heap
    *  
    * @param vertex
    *           the vertex to insert
    * @param value
    *           its current value
    */
    void insert_vertex(int vertex, int value);

    /**
    * Updates the value of a vertex already in the heap
    *  
    * @param vertex
    *           the vertex to insert
    * @param value
    *           its current value
    */
    void update_value(int vertex, int new_distance);
};

//------------------------------------------------------------------------
//---------------------- MAIN --------------------------------------------
//------------------------------------------------------------------------

int main() {

    //Eliminate overhead of using c++ i/o
    std::cout.sync_with_stdio(false);

    //Get number of vertices, branches, connections and locations;
    int num_vertices, num_branches, num_connections;
    std::cin >> num_vertices >> num_branches >> num_connections;

    //Get branches
    std::vector<int> branches(num_branches, 0);
    for(int i = 0; i < num_branches; i++) 
        std::cin >> branches[i];
    

    //Create graph and get edges
    Graph graph = Graph(num_vertices);
    for(int i = 0; i < num_connections; i++) {
        int vertex1, vertex2, weight;
        std::cin >> vertex1 >> vertex2 >> weight;
        graph.add_edge(vertex1, vertex2, weight);
    }

    //Create distances matrix
    std::vector<int> distances(num_branches, 0);    
    int distance, location;

    //Apply Johnson algorithm to vertices in branches and update distances
    graph.find_best_meetup(branches, distances, location, distance);

    if(distance == std::numeric_limits<int>::max()) {
        std::cout << "N" << std::endl;
        return 0;
    }

    //Prints best meeting location and its distance from all branches
    std::cout << location << " " << distance << std::endl;

    //Prints minimal distance between all branches and best meeting location
    location--;
    for(int i = 0; i < num_branches; i++) 
        std::cout << distances[i] << " ";
    std::cout << std::endl;

    return 0;
}

//------------------------------------------------------------------------
//---------------------- IMPLEMENTATION ----------------------------------
//------------------------------------------------------------------------

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

/** Contructor to initialise heap */
VertexHeap::VertexHeap(int root, int number_of_vertices) : _heap_size(0),
                                                           _heap_vector(number_of_vertices, 0),
                                                           _vertex_to_pos(number_of_vertices, 0) ,
                                                           _pos_to_vertex(number_of_vertices, 0){ 
    for(int i = 0; (unsigned) i < _vertex_to_pos.size(); i++)
        _pos_to_vertex[i] = _vertex_to_pos[i] = i;

    insert_vertex(root, 0);
}

/** Checks if the heap is empty*/
bool VertexHeap::empty(){
        return _heap_size == 0;
}

/** Checks if a vertex is in the heap by checking if its index is smaller than the tree size */
bool VertexHeap::has_vertex(int vertex){
    return _heap_size != 0 && _vertex_to_pos[vertex] < _heap_size;
}


/** Pops the minimum valued vertex, and corrects the heap */ 
void VertexHeap::pop(int& vertex, int& value){
    switch_vertices(0, _heap_size-- - 1);
    __push_down(0);
    value = _heap_vector[_heap_size];
    vertex = _pos_to_vertex[_heap_size];
}

/** Inserts a vertex at the end of the heap and then corrects the heap */
void VertexHeap::insert_vertex(int vertex, int value){

    _heap_vector[_vertex_to_pos[vertex]] = value;

    if(_vertex_to_pos[vertex] > _heap_size)
        switch_vertices(_vertex_to_pos[vertex], _heap_size);

    __push_up(_heap_size++);
}

/** Updates the value of a vertex and then corrects the heap */
void VertexHeap::update_value(int vertex, int new_distance){
    int index = _vertex_to_pos[vertex];
    _heap_vector[index] = new_distance;
    __push_up(index);
}

/** Fixes the location of a element in the heap by pushing it up to its correct place */
void VertexHeap::__push_up(int index){
    if(index == 0)
        return;

    int parent_index = parent(index);
    if(_heap_vector[index] < _heap_vector[parent_index]){
        switch_vertices(index, parent_index);
        __push_up(parent_index);
    }
}

/** Fixes the location of a element in the heap by pushing it down to its correct place */
void VertexHeap::__push_down(int index){
    int left_c = child(index, LEFT), right_c = child(index, RIGHT);
    if(right_c < _heap_size){
        if (_heap_vector[index] > _heap_vector[left_c] && _heap_vector[left_c] < _heap_vector[right_c]){
            switch_vertices(index, left_c);
            __push_down(left_c);
        }
        else if (_heap_vector[index] > _heap_vector[right_c]){
           switch_vertices(index, right_c);
            __push_down(right_c); 
        }
    }

    else if(right_c == _heap_size && _heap_vector[index] > _heap_vector[left_c])
        switch_vertices(index, left_c);
}

/** Switches two elements in the heap */
inline void VertexHeap::switch_vertices(int i1, int i2) { 
    int v1 = _pos_to_vertex[i1], v2 = _pos_to_vertex[i2];
    int aux = _heap_vector[i1];

    _heap_vector[i1] = _heap_vector[i2];
    _heap_vector[i2] = aux;

    _pos_to_vertex[i1] = v2;
    _pos_to_vertex[i2] = v1;
    _vertex_to_pos[v1] = i2;
    _vertex_to_pos[v2] = i1;
}

/** Parent of an element */
inline int VertexHeap::parent(int i) {return (i-1)/2;}

/** Left/Right child of an element */
inline int VertexHeap::child(int i, int side) {return side==LEFT ? i*2 + 1 : (i+1)*2;}