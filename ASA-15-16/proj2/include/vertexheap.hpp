#ifndef VERTEX_HEAP_H
#define VERTEX_HEAP_H

#include <vector>


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

#endif