#ifndef VERTEX_HEAP_H
#define VERTEX_HEAP_H

#include <vector>

class VertexHeap{
private:
    int _heap_size;
    std::vector<int> _heap_vector;
    std::vector<int> _vertex_to_pos;
    std::vector<int> _pos_to_vertex;

    inline void switch_vertices(int i1, int i2);
    inline int parent(int i);
    inline int child(int i, int side);

    void __push_up(int index);
    void __push_down(int index);


public:

    VertexHeap(int root, int number_of_vertices);
      
    bool empty();

    bool has_vertex(int vertex);

    void pop(int& vertex, int& value);

    void insert_vertex(int vertex, int value);

    void update_distance(int vertex, int new_distance);
};

#endif