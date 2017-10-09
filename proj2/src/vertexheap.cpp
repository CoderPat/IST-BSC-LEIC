#include <vector>
#include "vertexheap.hpp"

#define LEFT true
#define RIGHT false

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