#include <vector>
#include "vertexheap.hpp"

#define LEFT true
#define RIGHT false

VertexHeap::VertexHeap(int root, int number_of_vertices) : _heap_size(0),
                                                           _heap_vector(number_of_vertices, 0),
                                                           _vertex_to_pos(number_of_vertices, 0) ,
                                                           _pos_to_vertex(number_of_vertices, 0){ 
    for(int i = 0; (unsigned) i < _vertex_to_pos.size(); i++)
        _pos_to_vertex[i] = _vertex_to_pos[i] = i;

    insert_vertex(root, 0);
}


bool VertexHeap::empty(){
        return _heap_size == 0;
}

void VertexHeap::pop(int& vertex, int& value){
    switch_vertices(0, _heap_size-- - 1);
    __push_down(0);
    value = _heap_vector[_heap_size];
    vertex = _pos_to_vertex[_heap_size];
}

void VertexHeap::insert_vertex(int vertex, int value){

    _heap_vector[_vertex_to_pos[vertex]] = value;

    if(_vertex_to_pos[vertex] > _heap_size)
        switch_vertices(_vertex_to_pos[vertex], _heap_size);

    __push_up(_heap_size++);
}

bool VertexHeap::has_vertex(int vertex){
    return _heap_size != 0 && _vertex_to_pos[vertex] < _heap_size;
}

void VertexHeap::update_distance(int vertex, int new_distance){
    int index = _vertex_to_pos[vertex];
    _heap_vector[index] = new_distance;
    __push_up(index);
}

void VertexHeap::__push_up(int index){
    if(index == 0)
        return;

    int parent_index = parent(index);
    if(_heap_vector[index] < _heap_vector[parent_index]){
        switch_vertices(index, parent_index);
        __push_up(parent_index);
    }
}

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

inline int VertexHeap::parent(int i) {return (i-1)/2;}
inline int VertexHeap::child(int i, int side) {return side==LEFT ? i*2 + 1 : (i+1)*2;}