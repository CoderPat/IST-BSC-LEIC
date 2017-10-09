#Project 1 - Essential people

This project consisted of, given a social network with the people in it and the (bidirectional) connections between them, finding the person(s) which are essential to the propagation of information. 
This means that, if that person is removed, information introduced to a person in the network can never reach everyone.

The solution consists of considering the social network as a graph, and then finding the articulation vertices (the vertices that separate two biconnected components).