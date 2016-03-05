#include <iostream>
#include <string>
#include "../include/Graph.hpp"

/*
 * Program that recives a graph and returns to std::cout information related to the graph articulations
 */

int main() {

	while(1) {

		int in1, in2, higherArti , lowerArti;
		Graph g = Graph(in1);
		
		std::cin >> in1 >> in2;
		std::vector<int> articulations;

		for(int i = 0; i < in2; i++) {
			std::cin >> in1 >> in2;
			g.addEdge(in1, in2);
		}

		articulations = g.getArticulations();
		higherArti = -1;
		if(articulations.size() == 0)
			lowerArti = -1;
		else
			lowerArti = articulations[0];

		for(int i = 0; i < articulations.size(); i++){
			if(articulations[i] < lowerArti)
				lowerArti = articulations[i];
			if(articulations[i] > higherArti)
				higherArti = articulations[i];
		}

		std::cout << articulations.size() << "\n";
		std::cout << lowerArti << " " << higherArti << srd::endl;

	}
	return 0;
}
