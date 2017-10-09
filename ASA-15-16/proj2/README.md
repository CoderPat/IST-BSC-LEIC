#Project 2 - Best meetup location

In this project, the problem consisted of, given a map with locations, the branches of a company (their locations), and the costs/profits of traversing between pairs of locations, finding a location for all the branches to meet up such as it maximizes the total profits for all the branches.

Again the solution consisted of considering the map as weighted graph, with the weights being the cost of traversing between the two locations (a profit is a negative costs). Then the solution consisted of finding the location for which the sum of the path distances for all the branch vertices was minimal. That could be done with 
an all-pair-minimum-distance algorithm (in our case, the Johnson's algorithm).