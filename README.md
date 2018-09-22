# Artificial-Intelligence
### Implementing Games using Artificial Intelligence Algorithms.
##### Game 1: DeliveryMan
The DeliveryMan game is a grid type game. Where it consists of connected points/nodes
with multiple packages located on random nodes across the grid and each of these packages has a certain destination. And the purpose of the game is for the car to deliver the packages to their destinations as fast/cheap as possible. Where the available moves to be done is for the car to go up, down, left or right within the border of the grid. Putting in mind that we can only carry one package at a time. Therefore, we classify this game as a graph search problem which can be solved using A* search algorithm. And due to the nature of the game; we can consider this to be multiple runs of the A* algorithm. Where we have more than one start node (packages locations) and goal node (packages destinations), and we need to reach them all. And as we move along the graph/grid (up, down, left or right), the roads conditions keep changing. Therefore, after every step we take, it is essential to re-calculate all the costs to the goal since the previous ones become outdated.

##### Game 2: WheresCroc
The Where’sCroc game is a graph type game. Where it consists of 40 connected
states/waterholes with a crocodile, the player, and 2 backpackers randomly located initially in one of these nodes. And as time passes by the crocodile and 2 backpackers keep moving in the graph. And they are only allowed to move from one waterhole to its connected waterholes. And the purpose of the game is for the player (computer) to find the crocodile before it goes unavailable and loses the game. Putting in mind that if the crocodile goes to the same waterhole as a backpacker, then the crocodile eats him and he dies. In addition, the player is only allowed to move from one waterhole to its connected waterholes. And the player can make 2 actions at a time before the others (the crocodile and the backpackers) move. Where these 2 actions can be the following:
1. Move 2 steps ahead.
2. Search the current waterhole and move only 1 step ahead.
3. Move only 1 step ahead and search the new waterhole.

Where information is given about the waterholes to help find croc. Where in each waterhole
there are 3 water sensors that records the hole’s salinity, phosphate and nitrogen levels. And the crocodile only sends the sensors values from his current waterhole. 
Since the position of the crocodile is unknown, while having the waterhole’s sensors observables, and we need to go to the position of the crocodile in order to find it. Therefore, we can classify this game as a Hidden Markov Model and a search graph problem.
