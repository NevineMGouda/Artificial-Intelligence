list.of.packages <- c("igraph")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(igraph)

#' @export
randomWC=function(moveInfo,readings,positions,edges,probs) {
  moveInfo$moves=c(sample(getOptions(positions[3],edges),1),0)  
  return(moveInfo)
}

#' @export
manualWC=function(moveInfo,readings,positions,edges,probs) {
  options=getOptions(positions[3],edges)
  print("Move 1 options (plus 0 for search):")
  print(options)
  mv1=readline("Move 1: ")
  if (mv1=="q") {stop()}
  if (!mv1 %in% options && mv1 != 0) {
    warning ("Invalid move. Search ('0') specified.")
    mv1=0
  }
  if (mv1!=0) {
    options=getOptions(mv1,edges)
  }
  print("Move 2 options (plus 0 for search):")
  print(options)
  mv2=readline("Move 2: ")    
  if (mv2=="q") {stop()}
  if (!mv1 %in% options && mv1 != 0) {
    warning ("Invalid move. Search ('0') specified.")
    mv2=0
  }
  moveInfo$moves=c(mv1,mv2)
  return(moveInfo)
}

getProbabilities= function(croc_readings, sensors_prob){
  states_count =nrow(sensors_prob$salinity)
  sensors_count = length(croc_readings)
  probabilities =  matrix (nrow=states_count, ncol=sensors_count)
  colnames(probabilities) <-c("salinity","phosphate","nitrogen")
  # j = 1 => salinity
  # j = 2 => phosphate
  # j = 3 => nitrogen
  for (j in 1:sensors_count){
    croc_sensor = croc_readings[j]
    for (i in 1:states_count){
      if (j==1){
        mean_val = sensors_prob$salinity[i,1]
        standard_dev = sensors_prob$salinity[i,2]
        probabilities[i,"salinity"] = dnorm(croc_sensor, mean= mean_val, sd=standard_dev)
      }
      else if(j == 2){
        mean_val = sensors_prob$phosphate[i,1]
        standard_dev = sensors_prob$phosphate[i,2]
        probabilities[i,"phosphate"] = dnorm(croc_sensor, mean= mean_val, sd=standard_dev)
      }
      else{
        mean_val = sensors_prob$nitrogen[i,1]
        standard_dev = sensors_prob$nitrogen[i,2] 
        probabilities[i,"nitrogen"] = dnorm(croc_sensor, mean= mean_val, sd=standard_dev)
      }
    }
  }
  return(probabilities)
}

getNeighbours = function(states_edges, size){
  new_edges = matrix(c(states_edges[1,]),ncol = 2)
  edges_count = nrow(states_edges) 
  dict = list()
  for (i in 1:edges_count){
    state = states_edges[i, 1]
    neighbour = states_edges[i, 2]
    dict_state = dict[[toString(state)]]
    dict_neighbour = dict[[toString(neighbour)]]
    if (is.null(dict_state)){
      dict[[toString(state)]] = c(state, neighbour)
      
    }
    else{
      dict[[toString(state)]] = c(dict[[toString(state)]], neighbour)
    }
    
    if (is.null(dict_neighbour)){
      dict[[toString(neighbour)]] = c(state, neighbour)
    }
    else{
      dict[[toString(neighbour)]] = c(dict[[toString(neighbour)]], state)
    }
  }
  return(dict)
}

getTransitionMatrix = function(neighbours_list, size){
  transition_matrix = matrix(0,ncol=size, nrow=size)
  for (i in 1:length(neighbours_list)){
    state_index = toString(i)
    neighbours = neighbours_list[[state_index]]
    prob = 1/length(neighbours)
    for(j in 1:length(neighbours)){
      neighbour_index = neighbours[j]
      transition_matrix[i,neighbour_index] = prob
    }
  }
  return(transition_matrix)
}

shortest_path = function(edges, start, goal){
  new_edges = as.vector(t(edges))
  g<-graph(new_edges, n=max(new_edges), directed=FALSE)
  #plot(g)
  shortest_path_info = get.shortest.paths(g, from=start, to=goal)
  nodes_to_goal=length(shortest_path_info$vpath[[1]])
  nodes = matrix(ncol=nodes_to_goal, nrow=1)
  for(i in 1:nodes_to_goal){
    nodes[i] = shortest_path_info$vpath[[1]][i]
  }
  return(nodes)
}

forwardalg = function(neighbours_list, previous_forward, emission, transition_matrix){
  states_count = length(previous_forward)
  #forward_next = matrix(0,ncol = states_count, nrow = 1)
  forward_next = rep_len(0,states_count)
  sum = 0
  # Each iteration calculates f ~ summ(F((t-1)i) * T(in)) En where summation is done where i = index of neighbours of current state
  for (i in 1:states_count){
    state_prob = 0
    state_index = toString(i)
    neighbours = neighbours_list[[state_index]]
    state_emission_value = emission[i]
    for (j in 1:length(neighbours)){
      neighbour_index = neighbours[j]
      transition_value = transition_matrix[neighbour_index, i]
      neighbour_previous_forward = previous_forward[j]
      state_prob = state_prob + (previous_forward * transition_value)
    }
    state_prob = state_prob * state_emission_value
    sum = sum + state_prob
    forward_next[i] = state_prob
  }
  # Normalize probabilities
  for(i in 1:states_count){
    value = forward_next[i]/sum
    forward_next[i] = value
  }
  return(forward_next)
}

choose_next_step = function(path, player_position, state_max_probability, max_probability){
  if(length(path) < 2 | player_position==path[2]){
    actions = c(0, 0)
  }
  #a most probable state and its at least 2 steps away then get as closer to it as possible
  else if (state_max_probability!=path[2] && state_max_probability!=path[3] && max_probability>0.8){
    actions = c(path[2], path[3])
  }
  #a most probable state and it is 1 step away then get go to the node and then explore it
  ## mesh kda mmkn tkon el ableha wasn't searched?
  else if(state_max_probability!=path[2] && state_max_probability==path[3] && max_probability>0.8){
    actions = c(path[2], 0)
  }
  else{
    actions = c(0, path[2])
  }
  return(actions)
}

ourAlgorithm = function(moveInfo, readings, positions, edges, probs){
  states_count =nrow(probs$salinity)
  #Check if this is the first iteration
  if(length(moveInfo$mem)==0){
    #Create initial_f, neighbours list and transistion matrix
    neighbours_list=getNeighbours(states_edges = edges, size = states_count)
    transition_matrix = getTransitionMatrix(neighbours_list = neighbours_list, size = states_count)
    #initial_f = [0.025, 0.025, 0.025, 0.025, 0.025, ...] where length(initial_f) = 40
    # Since the initial probability that the croc is at any state is equal 1/n where n is the number of possible water holes, in this case n = 40
    previous_forward = matrix(1/states_count, ncol = states_count, nrow=1)
  }
  else{
    neighbours_list = moveInfo$mem$neighbours_list
    transition_matrix = moveInfo$mem$transition_matrix
    previous_forward = moveInfo$mem$forward_vector
  }
  # returns a matrix of probabilities of size 40x3
  breakdown_probabilities = getProbabilities(croc_readings = readings, sensors_prob = probs)
  # create a vector of size 40x1 of probabilities
  probabilities = t(rowSums(breakdown_probabilities))
  probabilities = t(breakdown_probabilities[,1] * breakdown_probabilities[,2] * breakdown_probabilities[,3])
  forward_vector = forwardalg(neighbours_list=neighbours_list, previous_forward=previous_forward, emission=probabilities, transition_matrix=transition_matrix)
  #The state with the maximum probability to hold the croc
  state_max_probability = which.max(forward_vector)
  #That state's probability
  max_probability = forward_vector[state_max_probability]
  player_position = positions[3]
  bp1_position=positions[1]
  bp2_position=positions[2]
  # If the crocodile just killed BackPacker1 then just find the shorest path towards the BackPacker1
  if(!is.na(bp1_position) && bp1_position<0){
    goal = abs(bp1_position)
  }
  # If the crocodile just killed BackPacker2 then just find the shorest path towards the BackPacker2
  else if(!is.na(bp2_position) && bp2_position<0){
    goal = abs(bp2_position)
  }
  #else Find shortest_path based between player position and most probable state.
  else{
    goal = state_max_probability
  }
  path = shortest_path(edges, start=player_position, goal=goal)
  next_steps = choose_next_step(path=path,player_position=player_position, state_max_probability, max_probability=max_probability)
  moveInfo$moves=next_steps
  moveInfo$mem=list(forward_vector=forward_vector,neighbours_list=neighbours_list,transition_matrix=transition_matrix)
  return(moveInfo)
}
#' Run Where's Croc
#' 
#' Runs the Where's Croc game. In this game, you are a ranger in an Australian national park. 
#' This park consists of a number of waterholes, some of which are connected to each other.
#' There is a crocodile in the park called 'Croc'. Croc has been fitted with sensors that record 
#' the salinity, phosphate and nitrogen levels in the water where he is swimming. He was also 
#' fitted with a sensor that records his position, but that has broken.
#' Your task is to find Croc using the available information. To aid in this you have information
#' about the probability distributions for different salinity, phosphate and nitrogen levels in 
#' different waterholes.
#' There are also two tourists in the park. Both the tourists and Croc walk randomly, each turn
#' moving to one of the neighboring waterholes from where they are or staying still. All moves
#' are equally likely.
#' If Croc and a tourist end up on the same waterhole, Croc will eat the tourist. If you search
#' the waterhole you are on when Croc is there, you have found Croc and win the game. 
#' Your score is the number of turns it takes to find Croc.
#' To play manually pass manualWC
#' as the makeMoves function an`d enter the appropriate numbers to make moves.
#' @param makeMoves Your function that takes five arguments: (1) A list of information for the move.
#' This has two fiels. The first is a vector of numbers called 'moves', where you will enter 
#' the moves you want to make. You should
#' enter two moves (so you can move to a neighboring waterhole and search). Valid moves are the 
#' numbers of a neighboring or current waterhole or '0' which means you will search your current
#' waterhole for Croc. The second field is a list called
#' 'mem' that you can use to store information you want to remember from turn to turn. (2) A 
#' vector giving the salinity, phosphate and nitrogen reading from Croc sensors at his current 
#' location. (3) A vector giving the positions of the two tourists and yourself. If a tourist
#' has just been eaten by Croc that turn, the position will be multiplied by -1. If a tourist 
#' was eaten by Croc in a previous turn, then the position will be NA. (4) a matrix giving the 
#' edges paths between waterholes (edges) present. (5) a list of three matrices giving the mean
#' and standard deviation of readings for salinity, phosphate and nitrogen respectively
#' at each waterhole.
#' Your function should return the first argument passed with an updated moves vector 
#' and any changes to the 'mem' field you wish to access later on.
#' @param showCroc A Boolean value specifying whether you want Croc to be shown on the gameboard.
#' Note that you are not permitted to use this visual information when you are scored.
#' @param pause The pause period between moves. Ignore this.
#' @return A string describing the outcome of the game.
#' @export
runWheresCroc=function(makeMoves=ourAlgorithm,showCroc=F,pause=1) {
  #sets initial positions in the grid randomly for our 4 points
  positions=sample(1:40,4) # Croc, BP1, BP2, Player
  #gets the blue dots coordinates (fixed)
  points=getPoints()
  #gets the edges between the blue points (fixed)
  edges=getEdges()
  #gets the probability (mean and standard deviation) for the sensors records (salinity, phosphate and nitrogen)
  probs=getProbs()
  # Initialize the game moves count to 0
  move=0
  moveInfo=list(moves=c(),mem=list())
  # while the croc isn't found
  while (!is.na(positions[1])) {
    move=move+1
    # moves the croc after each step to a neighbouring point
    positions[1]=sample(getOptions(positions[1],edges),1)
    # If BP1 is not dead and wasn't eaten in the previous step then the BP1 is moved to a neighbouring point
    # Else if the BP1 was just eaten in the previous step (-1) then change his status to NA
    if (!is.na(positions[2])&&positions[2]>0) {
      positions[2]=sample(getOptions(positions[2],edges),1)
    } else if (!is.na(positions[2]) && positions[2]<0) {
      positions[2]=NA
    }
    # the same thing happens with BP2 as BP1
    if (!is.na(positions[3])&&positions[3]>0) {
      positions[3]=sample(getOptions(positions[3],edges),1)
    } else if (!is.na(positions[3]) && positions[3]<0) {
      positions[3]=NA
    }
    # If BP1 is alive but is in the same waterhole as the croc then BP1 is dead (-position)
    if (!is.na(positions[2]) && positions[2]==positions[1]) {
      positions[2]=-positions[2]
    }
    # the same thing happens with BP2 as BP1
    if (!is.na(positions[3]) && positions[3]==positions[1]) {
      positions[3]=-positions[3]
    }
    # plots the game
    plotGameboard(points,edges,move,positions,showCroc)
    
    Sys.sleep(pause)
    
    readings=getReadings(positions[1],probs)
    #oural(moveInfo = moveInfo, readings = readings, positions = positions[2:4], edges = edges, probs = probs)
    
    # calling the algorithm or manual implementation of the game
    moveInfo=makeMoves(moveInfo,readings,positions[2:4],edges,probs)
    
    if (length(moveInfo$moves)!=2) {
      stop("Error! Passed makeMoves function should return a vector of two elements.")
    }
    # implementing the moves returned from the function call
    for (m in moveInfo$moves) {
      #if move = 0 then it means search the current waterhole of the player
      if (m==0) {
        # IF the current hole is the position of the croc it ends the game
        # and returns the number of moves required to find the croc
        # else it makes sure that the move is a valid move and then move the player to a neighbouring blue point
        if (positions[1]==positions[4]) {
          print(paste("Congratualations! You got croc at move ",move,".",sep=""))
          return (move)
        }
      } else {
        if (m%in%getOptions(positions[4],edges)) {
          positions[4]=m
        } else {
          warning("Invalid move.")
        }
      }      
    }
  }
}
#' @export
getPoints=function() {
  points=matrix(c(1,1),ncol=2)
  points=rbind(points,c(1,7))
  points=rbind(points,c(1,17))
  points=rbind(points,c(2,3))
  points=rbind(points,c(2,12))
  points=rbind(points,c(3,2))
  points=rbind(points,c(3,19))
  points=rbind(points,c(4,7))
  points=rbind(points,c(4,11))
  points=rbind(points,c(5,5))
  points=rbind(points,c(5,15))
  points=rbind(points,c(6,1))
  points=rbind(points,c(6,20))
  points=rbind(points,c(7,6))
  points=rbind(points,c(7,11))
  points=rbind(points,c(8,2))
  points=rbind(points,c(8,14))
  points=rbind(points,c(8,18))
  points=rbind(points,c(9,6))
  points=rbind(points,c(10,10))
  points=rbind(points,c(10,18))
  points=rbind(points,c(11,1))
  points=rbind(points,c(11,12))
  points=rbind(points,c(12,6))
  points=rbind(points,c(12,12))
  points=rbind(points,c(13,16))
  points=rbind(points,c(14,4))
  points=rbind(points,c(14,12))
  points=rbind(points,c(14,20))
  points=rbind(points,c(15,3))
  points=rbind(points,c(15,8))
  points=rbind(points,c(15,17))
  points=rbind(points,c(16,14))
  points=rbind(points,c(17,3))
  points=rbind(points,c(17,18))
  points=rbind(points,c(18,10))
  points=rbind(points,c(19,13))
  points=rbind(points,c(20,2))
  points=rbind(points,c(20,6))
  points=rbind(points,c(20,19))
  return (points)
}

#' @export
getEdges=function() {
  edges=matrix(c(1,2),ncol=2)
  edges=rbind(edges,c(1,4))
  edges=rbind(edges,c(1,6))
  edges=rbind(edges,c(2,4))
  edges=rbind(edges,c(2,5))
  edges=rbind(edges,c(3,5))
  edges=rbind(edges,c(3,7))
  edges=rbind(edges,c(4,6))
  edges=rbind(edges,c(4,8))
  edges=rbind(edges,c(5,7))
  edges=rbind(edges,c(5,9))
  edges=rbind(edges,c(6,12))
  edges=rbind(edges,c(7,11))
  edges=rbind(edges,c(7,13))
  edges=rbind(edges,c(8,9))
  edges=rbind(edges,c(8,10))
  edges=rbind(edges,c(9,11))
  edges=rbind(edges,c(10,12))
  edges=rbind(edges,c(10,14))
  edges=rbind(edges,c(11,13))
  edges=rbind(edges,c(11,15))
  edges=rbind(edges,c(12,16))
  edges=rbind(edges,c(13,18))
  edges=rbind(edges,c(14,15))
  edges=rbind(edges,c(14,16))
  edges=rbind(edges,c(15,17))
  edges=rbind(edges,c(16,19))
  edges=rbind(edges,c(16,22))
  edges=rbind(edges,c(17,18))
  edges=rbind(edges,c(17,19))
  edges=rbind(edges,c(17,20))
  edges=rbind(edges,c(18,21))
  edges=rbind(edges,c(19,20))
  edges=rbind(edges,c(19,22))
  edges=rbind(edges,c(20,23))
  edges=rbind(edges,c(21,23))
  edges=rbind(edges,c(21,29))
  edges=rbind(edges,c(22,24))
  edges=rbind(edges,c(22,27))
  edges=rbind(edges,c(23,24))
  edges=rbind(edges,c(23,25))
  edges=rbind(edges,c(24,25))
  edges=rbind(edges,c(24,27))
  edges=rbind(edges,c(25,26))
  edges=rbind(edges,c(25,27))
  edges=rbind(edges,c(25,28))
  edges=rbind(edges,c(26,28))
  edges=rbind(edges,c(26,29))
  edges=rbind(edges,c(27,30))
  edges=rbind(edges,c(27,31))
  edges=rbind(edges,c(28,31))
  edges=rbind(edges,c(28,32))
  edges=rbind(edges,c(29,32))
  edges=rbind(edges,c(29,35))
  edges=rbind(edges,c(30,31))
  edges=rbind(edges,c(30,34))
  edges=rbind(edges,c(31,33))
  edges=rbind(edges,c(31,34))
  edges=rbind(edges,c(32,33))
  edges=rbind(edges,c(32,35))
  edges=rbind(edges,c(33,35))
  edges=rbind(edges,c(33,36))
  edges=rbind(edges,c(33,37))
  edges=rbind(edges,c(34,36))
  edges=rbind(edges,c(34,38))
  edges=rbind(edges,c(35,40))
  edges=rbind(edges,c(36,37))
  edges=rbind(edges,c(36,39))
  edges=rbind(edges,c(37,39))
  edges=rbind(edges,c(37,40))
  edges=rbind(edges,c(38,39))
  
  return (edges)
}

#' @export
getProbs=function(){
  salinity=cbind(runif(40,100,200),runif(40,5,30))
  phosphate=cbind(runif(40,100,200),runif(40,5,30))
  nitrogen=cbind(runif(40,100,200),runif(40,5,30))
  list(salinity=salinity,phosphate=phosphate,nitrogen=nitrogen)
}

#' @export
getReadings=function(point,probs){
  c(
    rnorm(1,probs$salinity[as.numeric(point),1],probs$salinity[as.numeric(point),2]),
    rnorm(1,probs$phosphate[as.numeric(point),1],probs$phosphate[as.numeric(point),2]),
    rnorm(1,probs$nitrogen[as.numeric(point),1],probs$nitrogen[as.numeric(point),2])
  )
}


#' @export
plotGameboard=function(points,edges,move,positions,showCroc) {
  plot(points,pch=18,col="blue",cex=2,xlab="X",ylab="Y",main=paste("Where's Croc - Move",move))
  xFrom=points[edges[,1],1]
  yFrom=points[edges[,1],2]
  xTo=points[edges[,2],1]
  yTo=points[edges[,2],2]
  segments(xFrom,yFrom,xTo,yTo)
  for (bp in 2:3)
    if (!is.na(positions[bp])) {
      if (positions[bp]>0) {
        points(points[as.numeric(positions[bp]),1],points[as.numeric(positions[bp]),2],col="orange",pch=17,cex=4)
      } else {
        points(points[-as.numeric(positions[bp]),1],points[-as.numeric(positions[bp]),2],col="red",pch=17,cex=4)
      }
    }
  points(points[as.numeric(positions[4]),1],points[as.numeric(positions[4]),2],col="green",pch=15,cex=4)
  if (showCroc) {
    points(points[as.numeric(positions[1]),1],points[as.numeric(positions[1]),2],col="red",pch=15,cex=4)      
  }
  text(points[,1]+.4, points[,2], labels=as.character(1:40))
}

#' @export
getOptions=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}