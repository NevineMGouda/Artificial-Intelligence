?seq.along
dumbDM=function(roads,car,packages){
  car$nextMove=sample(c(2,4,6,8),1)
  return (car)
}
basicDM=function(roads,car,packages) {
  nextMove=0
  toGo=0
  offset=0
  if (car$load==0) {
    toGo=which(packages[,5]==0)[1]
  } else {
    toGo=car$load  
    offset=2
  }
  if (car$x<packages[toGo,1+offset]) {nextMove=6}
  else if (car$x>packages[toGo,1+offset]) {nextMove=4}
  else if (car$y<packages[toGo,2+offset]) {nextMove=8}
  else if (car$y>packages[toGo,2+offset]) {nextMove=2}
  else {nextMove=5}
  car$nextMove=nextMove
  car$mem=list()
  return (car)
}
manualDM=function(roads,car,packages) {
  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
  }  
  car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  return (car)
}

ourAlgorithm=function(roads,car,packages) {
  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
  } 
  shortestPath = list()
  if (car$load == 0) {
    index = 1
    shortestPathLength = Inf
    # Loop on every package and
    for(i in 1:5) {
      if (packages[i,5] == 0) {
        result1 = shortestPath(roads, car$x, car$y, packages[i,1], packages[i,2])
        result2 = shortestPath(roads, packages[i,1], packages[i,2], packages[i,3], packages[i,4])
        if(shortestPathLength > result1$costs + result2$costs) {
          shortestPathLength = result1$costs + result2$costs
          index = i
        } 
      }
    }
    shortestPath = shortestPath(roads, car$x, car$y, packages[index,1], packages[index,2])
  } else {
    shortestPath = shortestPath(roads, car$x, car$y, packages[car$load,3], packages[car$load,4])
  }

  if (length(shortestPath$nodes) > 1) {
    if (shortestPath$nodes[2][[1]]$x > car$x) car$nextMove = 6
    if (shortestPath$nodes[2][[1]]$x < car$x) car$nextMove = 4
    if (shortestPath$nodes[2][[1]]$y > car$y) car$nextMove = 8
    if (shortestPath$nodes[2][[1]]$y < car$y) car$nextMove = 2 
  } else {
    car$nextMove = 5
  }
  
  return (car)
}

shortestPath = function(roads, originX, originY, destinationX, destinationY) {
  # A matrix of the grid's size, where each element represents the # of steps between the the destination and all other points
  heuristicMatrix = matrix(0, nrow = dim, ncol = dim)
  for(i in 1:dim) {
    for(j in 1:dim) {
      heuristicMatrix[i, j] = abs(destinationX - i) + abs(destinationY - j)
    }
  }
  frontier = list((list(nodes = list(list(x = originX, y = originY)), costs = 0, lastHeuristic = heuristicMatrix[originX,originY])))
  visited = list()
  result = explore(originX, originY, roads, heuristicMatrix, frontier, visited, destinationX, destinationY)
  return (result)
}

explore = function(originX, originY, roads, heuristic, frontier, visited, destinationX, destinationY) {
  for(i in 1:10000) {
    # check frontier
    searchingPath = frontier[[1]]
    index = 1
    # Setting the priority queue, in other words the path to explore is the path with the least cost
    for (i in 1:length(frontier)) {
      if (searchingPath$costs + searchingPath$lastHeuristic >    
          frontier[[i]]$costs + frontier[[i]]$lastHeuristic) {
        searchingPath = frontier[[i]]
        index = i
      }
    } 
    
    lastNode = lastNode(searchingPath$nodes)
    
    if (lastNode$x == destinationX && lastNode$y == destinationY) {
      return (searchingPath)
    }
    
    frontier[index] = NULL
    visited <- append(visited, list(lastNode))

    # visit neighbors
    #check if the node in the right is in grid and not visited before
    if (lastNode$x < dim && indexInList(lastNode$x + 1, lastNode$y, visited) == 0) {
      #create new searching path by adding the previous nodes, getting new heuristic  and calculate new cost
      newSearchingPath = list(nodes = searchingPath$nodes,
                              lastHeuristic = heuristic[lastNode$x + 1, lastNode$y],
                              costs = searchingPath$costs + roads$hroads[lastNode$y, lastNode$x]) #TODO: check costs
      # Add new node to the nodes
      newSearchingPath$nodes[[length(newSearchingPath$nodes) + 1]] = list(x = lastNode$x + 1, y = lastNode$y)
      frontier = checkFrontier(frontier, newSearchingPath)
    }
    #check if the node in the left is in grid and not visited before
    if (lastNode$x > 1 && indexInList(lastNode$x - 1, lastNode$y, visited) == 0) {
      newSearchingPath = list(nodes = searchingPath$nodes,
                              lastHeuristic = heuristic[lastNode$x - 1, lastNode$y],
                              costs = searchingPath$costs + roads$hroads[lastNode$y, lastNode$x - 1])
      newSearchingPath$nodes[[length(newSearchingPath$nodes) + 1]] = list(x = lastNode$x - 1, y = lastNode$y)
      frontier = checkFrontier(frontier, newSearchingPath)
    }
    #check if the upper node is in grid and not visited before
    if (lastNode$y < dim && indexInList(lastNode$x, lastNode$y + 1, visited) == 0) {
      newSearchingPath = list(nodes = searchingPath$nodes,
                              lastHeuristic = heuristic[lastNode$x, lastNode$y + 1],
                              costs = searchingPath$costs + roads$vroads[lastNode$y, lastNode$x])
      newSearchingPath$nodes[[length(newSearchingPath$nodes) + 1]] = list(x = lastNode$x, y = lastNode$y + 1)
      frontier = checkFrontier(frontier, newSearchingPath)
    }
    #check if the bottom node is in grid and not visited before
    if (lastNode$y > 1 && indexInList(lastNode$x, lastNode$y - 1, visited) == 0) {
      newSearchingPath = list(nodes = searchingPath$nodes,
                              lastHeuristic = heuristic[lastNode$x, lastNode$y - 1],
                              costs = searchingPath$costs + roads$vroads[lastNode$y - 1, lastNode$x])
      newSearchingPath$nodes[[length(newSearchingPath$nodes) + 1]] = list(x = lastNode$x, y = lastNode$y - 1)
      frontier = checkFrontier(frontier, newSearchingPath)
    }
  }
  
  return (NULL)
}

# Check if last node is reached more than one time, if true that store the path of the smallest cost
checkFrontier = function(frontier, searchingPath) {
  lastNode = lastNode(searchingPath$nodes)
  if (length(frontier) > 0) {
    for(i in 1:length(frontier)) {
      if (lastNode(frontier[[i]]$nodes)$x == lastNode$x && lastNode(frontier[[i]]$nodes)$y == lastNode$y) {
        if (frontier[[i]]$costs + frontier[[i]]$lastHeuristic >
            searchingPath$costs + searchingPath$lastHeuristic) {
          frontier[[i]] = searchingPath
        }
        return (frontier)
      }
    }
  }

  frontier <- append(frontier, list(searchingPath))
  return (frontier)
}

lastNode = function(nodes) {
  return (nodes[length(nodes)][[1]])
}

indexInList = function(x, y, nodes) {
  if (length(nodes) == 0) return (0)
  for(i in 1:length(nodes)) {
    if (nodes[[i]]$x == x && nodes[[i]]$y == y) {
      return (i)
    }
  }
  return (0)
}

#' Run Delivery Man
#' 
#' Runs the delivery man game. In this game, deliveries are randomly placed on a city grid. You
#' must pick up and deliver the deliveries as fast as possible under changing traffic conditions.
#' Your score is the time it takes for you to complete this task. To play manually pass manualDM
#' as the carReady function and enter the number pad direction numbers to make moves.
#' @param carReady Your function that takes three arguments: (1) a list of two matrices giving the 
#' traffice conditions. The first matrix is named 'hroads' and gives a matrix of traffice conditions
#' on the horizontal roads. The second matrix is named 'vroads' and gives a matrix of traffic 
#' conditional on the vertical roads. (2) a list providing information about your car. This
#' list includes the x and y coordinates of the car with names 'x' and 'y', the package the car 
#' is carrying, with name 'load' (this is 0 if no package is being carried), a list called
#' 'mem' that you can use to store information you want to remember from turn to turn, and
#' a field called nextMove where you will write what you want the car to do. Moves are 
#' specified as on the number-pad (2 down, 4 left, 6 right, 8 up, 5 stay still). (3) A
#' matrix containing information about the packages. This contains five columns and a row for each
#' package. The first two columns give x and y coordinates about where the package should be picked
#' up from. The next two columns give x and y coordinates about where the package should be 
#' delivered to. The final column specifies the package status (0 is not picked up, 1 is picked up but not delivered, 2 is delivered).
#' Your function should return the car object with the nextMove specified.
#' @param dim The dimension of the board. You will be scored on a board of dimension 10.
#' @param turns The number of turns the game should go for if deliveries are not made. Ignore this 
#' except for noting that the default is 2000 so if you have not made deliveries after 2000 turns
#' you fail.
#' @param doPlot Specifies if you want the game state to be plotted each turn.
#' @param pause The pause period between moves. Ignore this.
#' @param del The number of deliveries. You will be scored on a board with 5 deliveries.
#' @return A string describing the outcome of the game.
#' @export
runDeliveryMan <- function (carReady=ourAlgorithm,dim=10,turns=2000,
                            doPlot=T,pause=0.1,del=5) {
  roads=makeRoadMatrices(dim)
  car=list(x=1,y=1,wait=0,load=0,nextMove=NA,mem=list()) # initialize car
  packages=matrix(sample(1:dim,replace=T,5*del),ncol=5) # initialize packages
  packages[,5]=rep(0,del)
  for (i in 1:turns) {
    roads=updateRoads(roads$hroads,roads$vroads)
    if (doPlot) {
      makeDotGrid(dim,i) 
      plotRoads(roads$hroads,roads$vroads) 
      points(car$x,car$y,pch=16,col="blue",cex=3)  
      plotPackages(packages)      
    }
    if (car$wait==0) {
      if (car$load==0) {
        on=packageOn(car$x,car$y,packages)
        if (on!=0) {
          packages[on,5]=1
          car$load=on
        }
      } else if (packages[car$load,3]==car$x && packages[car$load,4]==car$y) {
        packages[car$load,5]=2
        car$load=0
        if (sum(packages[,5])==2*nrow(packages)) {
          print (paste("Congratulations! You suceeded in",i,"turns!"))
          return (i)
        }
      }      
      car=carReady(roads,car,packages)
      car=processNextMove(car,roads,dim)
    } else {
      car$wait=car$wait-1
    }
    if (pause>0) Sys.sleep(pause)
  }
  print (paste("You failed to complete the task. Try again."))
  return (NA)
}
packageOn<-function(x,y,packages){
  notpickedup=which(packages[,5]==0)
  onX=which(packages[,1]==x)
  onY=which(packages[,2]==y)
  available=intersect(notpickedup,intersect(onX,onY))
  if (length(available)!=0) {
    return (available[1])
  } 
  return (0)
}
processNextMove<-function(car,roads,dim) {
  nextMove=car$nextMove
  if (nextMove==8) {
    if (car$y!=dim) {
      car$wait=roads$vroads[car$y,car$x]
      car$y=car$y+1
    } else {
      warning(paste("Cannot move up from y-position",car$y))
    }
  } else if (nextMove==2) {
    if (car$y!=1) {
      car$y=car$y-1
      car$wait=roads$vroads[car$y,car$x]
    } else {
      warning(paste("Cannot move down from y-position",car$y))
    }
  }  else if (nextMove==4) {
    if (car$x!=1) {
      car$x=car$x-1
      car$wait=roads$hroads[car$y,car$x]
    } else {
      warning(paste("Cannot move left from x-position",car$x))
    }
  }  else if (nextMove==6) {
    if (car$x!=dim) {
      car$wait=roads$hroads[car$y,car$x]
      car$x=car$x+1
    } else {
      warning(paste("Cannot move right from x-position",car$x))
    }
  } else if (nextMove!=5) {
    warning("Invalid move. No move made. Use 5 for deliberate no move.")    
  }
  car$nextMove=NA
  return (car)
} 

plotPackages=function(packages) {
  notpickedup=which(packages[,5]==0) 
  notdelivered=which(packages[,5]!=2)
  points(packages[notpickedup,1],packages[notpickedup,2],col="green",pch=18,cex=3)
  points(packages[notdelivered,3],packages[notdelivered,4],col="red",pch=18,cex=3)
}

makeRoadGrid<-function() {
  
  out=matrix(rep("S",51*51),ncol=51)
  out[26,]=rep("H",51)
  out[,26]=rep("H",51)
}

makeRoadGrid<-function() {
  out=matrix(rep("S",51*51),ncol=51)
  out[26,]=rep("H",51)
  out[,26]=rep("H",51)
}
#' @export
makeDotGrid<-function(n,i) {
  plot(rep(seq(1,n),each=n),rep(seq(1,n),n),xlab="X",ylab="Y",main=paste("Delivery Man. Turn ", i,".",sep=""))
}

#' @export
makeRoadMatrices<-function(n){
  hroads=matrix(rep(1,n*(n-1)),nrow=n)
  vroads=matrix(rep(1,(n-1)*n),nrow=n-1)
  list(hroads=hroads,vroads=vroads)
}

#' @export
plotRoads<- function (hroads,vroads) {
  for (row in 1:nrow(hroads)) {
    for (col in 1:ncol(hroads)) {
      lines(c(col,col+1),c(row,row),col=hroads[row,col])
    }
  }
  for (row in 1:nrow(vroads)) {
    for (col in 1:ncol(vroads)) {
      lines(c(col,col),c(row,row+1),col=vroads[row,col])
    }
  }
}

#' @export
updateRoads<-function(hroads,vroads) {
  r1=runif(length(hroads))
  r2=runif(length(hroads))
  for (i in 1:length(hroads)) {
    h=hroads[i]
    if (h==1) {
      if (r1[i]<.05) {
        hroads[i]=2
      }
    }
    else {
      if (r1[i]<.05) {
        hroads[i]=h-1
      } else if (r1[i]<.1) {
        hroads[i]=h+1
      }
    }
    v=vroads[i]
    if (v==1) {
      if (r2[i]<.05) {
        vroads[i]=2
      }
    }
    else {
      if (r2[i]<.05) {
        vroads[i]=v-1
      } else if (r2[i]<.1) {
        vroads[i]=v+1
      }
    }    
  }
  list (hroads=hroads,vroads=vroads)
}


