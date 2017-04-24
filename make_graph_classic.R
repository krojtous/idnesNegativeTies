#make_graph_classic.R
#Script makes data matrix for analysis with negative ties



makeSignedGraph = function(YEAR, MONTH){
  TO_DIVIDE = 7
  require(igraph)
  source("./functions/transformData.R")
  
  
  #LOAD DATA
  relations = read.csv (paste0("./../data/",YEAR,"/relations_",YEAR,"_", MONTH,".csv"), encoding="UTF-8")
  comments  = read.csv (paste0("./../data/",YEAR,"/comments_",YEAR,"_", MONTH,".csv"), encoding = "utf8")
  articles  = read.csv (paste0("./../data/",YEAR,"/articles_",YEAR,"_", MONTH,".csv"), encoding="UTF-8")
  
  
  #SELECT CATEGORY
  relations = relations[which(relations$category == "zahranicni"),]
  
  #TRANSFORM RELATIONS TO WEIGHTED SIGNED TIES
  ties = weightRelations(relations)
  
  #TRANSFORM POSITIVE TIES
  ties_P = ties[ties[,3] > 0,]
  ties_P[,3] = floor((ties_P[,3]/TO_DIVIDE))
  ties_P = ties_P[which(ties_P[,3] != 0),]
  # ties_P[,3] = 1
  
  #TRANSFORM NEGATIVE TIES
  ties_N = ties[ties[,3] < 0,] 
  ties_N[,3] = ceiling((ties_N[,3]/TO_DIVIDE))
  ties_N = ties_N[which(ties_N[,3] != 0),]
  # ties_N[,3] = -1
  
  #MERGE POSITIVE AND NEGATIVE TIES TOGETHER
  ties_FIN = rbind(ties_N, ties_P)
  
  #MAKE GRAPH FOR PACKAGE IGRAPH
  graph = graph.data.frame(as.matrix(ties_FIN), directed=T)
  
  #CHOOSE BIGGEST COMPONENT
  component = components( graph )
  graph     = induced.subgraph( graph, V(graph)[which(component$membership == which.max(component$csize))])
  
  
  #EXPORT DATA
  write.graph(graph, paste0("./graphs/graph_classic_",MONTH,"_",YEAR,".txt"), "ncol")
}


for(month in c(1:8)){
  makeSignedGraph(2015,month)
}


#BASIC ANALYSIS
in.deg = degree(graph,v=V(graph), mode="in")+1
plot(graph,  vertex.label=NA, vertex.size=in.deg, edge.color = "black",edge.width=abs(E(graph)$weight/2),edge.arrow.size = 0, 
     mark.groups = NULL)

plot(graph,  vertex.label=NA, vertex.size=log(in.deg)*2, edge.color = "black",edge.width=abs(E(graph)$weight/2),edge.arrow.size = 0, 
     mark.groups = NULL)

