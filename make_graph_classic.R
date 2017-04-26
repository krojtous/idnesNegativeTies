#make_graph_classic.R
#Script makes data matrix for analysis with negative ties

YEAR = 2015
MONTH = 10

stats = data.frame()



makeSignedGraph = function(YEAR, MONTH){
  TO_DIVIDE = 8
  require(igraph)
  source("./functions/transformData.R")
  
  

  #LOAD DATA
  relations = read.csv (paste0("./../data/",YEAR,"/relations_",YEAR,"_", MONTH,".csv"), encoding="UTF-8")
  comments  = read.csv (paste0("./../data/",YEAR,"/comments_",YEAR,"_", MONTH,".csv"), encoding = "utf8")
  articles  = read.csv (paste0("./../data/",YEAR,"/articles_",YEAR,"_", MONTH,".csv"), encoding="UTF-8")
  
  
  #SELECT REFUGGE TAGS
  # Příliv uprchlíků do Evropy
  # Uprchlíci
  articles_id = unique(as.vector(articles[articles$tag %in% c("Příliv uprchlíků do Evropy", "Uprchlíci"),"article_id"]))
  
  relations = relations[which(relations$article_id %in% articles_id),]
  comments = comments[which(comments$article_id %in% articles_id),]
  
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

  #ADD STATS
  stats_month = c(
    MONTH,
    YEAR,
    TO_DIVIDE,
    length(articles_id),
    nrow(comments),
    nrow(relations),
    nrow(ties),
    nrow(ties_N),
    nrow(ties_P),
    nrow(ties_FIN),
    length(V(graph)),
    length(E(graph))
    )
  stats = rbind(stats,stats_month)
  
}

for(month in c(3:10)){
  makeSignedGraph(2015,month)
}

colnames(stats) = c(
  "MONTH",
  "YEAR",
  "TO_DIVIDE",
  "articles",
  "comments",
  "relations",
  "raw_ties",
  "negative_ties",
  "positive_ties",
  "all_ties",
  "nodes",
  "edges")

#BASIC ANALYSIS
in.deg = degree(graph,v=V(graph), mode="in")+1
plot(graph,  vertex.label=NA, vertex.size=in.deg, edge.color = "black",edge.width=abs(E(graph)$weight/2),edge.arrow.size = 0, 
     mark.groups = NULL)

plot(graph,  vertex.label=NA, vertex.size=log(in.deg)*2, edge.color = "black",edge.width=abs(E(graph)$weight/2),edge.arrow.size = 0, 
     mark.groups = NULL)



