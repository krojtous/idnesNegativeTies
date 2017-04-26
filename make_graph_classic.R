#make_graph_classic.R
#Script makes data matrix for analysis with negative ties

#LOAD PACKAGES AND FUNCTIONS
require(igraph)
source("./functions/transformData.R")


#BASIC SETTINGS
YEAR = 2015
stats = data.frame()
TO_DIVIDE = 8


for(MONTH in c(1:12)){
  #STATUS INFO
  cat(paste0(MONTH,", ", YEAR,"\n"))
  
  #LOAD DATA
  relations = read.csv (paste0("./data/refugees_relations_",YEAR,"_", MONTH,".csv"), sep = ";")
  comments  = read.csv (paste0("./data/refugees_comments_",YEAR,"_", MONTH,".csv"), sep = ";")
  articles  = read.csv (paste0("./data/refugees_articles_",YEAR,"_", MONTH,".csv"), sep = ";")
  
  #TRANSFORM RELATIONS TO WEIGHTED SIGNED TIES
  ties = weightRelations(relations)
  
  #TRANSFORM POSITIVE TIES
  ties_P = ties[ties[,3] > 0,]
  ties_P[,3] = floor((ties_P[,3]/TO_DIVIDE))
  ties_P = ties_P[which(ties_P[,3] != 0),]
  
  #TRANSFORM NEGATIVE TIES
  ties_N = ties[ties[,3] < 0,] 
  ties_N[,3] = ceiling((ties_N[,3]/TO_DIVIDE))
  ties_N = ties_N[which(ties_N[,3] != 0),]
  
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
    length(unique(articles$article_id)),
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

#NAME STATS
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

write.csv2(stats, file = "data_stats.csv")



# #BASIC ANALYSIS
# in.deg = degree(graph,v=V(graph), mode="in")+1
# plot(graph,  vertex.label=NA, vertex.size=in.deg, edge.color = "black",edge.width=abs(E(graph)$weight/2),edge.arrow.size = 0, 
#      mark.groups = NULL)
# 
# plot(graph,  vertex.label=NA, vertex.size=log(in.deg)*2, edge.color = "black",edge.width=abs(E(graph)$weight/2),edge.arrow.size = 0, 
#      mark.groups = NULL)
# 


