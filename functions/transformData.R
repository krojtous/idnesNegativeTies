#transformData.R


#------------------------transfromData----------------------------------------------
transformData = function ( relations, SETTINGS ){
    
    require(igraph)
    if( file.exists(SETTINGS$GRAPH_PATH)){
        return = read.graph(SETTINGS$GRAPH_PATH, "ncol")
    } else{ 
        relations = weightPositiveRelations( relations )
        #relations = weightRelations        ( relations )
        relations = transfromWeights       ( relations, SETTINGS )
        graph     = transformToGraph       ( relations )
        return    = graph
    }
}


#-------------------------weightedRelations---------------------------------
weightRelations = function(relations){
    #-----Sloučí opakované vztahy do jednoho a přidělí jim váhu podle opakování
    relPos = relations[which(relations$positive_reaction == 1),]     #Select positive rows
    relNeg = relations[which(relations$positive_reaction == 0),]     #Select negative rows
    require(plyr)
    
    relationsWeighted = ddply(relations, .(relPos$commenting_person_id, relPos$reacting_person_id), nrow)
    names(relationsWeighted) = c("target", "source", "weight")
    
    relationsWeightedN = ddply(relations, .(relNeg$commenting_person_id, relNeg$reacting_person_id), nrow)
    names(relationsWeightedN) = c("target", "source", "weight")
    
    relationsWeightedN[,3] = -relationsWeightedN[,3]
    
    relationsWeightedAll = merge(x = relationsWeighted, y = relationsWeightedN, by = c("target","source"), all = TRUE)
    relationsWeightedAll[is.na(relationsWeightedAll)] = 0
    relationsWeightedAll[,3] = relationsWeightedAll[,3] + relationsWeightedAll[,4]
    relationsWeightedAll[,4] <- NULL #drop collumn
    names(relationsWeightedAll)[3] = "weight"
    return = relationsWeightedAll
    
}



#-------------------------weightedPositiveRelations---------------------------------
weightPositiveRelations = function(relations){
    #-----Sloučí opakované vztahy do jednoho a přidělí jim váhu podle opakování
    relations = relations[which(relations$positive_reaction == 1),]     #Select positive rows
    
    require(plyr)
    relationsWeighted = ddply(relations, .(relations$commenting_person_id, relations$reacting_person_id), nrow)
    names(relationsWeighted) = c("target", "source", "weight")
    return = relationsWeighted
}
#-------------------------transformWeights---------------------------------
transfromWeights = function(relationsW, SETTINGS){
    #-----Upraví vztahy (divede by 10 and floor)
    relationsW[,3] = floor((relationsW[,3]/SETTINGS$TO_DIVIDE)) #IMPORTANT HOW MANY WILL BE EDGE WEIGHT DIVIDED!!!!
    #relationsW[which(relationsW[,3] < 0),3] = relationsW[which(relationsW[,3] < 0),3] + 1 #number less than zero are floored and we must add 1
    relationsW = relationsW[which(relationsW[,3] != 0),]
    return = relationsW
}


#------------------------transformToGraph---------------------------------
transformToGraph = function(relations){

    relationsW = as.matrix(relations)
    graph      = graph.data.frame(relationsW, directed=F)
    
    #select only biggest coherent component in the graph
    component = components( graph )
    graph     = induced.subgraph( graph, V(graph)[which(component$membership == which.max(component$csize))])
    return    = graph
}