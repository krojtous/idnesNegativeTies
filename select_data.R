#select_data.R

YEAR = 2015
for(MONTH in c(1:12)){
  #STATUS INFO
  cat(paste0(MONTH,", ", YEAR,"\n"))
  
  #LOAD RAW DATA
  relations = read.csv (paste0("./../data/",YEAR,"/relations_",YEAR,"_", MONTH,".csv"), encoding="UTF-8")
  comments  = read.csv (paste0("./../data/",YEAR,"/comments_",YEAR,"_", MONTH,".csv"), encoding = "utf8")
  articles  = read.csv (paste0("./../data/",YEAR,"/articles_",YEAR,"_", MONTH,".csv"), encoding="UTF-8")
  
  #SELECT REFUGGE TAGS (Příliv uprchlíků do Evropy, Uprchlíci)
  articles_id = unique(as.vector(articles[articles$tag %in% c("Příliv uprchlíků do Evropy", "Uprchlíci"),"article_id"]))
  
  #SELECT DATA
  relations = relations[which(relations$article_id %in% articles_id),]
  comments = comments[which(comments$article_id %in% articles_id),]
  articles = articles[which(articles$article_id  %in% articles_id),]
  
  #EXPORT DATA
  write.csv2(relations, file = paste0("./data/refugees_relations_",YEAR,"_",MONTH,".csv"))
  write.csv2(comments, file = paste0("./data/refugees_comments_",YEAR,"_",MONTH,".csv"))
  write.csv2(articles, file = paste0("./data/refugees_articles_",YEAR,"_",MONTH,".csv"))
}