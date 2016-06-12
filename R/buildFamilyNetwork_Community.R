buildFamilyNetwork_Community <-
function(data, lastName, middleName, ID, byArea, withinArea, graph=TRUE, directed=TRUE){
  data2 <- data

  # Load Packages/Functions
  library(igraph)
  library(data.table)
  # Combi Function
  combi <- function(vec1){
    si <- length(vec1)
    first <- rep(vec1, (si-1):0)
    secR <- rev(vec1)
    second <- secR[sequence(1:(si-1))]
    second <- rev(second)
    combi <- matrix(cbind(first, second), ncol = 2)
    return(combi)
  }

  # Convert to data2.table
  if(!"data.table" %in% class(data2)){ data2 <- as.data.table(data2)}

  # Rename the Name Issue
  if(lastName=="lastName"){
    data2[,l:=lastName]
    data2[,lastName:=NULL]
    lastName <- "l"
  }
  if(middleName=="middleName"){
    data2[,m:=middleName]
    data2[,middleName:=NULL]
    middleName <- "m"
  }
  if(ID=="ID"){
    setnames(data2, ID, "id")
    ID <- "id"
  }

  # Change Area Column Classes
  if(class(data2[,get(byArea)])!="character"){
    set(data2, j=byArea, v=as.character(data2[[byArea]]))
  }
  if(class(data2[,get(withinArea)])!="character"){
    set(data2, j=withinArea, v=as.character(data2[[withinArea]]))
  }
  if(class(data2[,get(lastName)])!="character"){
    set(data2, j=lastName, v=as.character(data2[[lastName]]))
  }
  if(class(data2[,get(middleName)])!="character"){
    set(data2, j=middleName, v=as.character(data2[[middleName]]))
  }
  if(!class(data2[,get(ID)]) %in% c("character","numeric","integer")){
    set(data2, j=ID, v=as.character(data2[[ID]]))
  }

  # Identify Unique withinArea
  w <- unique(data2[,get(withinArea)])

  # For each within area, identify unique pairs
  pairs <- vector("list", length(w)); names(pairs) <- w
  for(i in 1:length(w)){
    pairs[[i]] <- data2[get(withinArea)==w[i], combi(unique(get(byArea)))]
  }

  for(i in 1:length(pairs)){
    pairs[[i]] <- data.frame(pairs[[i]], to=rep(NA, nrow(pairs[[i]])), from=rep(NA, nrow(pairs[[i]])))
  }

  #   pairIDs <- vector("list", length(pairs))
  #     for(i in 1:length(pairIDs)){
  #       pairIDs[[i]] <- vector("list", length(pairs[[i]]))
  #         }

  for(k in 1:length(pairs)){ # loop over withinArea
    for(i in 1:nrow(pairs[[k]])){ # loop over unique pairs
      for(j in 1:2){ # loop over sender vs receiver
        pairs[[k]][i,2+j] <- length(unique(c(
          data2[get(byArea)==pairs[[k]][i,c(1:2)[j]] & get(lastName) %in% data2[get(byArea)==pairs[[k]][i,rev(c(1:2))[j]] , get(lastName)] , get(ID)],
          data2[get(byArea)==pairs[[k]][i,c(1:2)[j]] & get(lastName) %in% data2[get(byArea)==pairs[[k]][i,rev(c(1:2))[j]] , get(middleName)] , get(ID)],
          data2[get(byArea)==pairs[[k]][i,c(1:2)[j]] & get(middleName) %in% data2[get(byArea)==pairs[[k]][i,rev(c(1:2))[j]] , get(middleName)] , get(ID)])))
      }
    }
  }

  # Join the list elements
  # if(class()=="list")
  out <- rbindlist(pairs)
  out[,':='(nPeople=to+from, to=NULL, from=NULL)]

  # graph is FALSE
  if(graph==FALSE){
    return(out)
  }

  # graph is TRUE
  if(graph==TRUE){
    g <- graph_from_data_frame(out, direct=FALSE)
    names(edge_attr(g))[which(names(edge_attr(g)) == "nPeople")] <- "weight"
    return(g)
  }
}
