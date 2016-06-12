buildFamilyNetwork_Ind <-
function(data, ID=NULL, lastName, middleName, area=NULL, graph=TRUE, metadata=TRUE){


	# Libraries and Functions
		library(data.table)
		library(igraph)
		trim <- function (x) gsub("^\\s+|\\s+$", "", x)
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

	# ----- Prep ----- #

	# Data as Data.frame
	data <- as.data.frame(data)

	# Convert Class
	if(!is.null(area)){
		data[,paste0(area)] <- as.character(data[,paste0(area)])
	}

	# If ID==NULL, then generate it
	if(is.null(ID)){
		data$ID <- 1:nrow(data)
		ID <- "ID"
	}

	# Split by area, if requested
	if(!is.null(area)){
		s <- split(data, f=data[, paste0(area)])
	} else{
		s <- list(data)
	}

	# ----- Last-Last, Last-Middle, Middle-Last ----- #
	# Identify Unique Last Names in Each Area
	lastNamesl <- lapply(s, function(x){unique(x[,paste0(lastName)])})
	  # Remove NAs
	  lastNamesl <- lapply(lastNamesl, function(x){x[!is.na(x)]})

	# Identify Unique Middle Names in Each Area
	middleNamesl <- lapply(s, function(x){unique(x[,paste0(middleName)])})
	  # Remove NAs
	  middleNamesl <- lapply(middleNamesl, function(x){x[!is.na(x)]})


	# Placeholder for Last-Last, Last-Middle and Middle-Last
	edges <- vector("list", length(s))
		for(i in 1:length(s)){
			edges[[i]] <- vector("list", length(lastNamesl[[i]]))
		}

	i = 1
	j = 1
	# Loop over areas
	for(i in 1:length(s)){
	# Loop over lastNames, construct last-last, last-middle and middle-last ties
	for(j in 1:length(lastNamesl[[i]])){
		# Placeholders
		tempName <- NA

	# 	# Identify the Name of Interest
		tempName <- lastNamesl[[i]][j]

	# 	# Generate all combinations
			edges[[i]][[j]] <- data.frame(combi(
				c(
					s[[i]][,ID][which(s[[i]][,lastName]==tempName)],
					s[[i]][,ID][which(s[[i]][,middleName]==tempName)])))
		}
	}


	# ----- Middle-Middle ----- #
	edges2 <- vector("list", length(s))
	for(i in 1:length(s)){
		edges2[[i]] <- vector("list", length(middleNamesl[[i]]))
	}

	i = 1
	j = 1
	# Loop over areas
	for(i in 1:length(s)){
	# Loop over middleNames, construct middle-middle ties
		for(j in 1:length(middleNamesl[[i]])){
			# Generate all combinations
			edges2[[i]][[j]] <- data.frame(combi(s[[i]][,paste0(ID)][which(s[[i]][,paste0(middleName)]==middleNamesl[[i]][j])]))
		}
	}

	# Combine the area edge lists
	edges <- lapply(edges, function(x){do.call("rbind", x)})
	edges <- do.call("rbind", edges)

	edges2 <- lapply(edges2, function(x){do.call("rbind", x)})
	edges2 <- do.call("rbind", edges2)

	EL <- base::rbind(edges, edges2)

	# Remove NA rows from Edges
	EL <- EL[complete.cases(EL),]

	# Drop Non-unique Rows
	EL <- EL[!duplicated(EL),]

	# Remove Duplicated Rows
	EL <- unique(EL)

	# Sort Edgelist by ego ID
	EL <- EL[order(EL[,1]),]

	data <- data[,c(which(colnames(data)==ID), which(colnames(data)!=paste0(ID)))]

	# If Igraph object requested
	if(graph==TRUE & metadata==TRUE){
		g <- graph_from_data_frame(d=EL, directed=FALSE, vertices=data)
		return(g)
	}
	if(graph==TRUE & metadata==FALSE){
		g <- graph_from_data_frame(d=EL, directed=FALSE, vertices=NULL)
		return(g)
	}

	# If Edge list only requested
	if(graph==FALSE & metadata==TRUE){
		g <- graph_from_data_frame(d=EL, directed=FALSE, vertices=data)
		return(get.data.frame(g, what="both"))
	}
	if(graph==FALSE & metadata==FALSE){
		return(EL)
	}
}
