buildFamilyNetwork_Fam <-
function(data, lastName, middleName, area=NULL, graph=TRUE){

	# Packages/Functions
	require(data.table)
	require(igraph)

	# Convert to data.table
	if(!"data.table" %in% class(data)){
		data <- as.data.table(data)
	}

	# Split by area, if requested
	if(!is.null(area)){
		# Identify Unique Families
		verts <- rbind(
			data[, .(nMember=.N), by=.(name=get(lastName), area=get(area))],
			data[, .(nMember=.N), by=.(name=get(middleName), area=get(area))])
		# Sum by family
		verts <- verts[, .(nMember=sum(nMember, na.rm=TRUE)), by=.(name, area)]

		# Add family ID
		setkey(verts, area, name)
		verts[,ID:=1:.N]

		setcolorder(verts, c("ID","name","area","nMember"))
		

		# Aggregate by last - middle dyads
		p <- data[, .(nTies=.N), by=.(ego=get(lastName), alter=get(middleName), area=get(area))]
		

		# Replace Names with IDs in edge list
			# Ego
			setnames(p, "ego", "name")
			setkey(p, area, name)
			p <- p[verts]
			p[,nMember:=NULL]
			setnames(p, "name", "ego")
			# Alter
			setnames(p, "alter", "name")
			setkey(p, area, name)
			p <- p[verts]
			p[,nMember:=NULL]
			setnames(p, "name", "alter")

			setcolorder(p, c("ID", "i.ID", "area", "nTies", "ego", "alter"))
			p[,c("alter", "ego"):=NULL]
				p <- p[!is.na(ID)]
				p <- p[!is.na(i.ID)]
				setkey(p, ID, i.ID)

		# Construct the graph
		g <- graph_from_data_frame(as.matrix(p), directed=FALSE, vertices=verts)

		# Add edge weights
		E(g)$weight <- p[,nTies]

		if(graph==TRUE){
			return(g)
		}
		if(graph==FALSE){
			return(get.data.frame(g, what="edges"))
		}

	}
	if(is.null(area)){
		# Identify Unique Families
		verts <- rbind(
			data[, .(nMember=.N), by=.(name=get(lastName))],
			data[, .(nMember=.N), by=.(name=get(middleName))])
		# Sum by family
		verts <- verts[, .(nMember=sum(nMember, na.rm=TRUE)), by=.(name)]

		# Add family ID
		setkey(verts, name)
		verts[,ID:=1:.N]

		setcolorder(verts, c("ID","name","nMember"))
		

		# Aggregate by last - middle dyads
		p <- data[, .(nTies=.N), by=.(ego=get(lastName), alter=get(middleName))]
		

		# Replace Names with IDs in edge list
			# Ego
			setnames(p, "ego", "name")
			setkey(p, name)
			p <- p[verts]
			p[,nMember:=NULL]
			setnames(p, "name", "ego")
			# Alter
			setnames(p, "alter", "name")
			setkey(p, name)
			p <- p[verts]
			p[,nMember:=NULL]
			setnames(p, "name", "alter")

			setcolorder(p, c("ID", "i.ID", "nTies", "ego", "alter"))
			p[,c("alter", "ego"):=NULL]
				p <- p[!is.na(ID)]
				p <- p[!is.na(i.ID)]
				setkey(p, ID, i.ID)

		# Construct the graph
		g <- graph_from_data_frame(as.matrix(p), directed=FALSE, vertices=verts)

		# Add edge weights
		E(g)$weight <- p[,nTies]

		if(graph==TRUE){
			return(g)
		}
		if(graph==FALSE){
			return(get.data.frame(g, what="edges"))
		}
	}
}
