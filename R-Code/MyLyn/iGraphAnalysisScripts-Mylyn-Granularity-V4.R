rm(list=ls(all=TRUE))
library(igraph)
library(ggplot2)
igraph.par("print.vertex.attributes", TRUE)
igraph.par("print.edge.attributes", TRUE)

files <- system("ls IntegratingData/granular*",intern=T)

for (i in 1:length(files)) {
	
	fileNamer = as.character(i)
	#Import the files
	el <- read.table(files[i], header=TRUE, dec=".", fill=TRUE)
	#netName = "n"+as.character(i)
	#print(el)

	#el <- read.table("data/release10TNET.csv", header=TRUE)

	disAll <-graph.data.frame(el, directed=FALSE)

	V(disAll)$label <- V(disAll)$name
	layout.fruchterman.reingold(disAll, weights=E(disAll)$weight)
	E(disAll)$width <- E(disAll)$weight
	E(disAll)$arrow.size <-0.4
	layout.spring(disAll,weights=E(disAll)$weight)
	V(disAll)$size <- (3+(betweenness(disAll, v=V(disAll))))
	g<-disAll
	
	if (is.connected(g)){
		com <- spinglass.community(g, spins=8) 
		V(g)$color <- com$membership+1 
	}
	
	#g <- set.graph.attribute(g, "layout", layout.kamada.kawai(g)) 
			
	#output the file	
	filename=paste("output/September1-2010/","granular",i,"-granular-iGraph.pdf")
	pdf(filename)
	#plot.igraph(g)
	plot.igraph(g, vertex.label.dist=.6, vertex.label.cex=1, vertex.label.color="blue", 
	vertex.frame.color="white") 
	dev.off()
			
	#output the file	
	filename=paste("output/September1-2010/","granular",i,"-granular-Reg.pdf")
	pdf(filename)
	plot(g, vertex.label.dist=.6, vertex.label.cex=1, vertex.label.color="red", 
	vertex.frame.color="white") 

	dev.off()
}

