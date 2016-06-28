rm(list=ls(all=TRUE))
library(igraph)
library(ggplot2)
igraph.options(print.vertex.attributes = TRUE)
igraph.options(print.edge.attributes = TRUE)

setwd("~/GitHub/Group-Informatics/R-Code/MyLyn")

### This is data about discussions
files <- system("ls data/release*TNET*",intern=T)

for (i in 1:length(files)) {

	fileNamer = as.character(i)
	#Import the files
	el <- read.table(files[i], header=TRUE, dec=".", fill=TRUE)

	disAll <-graph.data.frame(el)

	V(disAll)$label <- V(disAll)$name
	#layout.fruchterman.reingold
	layout.kamada.kawai(disAll, weights=E(disAll)$weight)
	E(disAll)$width <- E(disAll)$weight/25
	E(disAll)$arrow.size <-0.4
	layout.spring(disAll,weights=E(disAll)$weight)
	V(disAll)$size <-(3+sqrt(sqrt(betweenness(disAll, v=V(disAll)))))
	g<-disAll

	if (is.connected(g)){
		com <- spinglass.community(g, spins=8)
		V(g)$color <- com$membership+1
	}

	g <- set.graph.attribute(g, "layout", layout.kamada.kawai(g))

	#output the file
	filename=paste("output/testA/","community",i,"b.pdf")
	pdf(filename)
	#plot.igraph(g)
	plot(g, vertex.label.dist=.5, vertex.label.cex=.7, vertex.label.color="black",
	vertex.frame.color="white")
	dev.off()
}
