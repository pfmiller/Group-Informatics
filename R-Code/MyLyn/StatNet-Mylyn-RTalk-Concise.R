rm(list=ls(all=TRUE))

library(network)
library(statnet)  
library(MASS)

####################################################################
# Read the Data in
####################################################################
files <- system("ls data/release*TNET*",intern=T)

for (i in 1:length(files)) {
	#this function is in the MASS package
	fileNamer = as.character(i)
	#Import the files
	el <- read.table(files[i], header=TRUE, dec=".", fill=TRUE)
	#netName = "n"+as.character(i)
	print(el)
	n=network(el,matrix.type="edgelist")
	print(n)
	print(i)

	filename=paste("output/testA/","1graphOne",i,".pdf")
	pdf(filename)
	gplot(n, displayisolates=FALSE,edge.len=(1/n$mel.dist),label=network.vertex.names(n), boxed.labels=TRUE, 	label.pad=0.01)
	dev.off()

	## Show graph of centrality measures
	degree(n)
	ideg <- degree(n, cmode="indegree")
	odeg <- degree(n, cmode="outdegree")
	filename=paste("output/testA/","2graphTwo-MidPlots",i,".pdf")
	pdf(filename)
	plot(ideg, odeg, type="n", xlab="Bug/Task Opener", ylab="Bug/Task Commenter")
	abline(0,1, lty=3)
	text(jitter(ideg), jitter(odeg), network.vertex.names(n), cex=0.75, col=2)
	dev.off()

	## Simple histograms of degree distribution
	par(mfrow=c(2,2))
	filename=paste("output/testA/","3graphThree-histogram",i,".pdf")
	pdf(filename)
	hist(ideg, xlab="Indegree", main="Indegree Distribution", prob=TRUE)
	hist(odeg, xlab="Outdegree", main="Outdegree Distribution", prob=TRUE)
	hist(odeg+ideg, xlab="Total degree", main="Total degree Distribution", prob=TRUE)
	dev.off()
	par(mfrow=c(1,1))

	## Use centrality scores to size and color the network plot
	filename=paste("output/testA/","4graphFourCentralityColorized",i,".pdf")
	pdf(filename)
	gplot(n,vertex.cex=(ideg+odeg)^0.5/2, vertex.sides=50, label.cex=0.8, vertex.col=rgb(odeg/max(odeg), 0, ideg/max	(ideg)), label=network.vertex.names(n), displayisolates=FALSE,  boxed.labels=TRUE)
	dev.off()

	## Show network diagram with betweenness centrality as the key sizing dimension
	bet <- betweenness(n,gmode="graph")
	filename=paste("output/testA/","5graphFiveBetweennessPlot",i,".pdf")
	pdf(filename)
	gplot(n, vertex.cex=sqrt(bet)/12, gmode="graph", label.cex=0.8, label=network.vertex.names(n), 	displayisolates=FALSE,  boxed.labels=TRUE)
	dev.off()
	
	## Plot the largest weak component
	cl <- component.largest(n, connected="weak")        # Who's in the largest component? 
	cl 
	filename=paste("output/testA/","6graphSixLargest-Weak-Component",i,".pdf")
	pdf(filename)
	gplot(n[cl,cl], boxed.lab=TRUE, label.cex=0.5, label.col=4, label=network.vertex.names(n)[cl], displayisolates=FALSE) # Plot the largest 		weak component
	dev.off()
	
	cl <- component.largest(n, connected="strong")        # Who's in the largest component? 
	cl 
	filename=paste("output/testA/","7graphSevenLargest-Strong-Component",i,".pdf")
	pdf(filename)
	gplot(n[cl,cl], boxed.lab=TRUE, label.cex=0.5, label.col=4, label=network.vertex.names(n)[cl], displayisolates=FALSE) # Plot the largest 		weak component
	dev.off()
}


