## Look at the networksis package for this data...
## Quite a gap between methods used in journals and the methods that
## are available for use.  R plays a role 
# install.packages("RODBC")
# install.packages("MASS")
# install.packages("igraph")
# install.packages("statnet")
# install.packages("network")

rm(list=ls(all=TRUE))

library(network)
library(statnet)
library(MASS)

####################################################################
# Read the Data in
####################################################################
# unexploded data set
# files <- system("ls data/release*TNET*",intern=T)

# exploded data set

# All exploded data
# files <- system("ls TimeSeriesAnalysis/Exploded_Raw_Talk_Data/TimeSeriesNetwork*",intern=T)

# subsets based on targeted network subraphs.  this first one is focused on 193, 106, 416, & 1 
files <- system("ls TimeSeriesNetwork_14_Subset2*",intern=T)



for (i in 1:length(files)) {
	# this function is in the MASS package
	fileNamer = as.character(i)
	# Import the files
	el <- read.table(files[i], sep=",", header=TRUE, fill=TRUE, dec=".")
	# netName = "n"+as.character(i)
	print(el)
	n=network(el,matrix.type="edgelist")
	print(n)
	print(i)
	
	degree(n)
	ideg <- degree(n, cmode="indegree")
	odeg <- degree(n, cmode="outdegree")

	# Basic Graph
	filename=paste("output/bz_exploded_subsets/","1graphOne",i,".pdf")
	pdf(filename)
	gplot(n, displayisolates=FALSE,edge.len=(1/n$mel.dist),label=network.vertex.names(n), boxed.labels=TRUE, 	label.pad=0.01, main=filename)
	dev.off()

	# Show graph of centrality measures
	filename=paste("output/bz_exploded_subsets/","2graphTwo-MidPlots",i,".pdf")
	pdf(filename)
	plot(ideg, odeg, type="n", xlab="incoming MIDS", ylab="outgoing MIDS", main=filename)
	abline(0,1, lty=3)
	text(jitter(ideg), jitter(odeg), network.vertex.names(n), cex=0.75, col=2)
	dev.off()

	# Simple histograms of degree distribution
	par(mfrow=c(2,2))
	filename=paste("output/bz_exploded_subsets/","3graphThree-histogram",i,".pdf")
	pdf(filename)
	atitle=paste(filename,i, "Total Degree Distribution")
	hist(ideg, xlab="Indegree", main="Indegree Distribution", prob=TRUE)
	hist(odeg, xlab="Outdegree", main="Outdegree Distribution", prob=TRUE)
	hist(odeg+ideg, xlab="Total degree", main=atitle, prob=TRUE)
	dev.off()
	par(mfrow=c(1,1))

	# Use centrality scores to size and color the network plot
	filename=paste("output/bz_exploded_subsets/","4graphFourCentralityColorized",i,".pdf")
	pdf(filename)
	
	# adjusted vertex for weighted measures.
	# gplot(n,vertex.cex=(ideg+odeg)^0.5/2, vertex.sides=50, label.cex=0.8, vertex.col=rgb(odeg/max(odeg), 0, ideg/max	(ideg)), label=network.vertex.names(n), displayisolates=FALSE,  boxed.labels=TRUE)
		
	# weighted vertex size adjustment
	gplot(n,vertex.cex=(ideg+odeg)^0.5/7, vertex.sides=50, label.cex=0.8, vertex.col=rgb(odeg/max(odeg), 0, ideg/max	(ideg)), label=network.vertex.names(n), displayisolates=FALSE,  boxed.labels=TRUE, pad=5, main=filename)
	
	dev.off()

	# Show network diagram with betweenness centrality as the key sizing dimension
	bet <- betweenness(n,gmode="graph")
	filename=paste("output/bz_exploded_subsets/","5graphFiveBetweennessPlot",i,".pdf")
	pdf(filename)
	gplot(n, vertex.cex=sqrt(bet)/12, gmode="graph", label.cex=0.8, label=network.vertex.names(n), 	displayisolates=FALSE,  boxed.labels=TRUE, main=filename)
	dev.off()
	
	## Plot the largest weak component
	cl <- component.largest(n, connected="weak")        # Who's in the largest component? 
	cl 
	filename=paste("output/bz_exploded_subsets/","6graphSixLargest-Weak-Component",i,".pdf")
	pdf(filename)
	gplot(n[cl,cl], boxed.lab=TRUE, label.cex=0.5, label.col=4, label=network.vertex.names(n)[cl], displayisolates=FALSE, main=filename) # Plot the largest 		weak component
	dev.off()
	
	cl <- component.largest(n, connected="strong")        # Who's in the largest component? 
	cl 
	filename=paste("output/bz_exploded_subsets/","7graphSevenLargest-Strong-Component",i,".pdf")
	pdf(filename)
	gplot(n[cl,cl], boxed.lab=TRUE, label.cex=0.5, label.col=4, label=network.vertex.names(n)[cl], displayisolates=FALSE, main=filename) # Plot the largest 		weak component
	dev.off()
}

	centA <-rbind(0)
	centB <-rbind(0)
	densa <-rbind(0)
	dyadicRec <- rbind(0)
	edgewiseRec <- rbind(0)
	transitivity <- rbind(0)
	transitiveCompletion <-rbind(0)
	
for (i in 1:length(files)) {
	#Import the files
	el <- read.table(files[i], header=TRUE, dec=".", fill=TRUE)
	#netName = "n"+as.character(i)
	n=network(el,matrix.type="edgelist")
	
	## Do MIDS concentrate?
	a<-centralization(n, degree, cmode="indegree")
	## Eigenvector Centralization
	b<-centralization(n, evcent)
	## Basic network level indices
	c<-gden(n) #density
	d<-grecip(n)  # Dyadic reciprocity
	e<-grecip(n, measure="edgewise") # edgewise reciprocity
	f<-gtrans(n)	#transitivity
	g<-log(gtrans(n)/gden(n)) ## transitive completion LRR

	centA <-rbind(centA,a)
	centB <-rbind(centB,b)
	densa <-rbind(densa,c)
	dyadicRec <- rbind(dyadicRec,d)
	edgewiseRec <- rbind(edgewiseRec,e)
	transitivity <- rbind(transitivity,f)
	transitiveCompletion <-rbind(transitiveCompletion,g)
}

	plot(centB, transitivity, type="n", xlab="Centralization", ylab="Transitivity")
	abline(0,1, lty=3)
	text(jitter(centB), jitter(transitivity), round(centB[,], digits=3), cex=0.75, col=2)


	plot(centA, transitivity, type="n", xlab="Density", ylab="Transitivity")
	abline(0,1, lty=3)
	text(jitter(centA), jitter(transitivity), round(centA[,], digits=3), cex=0.75, col=2)


	plot(centA, transitivity, type="n", xlab="Density", ylab="Transitivity")
	abline(0,1, lty=3)
	text(jitter(centA), jitter(transitivity), centA[n], cex=0.75, col=2)

## Looking at Subgraphs
############################################################
These Cause Memory Dumps... Try with a lower MaxLength (6 definitely crashes)
############################################################
dyad.census(n)
triad.census(n)
triad.census(n, mode="graph")
kpath.census(n, maxlen=4, tabulate.by.vertex=FALSE)  #paths
kcycle.census(n, maxlen=4, tabulate.by.vertex=FALSE) #cycles
clique.census(n, tabulate.by.vertex=FALSE, enumerate=FALSE) #find maximal cliques

#Look for an integrated, graphical understanding of subgraphs in the network
kpath.census(n, maxlen=4)
indirect <- kpath.census(n, maxlen=6, dyadic.tabulation="sum")$paths.bydyad
gplot(indirect, label.cex=0.8, vertex.cex=.75, label=network.vertex.names(n), displayisolates=FALSE, boxed.labels=TRUE)  

#################################    WEAK    #################################
components(n)                                       # Strong component count 
components(n, connected="weak")                     # Weak component count 
cd <- component.dist(n, connected="weak")           # Get weak components 
cd 
plot(1:length(cd$cdist), cd$cdist, xlab="Size", ylab="Frequency") # Component sizes 

cl <- component.largest(n, connected="weak")        # Who's in the largest component? 
cl 
gplot(n[cl,cl], boxed.lab=TRUE, label.cex=0.5, label.col=4, 
    label=network.vertex.names(n)[cl])              # Plot the largest weak component

#################################    STRONG    #################################
cd <- component.dist(n, connected="strong")           # Get strong components 
cd 
plot(1:length(cd$cdist), cd$cdist, xlab="Size", ylab="Frequency") # Component sizes 

cl <- component.largest(n, connected="strong")        # Who's in the largest strong component? 
cl 
gplot(n[cl,cl], boxed.lab=TRUE, label.cex=0.5, label.col=4, 
    label=network.vertex.names(n)[cl]) 			## Plot the largest strong component

# a convenience function to extract the "vertex.names" attribute from all vertices. 
# network.vertex.names(n)

# list edge attributes
# get.edge.attribute(n$mel, "dist", unlist=TRUE)
#export the data with attributes 
# m <- as.sociomatrix(n, "dist", method="euclidean")
plot(n, displayisolates = FALSE, vertex.col="color", edge.len=(1/n$mel.dist)) 


####################################################################

#This tells me if the distance on an edge is greater than 3
#test <- n %e% "dist" > 3

#### Two or more connections 
## This is just a sketch that is implemented more fully above.  Keeping it because I think 
## There are a few ideas here that I did not pursue, but might later.

n2 <- network.copy(n)
eList2 <-rbind(1)
for (i in 1:network.edgecount(n2))
{
	print(network.edgecount(n2))
	if (get.edge.value(n2,"dist")[i] < 2)
	{
		eList2 <- rbind(eList2,i)
	}
}
eList2 <- as.vector(eList2)
delete.edges(n2, eList2)
filename=paste("output/SN2/","subgraph","n2",".pdf")
pdf(filename)
plot(n2, displayisolates = FALSE, vertex.col="color", edge.len=(1/n2$mel.dist)) 
dev.off()

