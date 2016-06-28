# > install.packages("MASS")
# > install.packages("tnet")
# Clean space
rm(list=ls(all=TRUE))

# Load tnet
library(tnet)
library(MASS)

files <- system("ls data/release*TNET*",intern=T)

## Betweenness
for (i in 1:length(files)) {
	#this function is in the MASS package
	fileNamer = as.character(i)
	#Import the files
	tcommentLinks1 <- read.table(files[i], header=TRUE, dec=".", fill=TRUE)
	tcommentLinksMatrix <- as.matrix(tcommentLinks1)
	# Check network (remove self-loops)
	tcommentLinksMatrix <- as.tnet(tcommentLinksMatrix, type="weighted one-mode tnet")
	newBetween <- betweenness_w(tcommentLinksMatrix)
	# Extract active ids from output
	# Display results
	newBetween
	#filter results to eliminate those with 0 betweenness
	test <- apply(newBetween, 1, function(.row) any((.row[2]==0)))
	cbind(newBetween, test)
	newBetween <- newBetween[!test,]
	write.matrix(newBetween,file=paste("betweenness",fileNamer))
}

## Closeness
for (i in 1:length(files)) {
	#this function is in the MASS package
	fileNamer = as.character(i)
	#Import the files
	tcommentLinks1 <- read.table(files[i], header=TRUE, dec=".", fill=TRUE)
	tcommentLinksMatrix <- as.matrix(tcommentLinks1)
	# Check network (remove self-loops)
	tcommentLinksMatrix <- as.tnet(tcommentLinksMatrix, type="weighted one-mode tnet")
	inDegree <- degree_w(tcommentLinksMatrix, type="in")
	outDegree <- degree_w(tcommentLinksMatrix, type="out")
	kek <- degree_w(tcommentLinksMatrix, type="out")
	
	# Extract active ids from output
	# Display results
	inDegree
	outDegree
	#filter results to eliminate those with 0 betweenness
	inTest <- apply(inDegree, 1, function(.row) any((.row[2]==0)))
	outTest <- apply(outDegree, 1, function(.row) any((.row[2]==0)))
	cbind(inDegree, inTest)
	cbind(outDegree, outTest)
	inDegree <- inDegree[!inTest,]
	outDegree <- outDegree[!outTest,]

	write.matrix(inDegree,sep=",",file=paste("inDegree",fileNamer,".csv"))
	write.matrix(outDegree,sep=",",file=paste("outDegree",fileNamer,".csv"))

}


