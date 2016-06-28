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
		## Plot the largest weak component
	cl <- component.largest(n, connected="weak")        # Who's in the largest component? 
	cl 
	filename=paste("output/","6graphSixLargest-Weak-Component",i,".pdf")
	pdf(filename)
	gplot(n[cl,cl], boxed.lab=TRUE, label.cex=0.5, label.col=4, label=network.vertex.names(n)[cl], displayisolates=FALSE) # Plot the largest 		weak component
	dev.off()
	
	cl <- component.largest(n, connected="strong")        # Who's in the largest component? 
	cl 
	filename=paste("output/","7graphSevenLargest-Strong-Component",i,".pdf")
	pdf(filename)
	gplot(n[cl,cl], boxed.lab=TRUE, label.cex=0.5, label.col=4, label=network.vertex.names(n)[cl], displayisolates=FALSE) # Plot the largest 		weak component
	dev.off()
	}