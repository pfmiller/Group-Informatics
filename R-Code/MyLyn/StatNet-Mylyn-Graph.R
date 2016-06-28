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
  
  ## Calculate centrality measures
  degree(n)
  ideg <- degree(n, cmode="indegree")
  odeg <- degree(n, cmode="outdegree")
  
  ## Use centrality scores to size and color the network plot
  filename=paste("output/","GraphTestDiscussion",i,".pdf")
  pdf(filename)
  gplot(n,vertex.cex=(ideg+odeg)^0.5/2, vertex.sides=50, label.cex=0.8, vertex.col=rgb(odeg/max(odeg), 0, ideg/max	(ideg)), label=network.vertex.names(n), displayisolates=FALSE,  boxed.labels=TRUE)
  dev.off()
}
