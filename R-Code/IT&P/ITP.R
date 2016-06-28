# Working directory "..

rm(list=ls(all=TRUE))
library(igraph)
library(ggplot2)
library(blockmodeling)
igraph.options(print.vertex.attributes = TRUE)  
igraph.options(print.edge.attributes = TRUE)

# Setting the working directory
setwd("~/GitHub/Group-Informatics/R-Code/IT&P")


mod3<- read.graph("mod3d.net", format="pajek")
mod4<- read.graph("mod4d.net", format="pajek")
mod5<- read.graph("mod5d.net", format="pajek")
mod6<- read.graph("mod6d.net", format="pajek")

# need to set the name attribute on the vertices for the matrix conversion below
V(mod3)$name <- V(mod3)$id
V(mod4)$name <- V(mod4)$id
V(mod5)$name <- V(mod5)$id
V(mod6)$name <- V(mod6)$id

mod3 <- delete.vertices(mod3, c(2,9,19,20,21))
mod4 <- delete.vertices(mod4, c(8,18,19))
mod5 <- delete.vertices(mod5, c(8))
mod6 <- delete.vertices(mod6, c(8))

# create a matrix version of the data for blockmodeling
mod3m <- get.adjacency(mod3)
mod4m <- get.adjacency(mod4)
mod5m <- get.adjacency(mod5)
mod6m <- get.adjacency(mod6)

V(mod3)$label <- V(mod3)$id
V(mod3)$shape <- "none"
V(mod3)[ x == 1 ]$shape <- "vrectangle"
V(mod3)[ x == 2 ]$shape <- "crectangle"
V(mod3)[ x == 3 ]$shape <- "rectangle"
V(mod3)[ x == 4 ]$shape <- "circle"
V(mod3)[ x == 5 ]$shape <- "square"
V(mod3)[ x == 6 ]$shape <- "csquare"

V(mod3)[ x == 1 ]$color <- gray(0)
V(mod3)[ x == 2 ]$color <- gray(.2)
V(mod3)[ x == 3 ]$color <- gray(.4)
V(mod3)[ x == 4 ]$color <- gray(.6)
V(mod3)[ x == 5 ]$color <- gray(.8)
V(mod3)[ x == 6 ]$color <- gray(1)

V(mod3)$labelcolor <- "black"


V(mod3)[ x == 1 ]$label.color <- "white"
V(mod3)[ x == 2 ]$label.color <- "white"
V(mod3)[ x == 3 ]$label.color <- "white"
V(mod3)[ x == 4 ]$label.color <- "white"
V(mod3)[ x == 5 ]$label.color <- "black"
V(mod3)[ x == 6 ]$label.color <- "black"


# spinglass.community(mod3, spins=8)
# V(mod3)$size <- (1+(alpha.centrality(mod3, alpha=.005)))^2
E(mod3)$width <- sqrt(E(mod3)$weight/50)
E(mod3)$arrow.size <-0.4

		filename=paste("output/","module3",".pdf")
		pdf(filename)
		plot.igraph(mod3, vertex.label.cex=.7, layout=layout.fruchterman.reingold(mod3, weights=V(mod3)$weight))
		dev.off()


# Mod 4
V(mod4)$label <- V(mod4)$id
V(mod4)$shape <- "none"
V(mod4)[ x == 1 ]$shape <- "vrectangle"
V(mod4)[ x == 2 ]$shape <- "crectangle"
V(mod4)[ x == 3 ]$shape <- "rectangle"
V(mod4)[ x == 4 ]$shape <- "circle"
V(mod4)[ x == 5 ]$shape <- "square"
V(mod4)[ x == 6 ]$shape <- "csquare"

V(mod4)[ x == 1 ]$color <- gray(0)
V(mod4)[ x == 2 ]$color <- gray(.2)
V(mod4)[ x == 3 ]$color <- gray(.4)
V(mod4)[ x == 4 ]$color <- gray(.6)
V(mod4)[ x == 5 ]$color <- gray(.8)
V(mod4)[ x == 6 ]$color <- gray(1)

V(mod4)$labelcolor <- "black"


V(mod4)[ x == 1 ]$label.color <- "white"
V(mod4)[ x == 2 ]$label.color <- "white"
V(mod4)[ x == 3 ]$label.color <- "white"
V(mod4)[ x == 4 ]$label.color <- "white"
V(mod4)[ x == 5 ]$label.color <- "black"
V(mod4)[ x == 6 ]$label.color <- "black"


# spinglass.community(mod4, spins=8)
# V(mod4)$size <- (1+(alpha.centrality(mod4, alpha=.005)))^2
E(mod4)$width <- sqrt(E(mod4)$weight/50)
E(mod4)$arrow.size <-0.4

		filename=paste("output/","module4",".pdf")
		pdf(filename)
		plot.igraph(mod4, vertex.label.cex=.7, layout=layout.fruchterman.reingold(mod4, weights=V(mod4)$weight))
		dev.off()
		
# Mod 5
V(mod5)$label <- V(mod5)$id
V(mod5)$shape <- "none"
V(mod5)[ x == 1 ]$shape <- "vrectangle"
V(mod5)[ x == 2 ]$shape <- "crectangle"
V(mod5)[ x == 3 ]$shape <- "rectangle"
V(mod5)[ x == 4 ]$shape <- "circle"
V(mod5)[ x == 5 ]$shape <- "square"
V(mod5)[ x == 6 ]$shape <- "csquare"

V(mod5)[ x == 1 ]$color <- gray(0)
V(mod5)[ x == 2 ]$color <- gray(.2)
V(mod5)[ x == 3 ]$color <- gray(.4)
V(mod5)[ x == 4 ]$color <- gray(.6)
V(mod5)[ x == 5 ]$color <- gray(.8)
V(mod5)[ x == 6 ]$color <- gray(1)

V(mod5)$labelcolor <- "black"


V(mod5)[ x == 1 ]$label.color <- "white"
V(mod5)[ x == 2 ]$label.color <- "white"
V(mod5)[ x == 3 ]$label.color <- "white"
V(mod5)[ x == 4 ]$label.color <- "white"
V(mod5)[ x == 5 ]$label.color <- "black"
V(mod5)[ x == 6 ]$label.color <- "black"


# spinglass.community(mod5, spins=6)
# V(mod5)$size <- (1+(alpha.centrality(mod5, alpha=.005)))^2
E(mod5)$width <- sqrt(E(mod5)$weight/50)
E(mod5)$arrow.size <-0.4

		filename=paste("output/","module5",".pdf")
		pdf(filename)
#		layout.mds(mod5)
		plot.igraph(mod5, vertex.label.cex=.7, layout=layout.fruchterman.reingold(mod5, weights=V(mod5)$weight))
		dev.off()


# Mod 6

V(mod6)$label <- V(mod6)$id
V(mod6)$shape <- "none"
V(mod6)[ x == 1 ]$shape <- "vrectangle"
V(mod6)[ x == 2 ]$shape <- "crectangle"
V(mod6)[ x == 3 ]$shape <- "rectangle"
V(mod6)[ x == 4 ]$shape <- "circle"
V(mod6)[ x == 5 ]$shape <- "square"
V(mod6)[ x == 6 ]$shape <- "csquare"

V(mod6)[ x == 1 ]$color <- gray(0)
V(mod6)[ x == 2 ]$color <- gray(.2)
V(mod6)[ x == 3 ]$color <- gray(.4)
V(mod6)[ x == 4 ]$color <- gray(.6)
V(mod6)[ x == 5 ]$color <- gray(.8)
V(mod6)[ x == 6 ]$color <- gray(1)

V(mod6)$labelcolor <- "black"


V(mod6)[ x == 1 ]$label.color <- "white"
V(mod6)[ x == 2 ]$label.color <- "white"
V(mod6)[ x == 3 ]$label.color <- "white"
V(mod6)[ x == 4 ]$label.color <- "white"
V(mod6)[ x == 5 ]$label.color <- "black"
V(mod6)[ x == 6 ]$label.color <- "black"


# spinglass.community(mod6, spins=8)
# V(mod6)$size <- sqrt((20+(alpha.centrality(mod6, alpha=.005))))
E(mod6)$width <- sqrt(E(mod6)$weight/50)
E(mod6)$arrow.size <-0.4

		filename=paste("output/","module6",".pdf")
		pdf(filename)
		plot.igraph(mod6, vertex.label.cex=.7, layout=layout.fruchterman.reingold(mod6, weights=V(mod6)$weight))
		dev.off()


# Fruchterman-Reingold ALL ==========================================

		filename=paste("output/","springAll","b.pdf")
		pdf(filename)
		# repulse=TRUE, repeqdis=.07, 
		
		E(mod3)$width <- (E(mod3)$weight/23)
		E(mod4)$width <- (E(mod4)$weight/23)
		E(mod5)$width <- (E(mod5)$weight/23)
		E(mod6)$width <- (E(mod6)$weight/23)

		plot.igraph(mod3, main="mod3", vertex.label.cex=.7, layout=layout.Fruchterman-Reingold(mod3, repulse=TRUE, repeqdis=1, mass=.72))
		plot.igraph(mod4, main="mod4", vertex.label.cex=.7, layout=layout.Fruchterman-Reingold(mod4, repulse=TRUE, repeqdis=1, mass=.72))
		plot.igraph(mod5, main="mod5", vertex.label.cex=.7, layout=layout.Fruchterman-Reingold(mod5, repulse=TRUE, repeqdis=1, mass=.72))
		plot.igraph(mod6, main="mod6", vertex.label.cex=.7, layout=layout.Fruchterman-Reingold(mod6, repulse=TRUE, repeqdis=1, mass=.72))
		dev.off()


# Kawai ALL ==========================================

		filename=paste("output/","KawaiAll","b.pdf")
		pdf(filename)
		# repulse=TRUE, repeqdis=.07, 
		plot.igraph(mod3, main="mod3", vertex.label.cex=.7, layout=layout.kamada.kawai(mod3,  weights=sqrt(E(mod3)$weight+.01)))
		plot.igraph(mod4, main="mod4", vertex.label.cex=.7, layout=layout.kamada.kawai(mod4,  weights=sqrt(E(mod4)$weight+.01)))
		plot.igraph(mod5, main="mod5", vertex.label.cex=.7, layout=layout.kamada.kawai(mod5,  weights=sqrt(E(mod5)$weight+.01)))
		plot.igraph(mod6, main="mod6", vertex.label.cex=.7, layout=layout.kamada.kawai(mod6,  weights=sqrt(E(mod6)$weight+.01)))
		dev.off()

# spinglass communities
##=====================SPIN GLASS============================================

V(mod3)$label.color <- "black"
V(mod4)$label.color <- "black"
V(mod5)$label.color <- "black"
V(mod6)$label.color <- "black"

com3 <- spinglass.community(mod3, spins=6) 
grayer3 <- 1/(com3$membership+1) 
V(mod3)$color <- gray(grayer3) 

com4 <- spinglass.community(mod4, spins=6) 
grayer4 <- 1/(com4$membership+1) 
V(mod4)$color <- gray(grayer4) 

com5 <- spinglass.community(mod5, spins=6) 
grayer5 <- 1/(com5$membership+1) 
V(mod5)$color <- gray(grayer5) 

com6 <- spinglass.community(mod6, spins=6) 
grayer6 <- 1/(com6$membership+1) 
V(mod6)$color <- gray(grayer6) 

		filename=paste("output/","ALL","spin.pdf")
		pdf(filename)
		plot.igraph(mod3, main="mod3", vertex.label.cex=.7, layout=layout.kamada.kawai(mod3, weights=E(mod3)$weight+.01))
		plot.igraph(mod4, main="mod4", vertex.label.cex=.7, layout=layout.kamada.kawai(mod4, weights=E(mod4)$weight+.01))
		plot.igraph(mod5, main="mod5", vertex.label.cex=.7, layout=layout.kamada.kawai(mod5, weights=E(mod5)$weight+.01))
		plot.igraph(mod6, main="mod6", vertex.label.cex=.7, layout=layout.kamada.kawai(mod6, weights=E(mod6)$weight+.01))

		dev.off()

