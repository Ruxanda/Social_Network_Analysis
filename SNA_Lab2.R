######################################################################################
######################################################################################
######################################################################################
### Social Network Analysis
### Lab 2
######################################################################################
######################################################################################
######################################################################################

### Calculating Centrality 

require(igraph)
setwd("~/Desktop/SOCIAL NETWORK ANALYSIS/Data/Krack Tech")
dat <- read.csv("Krack-Advice-no-row.csv", header=TRUE) # use “Lazega-Advice-Net-B.csv” which does not have a column of IDs ##
m <- as.matrix(dat)
net <- graph.adjacency(m, mode="directed") # the only difference between this and the weighted network code is that mode="directed" #
el <- get.edgelist(net)
traits <- read.csv("Krack-Atts.csv", header=TRUE) # use “Lazega-Atts-C.csv” which has all lowercase variable names, except for ID ##
g <- graph.empty()
g <- add.vertices(g, nrow(traits), ID=traits[,1], age=traits[,2], tenure=traits[,3], level=traits[,4], dept=traits[,5])

d <- read.csv("Krack-Advice-with-row.csv",header=TRUE,row.names=1,check.names=FALSE) # need first column as IDs - use “Lazega-Advice-Net.csv” ##
dm <- as.matrix(d) # coerces the data set as a matrix
dmg <- graph.adjacency(dm,mode="directed",weighted=NULL)
V(dmg)$name
V(dmg)$age <- traits$AGE[match(V(dmg)$name,traits$ID)]
V(dmg)$tenure <- traits$TENURE[match(V(dmg)$name,traits$ID)]
V(dmg)$level <- traits$LEVEL[match(V(dmg)$name,traits$ID)]
V(dmg)$dept <- traits$DEPT[match(V(dmg)$name,traits$ID)]

d.in <- degree(dmg, mode = c("in"), loops = TRUE, normalized = FALSE)
d.out <- degree(dmg, mode = c("out"), loops = TRUE, normalized = FALSE)
degree <- degree(dmg, loops = TRUE, normalized = FALSE)
btwn <- betweenness(dmg,  directed = FALSE)
close <- closeness(dmg, mode = c("all"))
close.in <- closeness(dmg, mode = c("in"))
close.out <- closeness(dmg, mode = c("out"))
eigen <- evcent(dmg)
eigen.dir <- evcent(dmg, directed = TRUE)
bon <- bonpow(dmg)
graph.density(dmg, loops=TRUE) 

table(d.in)
table(d.out)
table(degree)
table(btwn)
table(close)
table(eigen$vector)
table(bon)

dd <- data.frame(name = V(dmg)$name)
cb1 <- cbind(dd, d.in, d.out, degree, btwn, close, eigen$vector, bon, V(dmg)$age, V(dmg)$tenure, V(dmg)$level, V(dmg)$dept)
head(cb1)
cb1

n <- graph.adjacency(m, mode="directed", weighted=NULL, diag=FALSE)
V(n)$name
V(n)$size <- degree(net)
V(n)$color <- V(dmg)$level # assign the "level" attribute as the vertex color
V(n)$color <- gsub("1", "red", V(dmg)$color) 
V(n)$color <- gsub("2", "blue", V(dmg)$color) 
V(n)$color <- gsub("3", "green", V(dmg)$color)
plot.igraph(n, vertex.label=V(n)$name, layout=layout.fruchterman.reingold)

############################################################################
############################################################################
############################################################################

### Krack-Advice-with-row.csv

require(igraph)
setwd("~/Desktop/SOCIAL NETWORK ANALYSIS/Data/Krack Tech")

traits <- read.csv("Krack-Atts.csv", header = TRUE)
Advice_Data <- read.csv("Krack-Advice-with-row.csv", header = TRUE, row.names = 1, check.names = FALSE) # need first column as IDs
Advice_Matrix <- as.matrix(Advice_Data) # coerces the data set as a matrix
Advice <- graph.adjacency(Advice_Matrix, mode = "directed", weighted = NULL)
V(Advice)$name
V(Advice)$age    <- traits$AGE[match(V(Advice)$name,traits$ID)]
V(Advice)$tenure <- traits$TENURE[match(V(Advice)$name,traits$ID)]
V(Advice)$level  <- traits$LEVEL[match(V(Advice)$name,traits$ID)]
V(Advice)$dept   <- traits$DEPT[match(V(Advice)$name,traits$ID)]

Advice_InDegree       <- degree(Advice, mode = c("in"), loops = TRUE, normalized = FALSE)
Advice_OutDegree      <- degree(Advice, mode = c("out"), loops = TRUE, normalized = FALSE)
Advice_Degree         <- degree(Advice, loops = TRUE, normalized = FALSE)
Advice_Betweenness    <- betweenness(Advice,  directed = FALSE)
Advice_Closeness      <- closeness(Advice, mode = c("all"))
Advice_InCloseness    <- closeness(Advice, mode = c("in"))
Advice_OutCloseness   <- closeness(Advice, mode = c("out"))
Advice_Eigenvector    <- evcent(Advice)
Advice_Bonacich       <- bonpow(Advice)

ID <- data.frame(name = V(Advice)$name)
Advice_with_traits <- cbind(ID, Advice_InDegree, Advice_OutDegree, Advice_Degree, Advice_Betweenness, 
                            Advice_Closeness, Advice_InCloseness, Advice_OutCloseness, 
                            Advice_Eigenvector$vector, Advice_Bonacich,
                            V(Advice)$age, V(Advice)$tenure, V(Advice)$level, V(Advice)$dept)
head(Advice_with_traits)
names(Advice_with_traits) <- c("ID", "Advice_InDegree", "Advice_OutDegree", "Advice_Degree", "Advice_Betweenness", 
                               "Advice_Closeness", "Advice_InCloseness", "Advice_OutCloseness", 
                               "Advice_Eigenvector", "Advice_Bonacich", 
                               "Age", "Tenure", "Level", "Dept")
head(Advice_with_traits)
cor(Advice_with_traits[,2:10])


### Krack-Friend-with-row.csv

require(igraph)
setwd("~/Desktop/SOCIAL NETWORK ANALYSIS/Data/Krack Tech")

traits <- read.csv("Krack-Atts.csv", header = TRUE)
Friend_Data <- read.csv("Krack-Friend-with-row.csv", header = TRUE, row.names = 1, check.names = FALSE) # need first column as IDs
Friend_Matrix <- as.matrix(Friend_Data) # coerces the data set as a matrix
Friend <- graph.adjacency(Friend_Matrix, mode = "directed", weighted = NULL)
V(Friend)$name
V(Friend)$age    <- traits$AGE[match(V(Friend)$name,traits$ID)]
V(Friend)$tenure <- traits$TENURE[match(V(Friend)$name,traits$ID)]
V(Friend)$level  <- traits$LEVEL[match(V(Friend)$name,traits$ID)]
V(Friend)$dept   <- traits$DEPT[match(V(Friend)$name,traits$ID)]

Friend_InDegree       <- degree(Friend, mode = c("in"), loops = TRUE, normalized = FALSE)
Friend_OutDegree      <- degree(Friend, mode = c("out"), loops = TRUE, normalized = FALSE)
Friend_Degree         <- degree(Friend, loops = TRUE, normalized = FALSE)
Friend_Betweenness    <- betweenness(Friend,  directed = FALSE)
Friend_Closeness      <- closeness(Friend, mode = c("all"))
Friend_InCloseness    <- closeness(Friend, mode = c("in"))
Friend_OutCloseness   <- closeness(Friend, mode = c("out"))
Friend_Eigenvector    <- evcent(Friend)
Friend_Bonacich       <- bonpow(Friend)

ID <- data.frame(name = V(Friend)$name)
Friend_with_traits <- cbind(ID, Friend_InDegree, Friend_OutDegree, Friend_Degree, Friend_Betweenness, 
                            Friend_Closeness, Friend_InCloseness, Friend_OutCloseness, 
                            Friend_Eigenvector$vector, Friend_Bonacich,
                            V(Friend)$age, V(Friend)$tenure, V(Friend)$level, V(Friend)$dept)
head(Friend_with_traits)
names(Friend_with_traits) <- c("ID", "Friend_InDegree", "Friend_OutDegree", "Friend_Degree", "Friend_Betweenness", 
                               "Friend_Closeness", "Friend_InCloseness", "Friend_OutCloseness", 
                               "Friend_Eigenvector", "Friend_Bonacich", 
                               "Age", "Tenure", "Level", "Dept")
head(Friend_with_traits)
cor(Friend_with_traits[,2:10])


### Krack-Report-with-row.csv

require(igraph)
setwd("~/Desktop/SOCIAL NETWORK ANALYSIS/Data/Krack Tech")

traits <- read.csv("Krack-Atts.csv", header = TRUE)
Report_Data <- read.csv("Krack-Report-with-row.csv", header = TRUE, row.names = 1, check.names = FALSE) # need first column as IDs
Report_Matrix <- as.matrix(Report_Data) # coerces the data set as a matrix
Report <- graph.adjacency(Report_Matrix, mode = "directed", weighted = NULL)
V(Report)$name
V(Report)$age    <- traits$AGE[match(V(Report)$name,traits$ID)]
V(Report)$tenure <- traits$TENURE[match(V(Report)$name,traits$ID)]
V(Report)$level  <- traits$LEVEL[match(V(Report)$name,traits$ID)]
V(Report)$dept   <- traits$DEPT[match(V(Report)$name,traits$ID)]

Report_InDegree       <- degree(Report, mode = c("in"), loops = TRUE, normalized = FALSE)
Report_OutDegree      <- degree(Report, mode = c("out"), loops = TRUE, normalized = FALSE)
Report_Degree         <- degree(Report, loops = TRUE, normalized = FALSE)
Report_Betweenness    <- betweenness(Report,  directed = FALSE)
Report_Closeness      <- closeness(Report, mode = c("all"))
Report_InCloseness    <- closeness(Report, mode = c("in"))
Report_OutCloseness   <- closeness(Report, mode = c("out"))
Report_Eigenvector    <- evcent(Report)
Report_Bonacich       <- bonpow(Report)

ID <- data.frame(name = V(Report)$name)
Report_with_traits <- cbind(ID, Report_InDegree, Report_OutDegree, Report_Degree, Report_Betweenness, 
                            Report_Closeness, Report_InCloseness, Report_OutCloseness, 
                            Report_Eigenvector$vector, Report_Bonacich,
                            V(Report)$age, V(Report)$tenure, V(Report)$level, V(Report)$dept)
head(Report_with_traits)
names(Report_with_traits) <- c("ID", "Report_InDegree", "Report_OutDegree", "Report_Degree", "Report_Betweenness", 
                               "Report_Closeness", "Report_InCloseness", "Report_OutCloseness", 
                               "Report_Eigenvector", "Report_Bonacich", 
                               "Age", "Tenure", "Level", "Dept")
head(Report_with_traits)
cor(Report_with_traits[,2:10])


### All InDegree
InDegree_All <- cbind(Advice_with_traits[,c(1,2)], Friend_with_traits[,c(2)], Report_with_traits[,c(2,11,12,13,14)])
names(InDegree_All) <- c("ID", "Advice_InDegree", "Friend_InDegree", "Report_InDegree", "Age", "Tenure", "Level", "Dept")
InDegree_All

### All OutDegree
OutDegree_All <- cbind(Advice_with_traits[,c(1,3)], Friend_with_traits[,c(3)], Report_with_traits[,c(3,11,12,13,14)])
names(OutDegree_All) <- c("ID", "Advice_OutDegree", "Friend_OutDegree", "Report_OutDegree", "Age", "Tenure", "Level", "Dept")
OutDegree_All

### All Degree
Degree_All <- cbind(Advice_with_traits[,c(1,4)], Friend_with_traits[,c(4)], Report_with_traits[,c(4,11,12,13,14)])
names(Degree_All) <- c("ID", "Advice_Degree", "Friend_Degree", "Report_Degree", "Age", "Tenure", "Level", "Dept")
Degree_All

### All Betweenness
Betweenness_All <- cbind(Advice_with_traits[,c(1,5)], Friend_with_traits[,c(5)], Report_with_traits[,c(5,11,12,13,14)])
names(Betweenness_All) <- c("ID", "Advice_Betweenness", "Friend_Betweenness", "Report_Betweenness", "Age", "Tenure", "Level", "Dept")
Betweenness_All

### All Closeness
Closeness_All <- cbind(Advice_with_traits[,c(1,6)], Friend_with_traits[,c(6)], Report_with_traits[,c(6,11,12,13,14)])
names(Closeness_All) <- c("ID", "Advice_Closeness", "Friend_Closeness", "Report_Closeness", "Age", "Tenure", "Level", "Dept")
Closeness_All

### All InCloseness
InCloseness_All <- cbind(Advice_with_traits[,c(1,7)], Friend_with_traits[,c(7)], Report_with_traits[,c(7,11,12,13,14)])
names(InCloseness_All) <- c("ID", "Advice_InCloseness", "Friend_InCloseness", "Report_InCloseness", "Age", "Tenure", "Level", "Dept")
InCloseness_All

### All OutCloseness
OutCloseness_All <- cbind(Advice_with_traits[,c(1,8)], Friend_with_traits[,c(8)], Report_with_traits[,c(8,11,12,13,14)])
names(OutCloseness_All) <- c("ID", "Advice_OutCloseness", "Friend_OutCloseness", "Report_OutCloseness", "Age", "Tenure", "Level", "Dept")
OutCloseness_All

### All Eigenvector
Eigenvector_All <- cbind(Advice_with_traits[,c(1,9)], Friend_with_traits[,c(9)], Report_with_traits[,c(9,11,12,13,14)])
names(Eigenvector_All) <- c("ID", "Advice_Eigenvector", "Friend_Eigenvector", "Report_Eigenvector", "Age", "Tenure", "Level", "Dept")
Eigenvector_All

### All Bonacich
Bonacich_All <- cbind(Advice_with_traits[,c(1,10)], Friend_with_traits[,c(10)], Report_with_traits[,c(10,11,12,13,14)])
names(Bonacich_All) <- c("ID", "Advice_Bonacich", "Friend_Bonacich", "Report_Bonacich", "Age", "Tenure", "Level", "Dept")
Bonacich_All



