data <- read.csv("C:\\Users\\Arwa\\Desktop\\deezer_europe_edges.csv")

G <- graph_from_data_frame(data, directed = FALSE)
G

install.packages("igraph")
library(igraph)

# Perform spring layout with 15 iterations
pos <- layout_with_fr(G, niter = 30)
pos <- layout_randomly(G)

# Plotting the graph
plot(G, layout = layout.fruchterman.reingold, edge.arrow.size = 0.5, vertex.size = 5,
     vertex.label=NA,vertex.color='#2D9596',edge.color='black')

##################################################################################################################################################


# 3) Clustering Coefficient
# what is the Clustering Ceofficient ?
clustering_coefficients <- transitivity(G , type = "local")
hist(clustering_coefficients, main = "Clustering Coefficient Distribution",
     xlab = "Clustering Coefficient", ylab = "Frequency")
avg_clustering_coefficient <- mean(clustering_coefficients, na.rm = TRUE)
# The number of isolated nodes?
isolated_nodes <- which(degree(G) == 0)
num_isolated_nodes <- length(isolated_nodes)
# Calculate percentage of isolated nodes
total_nodes <- vcount(G)
percentage_isolated_nodes <- (num_isolated_nodes / total_nodes) * 100

cat("Number of isolated nodes:", num_isolated_nodes, "\n")
cat("Total number of nodes:", total_nodes, "\n")
cat("Percentage of isolated nodes:", percentage_isolated_nodes, "%\n")
# Calculating the number of traingles in a graph
triads <- triangles(G)
num_of_unique_triangles <- sum(triads)/3
cat("Number of Unique Triangles:", num_of_unique_triangles, "\n")
# The average number of triangles a node could be a part of
print(mean(triads))
# Median 
print(median(triads))
# components:
components(G)
#______________________________________________________________________
# 4) Bridges
bridge_edges <- bridges(G)
# Print the result
cat("Does the graph contain bridges?", any(bridge_edges), "\n")
# Get the number of bridges that exist in the grpah:
no_of_bridges <- length(bridge_edges)
print(no_of_bridges)

bridge_info <- bridges(G)
articulation_points <- articulation.points(G)
no_of_articulation_points <- length(articulation_points)
# Create a plot with igraph

V(G)$color <- "orange"

##  Set articulation points to red:
V(G)$color[ articulation.points(G) ] <- "red"

##  Set normal edges to black:
E(G)$color <- "grey"


bridge_indices <- which(!is.na(bridges(G)))
for (i in bridge_indices)
{
  V(G)$color[i] <- 'red'
}




plot(G, layout = layout.fruchterman.reingold, 
     main = 'Bridges and Articulate Points Highlighted',
     vertex.label = NA,
     vertex.size = 10,
     vertex.color= V(G)$color,
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width= 2, 
     edge.color=E(G)$color)


#_______________________________________________________________________
# 5) Assortativity=
assortativity_value <- assortativity_degree(G, directed = FALSE)
print(assortativity_value)



###############################################################################################################################################################################


#degree centrality (normalized)
# degree centrality measures the number of connections (edges) each user node has in the network
degree_centrality <- degree(G, mode = "all", normalized = TRUE)
degree_centrality
#printing the 10 node which has the highest centrality
#node 867 is the node with the highest degree centrality
#this mean this node  is connected to more users in terms of mutual follower relationships
top_10_DC <- head(order(degree_centrality, decreasing = TRUE), 10)
top_10_DC 

#plotting degree centrality with th counts
hist(degree_centrality,  main="Degree Centrality Histogram", xlab="Degree Centrality", ylab="Counts")


# Betweenness Centrality
# betweenness centrality represents the importance of a node in terms of facilitating communication or interactions between other nodes in the network
betweenness_centrality <- betweenness(G)

# Top 10 nodes by betweenness centrality
#node 13548 is the user has user has highest betweenness centrality
#this mean it plays a crucial role in connecting different parts of the social network
#Information or interactions might flow through this user to reach other users
top_10_bet <- head(order(betweenness_centrality, decreasing = TRUE), 10)
top_10_bet

#plotting betwenness
hist(betweenness_centrality, main="Betweenness Centrality Histogram", xlab="Betweenness Centrality", ylab="Counts")



# Closeness Centrality
#Closeness centrality measures how quickly a node can reach all other nodes in the network
closeness_centrality <- closeness(G)

# Top 10 nodes by closeness centrality
#Node 13548 is the user has the highest closeness centrality
#this mean that it can reach other users in the network more quickly than any other user
#it well-connected and central to the network
top_10_close <- head(order(closeness_centrality, decreasing = TRUE), 10)
top_10_close

#plotting closeness

par(mfrow=c(1,1), mar=c(5, 5, 4, 2) + 0.1, cex.lab=1.5, cex.axis=1.5, cex.main=2)

# Create the histogram
hist(closeness_centrality,main="Closeness Centrality Histogram", xlab="Closeness Centrality", ylab="Counts")


# Eigenvector Centrality
#eigenvector centrality measures the influence of a node in the network
#putting in considration not only the number of connections a node has but also the importance of the nodes to which it is connected
eigenvector_centrality <- evcent(G)$vector

# Top 10 nodes by eigenvector centrality
#Node 19823 is the user has the highest eigenvector centrality
#it is ot only well connected but also connected to other influential users
top_10_EV <- head(order(eigenvector_centrality, decreasing = TRUE), 10)
top_10_EV


#plotting eigen vector
hist(eigenvector_centrality,  main="Eigenvector Centrality Histogram", xlab="Eigenvector Centrality", ylab="Counts")


# Getting the degree of each vertex in the graph
deg_1=degree(G)              # 1 2 3 4 Node
deg_1                        # 1 3 2 2 it's degree
hist(deg_1)
t1 <- table(deg_1) # convert this list to table to count the occurrance of each degree
t1                          # 1 2 3
# 1 2 1
# Ploting the degree distribution
barplot(t1, xlab = "Degree", ylab = "frequencies",
        main = "Degree distribution",
        col = "skyblue")

# normalize that values
relafreq_1 <- t1/sum(t1)

# ploting the normalized degree distribution
barplot(relafreq_1, xlab = "Degree", ylab = "Relative frequencies",
        main = "Degree distribution",
        col = "skyblue")

# the data is too big, from the graph we can see that degree more
# than 30 tends to be zero so we will get the first 30 values only

subset=relafreq_1[0:30]
subset

barplot(subset, xlab = "Degree", ylab = "Relative frequencies",
        main = "Degree distribution",
        col = "skyblue")


#####################


# initials an array all in black in the size of the numbers of the vertices 
colors= rep("#000000", vcount(G))

# count all the communities and extract the vectors 
communities= membership(cluster_louvain(G))

# get the no. of the unique communities
length(unique(communities))

# For loop to give each node a color for their specific community 
for (com in unique(communities)) {
  color <- sprintf("#%06X", sample(0:0xFFFFFF, 1))  # Generate random RGB color
  
  
  # Fill colors list with the particular color for the community nodes
  colors[which(communities == com)] <- color
}

# plot the graph of the communities 
plot(G, layout = layout_with_fr, vertex.size = 5, vertex.label = NA,
     edge.width = 0.05, vertex.color = colors)
