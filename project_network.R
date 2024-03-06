library(rvest)
library(dplyr)
library(dplyr)
library(purrr)
library(stringr)
library(igraph)
library(visNetwork)
library(geomnet)
#devtools::install_github("sctyner/geomnet")
library(statnet)
library(intergraph)
library(RColorBrewer)
library(networkD3)
library(reshape2)
library(htmlwidgets)

###########################################################
#                                                         #
#  Script used to create a network of Uppsala university  # 
#  programmes related to "teknik" and analyze it.         #
#  Authors:Stina Brunzell, Theodora Moldovan, Ida Nilsson #
#                                                         #
###########################################################

# Read scraped data
programs <- read.csv("programs2.csv")
courses <- read.csv("courses2.csv")

###########################################################
# Preprocessing 
###########################################################

# Step 1: List unique programs
programs_from_courses <- unique(courses$program_title)

# Step 2: Create a list of courses for each program
program_courses <- lapply(programs_from_courses, function(program) {
  courses %>% 
    filter(program_title == program) %>% 
    pull(course_name) %>% 
    unique()
})

# Step 3: Initialize the matrix
program_matrix <- matrix(0, nrow = length(programs_from_courses), ncol = length(programs_from_courses),
                         dimnames = list(programs_from_courses, programs_from_courses))

# Step 4: Calculate intersection counts
for(i in 1:length(programs_from_courses)) {
  for(j in 1:length(programs_from_courses)) {
    if (i != j) {
      # Calculate the intersection of courses between program i and j
      common_courses <- intersect(program_courses[[i]], program_courses[[j]])
      # Update the matrix with the count of common courses
      program_matrix[i, j] <- length(common_courses)
    }
  }
}

###########################################################
# igraph
###########################################################

# # Create the graph
# graph <- graph_from_adjacency_matrix(program_matrix, weighted = TRUE, mode = "undirected")
# 
# # Add node attributes
# V(graph)$credits <- programs$credits
# V(graph)$level <- programs$level
# V(graph)$language <- programs$language_of_instruction
# 
# # Define colors for languages 
# language_colors <- c("English" = "orange", "Swedish" = "lightblue")
# 
# # Define shapes for levels
# level_shapes <- c("Bachelor" = "square", "Master" = "circle")
# vertex_shapes <- sapply(V(graph)$level, function(l) level_shapes[l])
# 
# # Scale node size by credits 
# node_size <- sqrt(V(graph)$credits) * 0.5
# 
# # Combine edge list with weights 
# edge_list <- as_edgelist(graph)
# edge_weights <- E(graph)$weight
# edges_with_weights <- cbind(edge_list, weight = edge_weights)
# 
# # Generate layout
# layout_circle <- layout_in_circle(graph)
# 
# # Scale layout coordinates to increase spacing
# layout_circle <- layout_circle * 0.5
# 
# # Plot the graph (bad graph)
# par(bg="grey13", mar=c(0,0,0,0))
# plot(graph, 
#      layout = layout_circle,
#      vertex.size = node_size,
#      vertex.label.cex = 0.8,
#      vertex.label.color = "white",
#      vertex.color = language_colors[V(graph)$language],
#      vertex.shape = level_shapes,
#      vertex.frame.color = NA, 
#      edge.width = E(graph)$weight / 5,
#      #edge.label = E(graph)$weight, # label?
#      edge.label.cex = 0.6,
#      edge.color = "darkgrey",
#      edge.label.color = "white")
#      #main = "Science and Technology Programmes at Uppsala University")

# # Add legend for language of instruction
# legend("bottomright",
#        legend = names(language_colors),
#        fill = language_colors,
#        title = "Language of instruction",
#        cex = 0.5)

###########################################################
# forceNetwork() from networkD3 (not customizable enough)
###########################################################
# 
# # Create a nodes dataframe from the row names of the program_matrix
# nodes <- data.frame(name = rownames(program_matrix))
# 
# # Add a group column based on level
# nodes$group <- ifelse(grepl("Bachelor", nodes$name), "Bachelor", "Master")
# 
# # Convert to numeric groups for visualization purposes
# nodes$group <- as.numeric(factor(nodes$group))
# 
# # Melt the program_matrix to create a links dataframe
# links <- melt(program_matrix)
# links <- links[links$value != 0, ]  # Filter out links with 0 common courses
# 
# # Convert program names to indices for the source and target columns
# links$Var1 <- match(links$Var1, nodes$name) - 1
# links$Var2 <- match(links$Var2, nodes$name) - 1
# 
# # Rename columns for forceNetwork()
# links <- rename(links, source = Var1, target = Var2, value = value)
# 
# # Plot the network with the group information
# p <- forceNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target",
#                   Value = "value", NodeID = "name", Group = "group", opacity = 0.8,
#                   linkWidth = JS("function(d) { return Math.sqrt(d.value); }"))
# 
# # Save the widget
# saveWidget(p, file=paste0( getwd(), "/networkInteractive.html"))

###########################################################
# visNetwork() from visNetwork
###########################################################

# Create nodes data frame
nodes <- data.frame(id = 1:length(programs_from_courses), label = programs)
colnames(nodes) <- c("id", "label", "credits", "code", "level", "url", "location", "instruction", "pace", "language", "outline")
#id has to be the same like from and to columns in edges
nodes$id <- nodes$label

# Initialize an empty data frame for edges
edges <- data.frame(from = character(), to = character(), width = numeric())

# Iterate over the matrix to fill the 'edges' data frame
for (i in 1:nrow(program_matrix)) {
  for (j in 1:ncol(program_matrix)) {
    if (program_matrix[i, j] > 0) {
      from_program <- rownames(program_matrix)[i]
      to_program <- colnames(program_matrix)[j]
      width <- program_matrix[i, j]
      edges <- rbind(edges, data.frame(from = from_program, to = to_program, width = width))
    }
  }
}

#save(program_matrix, file="program_matrix.RData")

# Define colors for each language
colors <- setNames(c("#1f77b4", "#ff7f0e"), c("English", "Swedish"))
nodes$color <- colors[nodes$language]

# Define shapes for each level
shapes <- setNames(c("square", "triangle"), c("Bachelor", "Master"))
nodes$shape <- shapes[as.character(nodes$level)]

# Prepare the nodes dataframe for visNetwork
nodes$color.background <- nodes$color
nodes$color.border <- nodes$color
#nodes$shape <- nodes$shape

# Create the network visualization
visgraph <- visNetwork(nodes, edges, main="Science and technology programmes at UU") %>%
  visIgraphLayout(layout = "layout_in_circle") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, manipulation = FALSE) %>%
  visNodes(shape = nodes$shape, color = list(background = nodes$color), font = list(face = "times", size = 30, distance = 200)) %>%
  visInteraction(hover = TRUE) %>%
  visPhysics(solver = "forceAtlas2Based",
             forceAtlas2Based = list(gravitationalConstant = -50,
                                     centralGravity = 0.01,
                                     springLength = 100,
                                     springConstant = 0.08),
             maxVelocity = 50,
             minVelocity = 0.1) %>%
  visEvents(dragEnd = "function(nodeId, pointer) {
                  this.setOptions({physics:false});
              }")

saveWidget(visgraph, 'visNetworkGraph.html', selfcontained = TRUE)

# #Create graph for Louvain
# visgraph <- graph_from_data_frame(edges2, directed = FALSE)
# 
# #Louvain Comunity Detection
# cluster <- cluster_louvain(visgraph)
# 
# cluster_df <- data.frame(as.list(membership(cluster)))
# cluster_df <- as.data.frame(t(cluster_df))
# cluster_df$label <- rownames(cluster_df)
# 
# #Create group column
# nodes2 <- left_join(nodes2, cluster_df, by = "label")
# colnames(nodes2)[3] <- "group"

###########################################################
# Centrality
###########################################################
graph <- graph_from_adjacency_matrix(program_matrix, weighted = TRUE, mode = "undirected")

# Calculate degree centrality
dc <- igraph::degree(graph)
max(dc) # highest dc value in this network = 51
which.max(dc) # Master's Programme in Engineering Physics 

# Calculate betweenness centrality
bc <- igraph::betweenness(graph)
max(bc) # highest bc value in this network = 345.8234
which.max(bc) # Master's Programme in Biophysics

# Calculate eigenvector centrality
ec <- igraph::eigen_centrality(graph)$vector
max(ec) # highest degree value in this network = 1
which.max(ec) # Master's Programme in Engineering Physics 

# Summing up centrality scores
graph_cent <- data.frame (
  degree = dc,
  betweenness = bc,
  eigenvector = ec
)

# Top 10 programs by degree centrality
graph_cent <- graph_cent[order(-graph_cent$degree),]
head(graph_cent, 10)

# Top 10 programs by betweenness centrality
graph_cent <- graph_cent[order(-graph_cent$betweenness),]
head(graph_cent, 10)

# Top 10 programs by eigenvector centrality
graph_cent <- graph_cent[order(-graph_cent$eigenvector),]
head(graph_cent, 10)

###########################################################
# Community detection
###########################################################
par(bg="white", mar=c(0,0,0,0))

# Community detection using the Louvain method (modularity optimization)
# Quantifies the quality of a community structure by measuring the density of connections within communities relative to connections between communities
louvain_communities <- cluster_louvain(graph)
# Print the number of communities detected: 73
cat("Number of communities detected by Louvain method:", length(louvain_communities), "\n")
# Identify the members of each community detected by the Louvain method
louvain_membership <- membership(louvain_communities)
# Print the number of members in each community
cat("\nNumber of members in each community (Louvain method):\n")
table(louvain_membership)
plot(louvain_communities, graph, vertex.cex = 4, vertex.size=igraph::degree(graph)+6,
     vertex.label.cex = 0.7, vertex.label.color= "black", edge.curved=T,  edge.arrow.size = 0.5, main="Louvain community detection")

# Community detection using the Infomap algorithm (information flow)
# Aims to find a partition of the network that optimally compresses the information flow in the network
# Treats random walks on the network as paths of information flow and uses a map equation to measure the information-theoretic cost of encoding these paths
# Identifies communities by recursively compressing the network into modules that minimize the description length of the information flow
infomap_communities <- cluster_infomap(graph)
# Print the number of communities detected: 84
cat("Number of communities detected by Infomap algorithm:", length(infomap_communities), "\n")
# Identify the members of each community detected by the Infomap algorithm
infomap_membership <- membership(infomap_communities)
cat("\nNumber of members in each community (Infomap algorithm):\n")
table(infomap_membership)
plot(infomap_communities, graph, vertex.cex = 4, vertex.size=igraph::degree(graph)+6,
     vertex.label.cex = 0.7, vertex.label.color= "black", edge.curved=T,  edge.arrow.size = 0.5, main="Infomap community detection")
