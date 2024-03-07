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

load("program_matrix.Rdata")
programs <- read.csv("programs_full.csv")

# Create nodes data frame
nodes <- data.frame(id = 1:87, label = programs)
colnames(nodes) <- c("id", "label")
nodes <- nodes[, 1:2]
# community detection does not like punctuation signs 
nodes$label <- gsub(" - ", " – ", nodes$label) # get rid of em dashes
nodes$label <- gsub("-", " ", nodes$label) # get rid of en dashes
nodes$label <- gsub(":", "", nodes$label) # get rid of :
nodes$label <- gsub("/", " ", nodes$label) # replace / with a space 
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

#Create graph for Louvain
#visgraph <- graph_from_data_frame(edges, directed = FALSE)
visgraph <- graph_from_adjacency_matrix(program_matrix, weighted = TRUE, mode = "undirected")

#Louvain Comunity Detection
louvain_communities <- cluster_louvain(visgraph)

cluster_df <- data.frame(as.list(membership(louvain_communities)))
cluster_df <- as.data.frame(t(cluster_df))
cluster_df$label <- rownames(cluster_df)

# Preprocess labels in cluster_df to match format in nodes
cluster_df$label <- gsub("\\.{3}", " – ", cluster_df$label) # Replace ellipses with dashes
cluster_df$label <- gsub("\\.{2}", " ", cluster_df$label) # Replace double space with single space
cluster_df$label <- gsub("\\.", " ", cluster_df$label) # Replace dots with spaces
cluster_df$label <- gsub(" s ", "'s ", cluster_df$label) # Correct apostrophe placement

#Create group column
nodes <- left_join(nodes, cluster_df, by = "label")
colnames(nodes)[3] <- "group"

visgraph_communities <- visNetwork(nodes, edges) %>% #main = "Communities of programmes at UU"
  visIgraphLayout(layout = "layout_in_circle") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, manipulation = FALSE, selectedBy = "group") %>%
  visNodes(font = list(face = "times", size = 30, distance = 50)) %>%
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

saveWidget(visgraph_communities, 'visNetworkGraphCommunities.html', selfcontained = TRUE)
