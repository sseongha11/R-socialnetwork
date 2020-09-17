# 1. Network in igraph

rm(list = ls()) # Remove all the objects we created so far.

library(igraph) # Load the igraph package

# 2. Reading & modifying network data from files

nodes <- read.csv("dataset-blockchain-nodes.csv", header=T, as.is=T)

links <- read.csv("dataset-blockchain-edges.csv", header=T, as.is=T)

head(nodes)

head(links)

nrow(nodes); length(unique(nodes$id))

nrow(links); nrow(unique(links[,c("from", "to")]))

# Notice that there are more links than unique from-to combinations. That means we have cases in the data where there are multiple links between the same two nodes. We will collapse all links of the same type between the same two nodes by summing their weights, using aggregate() by “from”, “to”, & “type”. We don’t use simplify() here so as not to collapse different link types.

links <- aggregate(links[,3], links[,-3], sum)

links <- links[order(links$from, links$to),]

colnames(links)[4] <- "weight"

rownames(links) <- NULL

# 3. Turning networks into igraph objects

# We start by converting the raw data to an igraph network object. Here we use igraph’s graph.data.frame function, which takes two data frames: d and vertices.

# d describes the edges of the network. Its first two columns are the IDs of the source and the target node for each edge. The following columns are edge attributes (weight, type, label, or anything else).
# vertices starts with a column of node IDs. Any following columns are interpreted as node attributes.

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 

class(net)

net

# We also have easy access to nodes, edges, and their attributes with:

E(net)       # The edges of the "net" object

V(net)       # The vertices of the "net" object

E(net)$type  # Edge attribute "type"

V(net)$position # Vertex attribute "position"

V(net)$discipline # Vertex attribute "discipline"

V(net)$gender # Vertex attribute "gender"

# Now that we have our igraph network object, let's make a first attempt to plot it.

plot(net, edge.arrow.size=.4,vertex.label=NA)

# That doesn’t look very good. Let’s start fixing things by removing the loops in the graph.

net <- simplify(net, remove.multiple = F, remove.loops = T)

plot(net, edge.arrow.size=.4,vertex.label=NA)

# You might notice that we could have used simplify to combine multiple edges by summing their weights with a command like simplify(net, edge.attr.comb=list(weight="sum","ignore")). The problem is that this would also combine multiple edge types (in our data: “hyperlinks” and “mentions”).

# If you need them, you can extract an edge list or a matrix from igraph networks.

as_edgelist(net, names=T)

as_adjacency_matrix(net, attr="weight")

# Or data frames describing nodes and edges:

as_data_frame(net, what="edges")

as_data_frame(net, what="vertices")

# 5. Plotting networks with igraph
# Plotting with igraph: the network plots have a wide set oparameters you can set. Those include node options 

# plot with curved edges (edge.curved=.1) and reduce arrow size:
plot(net, edge.arrow.size=.4, edge.curved=.1)

# Set edge colour to gray, and the node colour to orange.
# Replace the vertex label with the node names stored in "name"

plot(net, edge.arrow.size=.2, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(net)$name, vertex.label.color="black",
     vertex.label.cex=.7)

# Generate colors based on media type:

colrs <-c("gray50", "tomato", "gold", "azure", "beige", "coral", "cornsilk", "chocolate")

V(net)$color <- colrs[V(net)$discipline.type]

# set node size based on authority power size: (provisional)

V(net)$size <- V(net)$authority.size*0.7

# The labels are currently node IDs.

# Setting them to NA will render no labels:

V(net)$label.color <- "black"
V(net)$label <- NA

# Set edge width based on weight:

E(net)$width <- E(net)$weight/6

# Change arrow size and edge color:

E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"

E(net)$width <- 1+E(net)$weight/12

plot(net)

legend(x=-1.5, y=-.5, c("pm", "civil", "building", "mech", "piping", "elec", "inc", "transportation"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

# Let's color the edges of hte graph based on their source node color. We can get the starting node for each edge with the ends() igraph function.

edge.start <- ends(net, es=E(net), names=F)[,1]

edge.col <- V(net)$color[edge.start]

plot(net, edge.color=edge.col, edge.curved=.1, vertex.label=V(net)$name, vertex.label.color="black",
     vertex.label.cex=.7)

legend(x=-1.5, y=-.5, c("pm", "civil", "building", "mech", "piping", "elec", "inc", "transportation"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

# 4. Network and node descriptives
# 4.1 Density
# The proportion of present edges from all possible edges in the network.

edge_density(net, loops=F)

ecount(net) / (vcount(net)*(vcount(net)-1)) #for a directed network

# 4.2 Reciprocity
# The proportion of reciprocated ties (for a directed network)
reciprocity(net)

dyad_census(net) # mutual, asymmetric, and nyll node paris

# 4.3 Transitivity
# global - ratio of triangles (direction disregarded) to connected triples.
# local - ratio of triangles to connected triples each vertex is part of.

transitivity(net, type="global") # net is treated as an undirected network

transitivity(as.undirected(net, mode="collapse")) # same as above

transitivity(net, type="local")

triad_census(net) # for directed networks

# 4.4 Diameter
# A network diameter is the longest geodesic distance (length of the shortest path between two nodes) in the network.
# In igraph, diameter() returns the distance, while get_diameter() returns the nodes along the first found path of that distance.

# Note that edge weights are used by default, unless set to NA.

diameter(net, directed=F, weights=NA)

diameter(net, directed=F)

diam <- get_diameter(net, directed=T)

diam

# Note that get_diameter() returns a vertex sequence. Note though that when asked to behaved as a vector, a vertex sequence will produce the numeric indexes of the nodes in it. The same applies for edge sequences

class(diam)

as.vector(diam)

# colour nodes along the diameter:

vcol <- rep("gray40", vcount(net))
vcol[diam] <- "gold"

ecol <- rep("gray80", ecount(net))

ecol[E(net, path=diam)] <- "orange"

# E(net, path=diam) finds edges along a path, here 'diam'

plot(net, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)

# 4.5 Node degrees
# The function degree() has a mode of in for in-degree, out for out-degree, and all or total for total degree

deg <-degree(net, mode="all")

plot(net, vertex.size=deg*3)

hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree")

# 4.6 Degree distribution

deg.dist <- degree_distribution(net, cumulative=T, mode="all")

plot(x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange")

# 4.7 Centrality & centralization

# Centrality functions (vertex level) and centralization functions (graph level). The centralization functions return res - vertex centrality, centralization, and theoretical_max - maximum centralization score for a graph of that size. The centrality function can run on a subset of nodes (set with the vids parameter).
# This is helpful for large graphs where calculating all centralities may be a resource-intensive and time-consuming task.

# Degree (number of ties)
degree(net, mode="in")
centr_degree(net, mode="in", normalized=T)

# Closeness (centrality based on distance to others in the graph)
# Inverse of the node's average geodesic distance to others in the network.

closeness(net, mode="all", weights=NA)

centr_clo(net, mode="all", normalized=T)

# Eigenvector (centrality proportional to the sum of connection centralities): Values of the first eigenvector of the graph matrix.

eigen_centrality(net, directed=T, weight=NA)

centr_eigen(net, directed=T, normalized = T)

# Betweenness (centrality based on a broker position connecting others): Number of geodesics that pass through the node or the edge

betweenness(net, directed=T, weights=NA)

edge_betweenness(net, directed=T, weights=NA)

centr_betw(net, directed=T, normalized=T)

# 4.8 Hubs and authorities

# The hubs and authorities algorithm developed by Jon Kleinberg was initially used to examine web pages. Hubs were expected to contain catalogs with a large number of outgoing links; while authorities would get many incoming links from hubs, presumably because of their high-quality relevant information.

hs <- hub_score(net, weights=NA)$vector
as <- authority_score(net, weights=NA)$vector

par(mfrow=c(1,2))
plot(net, vertex.size=hs*50, main="Hubs")
plot(net, vertex.size=as*30, main="Authorities")

# 5. Distances and paths

# Average path length: the mean of the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

mean_distance(net, directed=F)

mean_distance(net, directed=T)

# We can also find the length of all shortest paths in the graph:

distances(net) # with edge weights

distances(net, weights=NA) # ignore weights

# We can extract the distances to a node or set of nodes we are interested in. Here we will get the distance of every media from the "Milo"

dist.from.Milo <- distances(net, v=V(net)[name=="Milo"], to=V(net), weights=NA)

# Set colors to plot the distances:

oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.Milo)+1)
col <- col[dist.from.Milo+1]

plot(net, vertex.color=col, vertex.label=dist.from.Milo, edge.arrow.size=.6, vertex.label.color="white")

# We can also find the shortest path between specific nodes. Say here between "Milo" and "Luna"

news.path <- shortest_paths(net, from = V(net)[name=="Milo"], to = V(net)[name="Luna"], output = "both") # both path nodes and edges

# Generate edge colour variable to plot the path:

ecol <- rep("gray50", ecount(net))
ecol[unlist(news.path$epath)] <- "orange"

# Generate edge width variable to plot the path:

ew <- rep(2, ecount(net))
ew[unlist(news.path$epath)] <- 4

# Generate node color variable to plot the path:

vcol <- rep("gray40", vcount(net))
vcol[unlist(news.path$vpath)] <- "gold"

plot(net, vertex.color=vcol, edge.color=ecol, edge.width=ew, edge.arrow.mode=0)

# Identify the edges going into or out of a vertex, for instance "Levi". For a single node, use incident(), for multiple nodes use incident_edges()

inc.edges <- incident(net, V(net)[name=="Levi"], mode="all")

# Set colors to plot the selected edges.

ecol <- rep("gray80", ecount(net))

ecol[inc.edges] <- "orange"

vcol <- rep("grey40", vcount(net))

vcol[V(net)$name=="Levi"] <- "gold"

plot(net, vertex.color=vcol, edge.color=ecol)

# We can also easily identify the immediate neighbors of a vertex, say Levi. The neighbors function finds all nodes one step out from the focal actor.
# To find the neighbors for multiple nodes, use adjacent_vertices() instead of neighbors().
# To find node neighborhoods going more than one step out, use function ego() with parameter order set to the number of steps out to go from the focal node(s).

neigh.nodes <- neighbors(net, V(net)[name=="Levi"], mode="out")

# Set colors to plot the neighbors:
vcol[neigh.nodes] <- "#ff9d00"

plot(net, vertex.color=vcol)

# Special operators for the indexing of edge sequences: %-%. %->%, %<-%

# E(network)[X %-% Y] selects edges between vertex sets X and Y, ignoring direction

# E(network)[X %->% Y] selects edges from vertex sets X to vertex set Y

# E(network)[Y %->% X] selects edges from vertex sets Y to vertex set X

# For example, select edges from transportation to pm:

E(net)[ V(net)[discipline=="transportation"] %->% V(net)[discipline=="pm"]]



# Co-citation (for a couple of nodes, how many shared nominations they have):
cocitation(net)

# 5. Subgroups and communities

# Before we start, we wil lmake our network undirected. There are several ways to do that conversion:
# We can create an undirected link between any pair of connected nodes (mode="collapse")
# Create undirected link for each directed one in the network, potentially ending up with a multiplex graph (mode="each")
# Create undirected link for each symmetric link in hte graph (mode="mutual").

# In cases when we may have ties A->B and B->A ties collapsed into a single undirected link, we need to specify what to do with their edge attributes using the parameter 'edge.attr.comb' as we did earlier with simplify().
# Here we have said that the 'weight' of the links should be summed , and all ohter edge attributes ignored and dropped.

net.sym <- as.undirected(net, mode="collapse", edge.attr.comb=list(weight="sum", "ignore"))

# 5.1 Cliques

# Find cliques (complete subgraphs of an undirected graph)

cliques(net.sym) # list of cliques

sapply(cliques(net.sym), length) # clique sizes

largest_cliques(net.sym) # cliques with max number of nodes

vcol <- rep("grey80", vcount(net.sym))

vcol[unlist(largest_cliques(net.sym))] <- "gold"

plot(as.undirected(net.sym), vertex.label=V(net.sym)$name, vertex.color=vcol)

# 5.2 Community detection
# A number of algorithms aim to detect groups that consist of densely connected nodes with fewer connections across groups.

# Community detection based on edge betweenness (Newman-Girvan)

# High-betweenness edges are removed sequentially (recalculating at each step)and the best partitioning of the network is selected.

ceb <- cluster_edge_betweenness(net)

dendPlot(ceb, mode="hclust")

plot(ceb, net)

# Let's examine the community detection igraph object:
class(ceb)

length(ceb) # number of communities

membership(ceb) # community membership for each node

modularity(ceb) # how modular the graph partitioning is

crossing(ceb, net) # boolean vector: TRUE for edges across communities

# High modularity for a partitioning reflects dense connections within communities and sparse connections across communities.

# Community detection based on propagating labels

# Assigns node labels, randomizes, than replaces each vertex's label with the label that appears most frequently among neighbors. Those steps are repeated until each vertex has the most common label of its neighbors.

clp <- cluster_label_prop(net)

# Community detection based on greedy optimization of modularity

cfg <- cluster_fast_greedy(as.undirected(net))
plot(cfg, as.undirected(net))

# We can also plot the communities without relying on their built-in plot:

V(net)$community <- cfg$membership

colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)

plot(net, vertex.color=colrs[V(net)$community])

# 5.3 K-core decomposition

# The k-core is the maximal subgraph in which every node has degree of at least k. The result here gives the coreness of each vertex in the network. A node has coreness of each vertes in the network.

# A node has coreness D if it belongs to a D-core but not to (D+1)-core.

kc <- coreness(net, mode="all")

plot(net, vertex.size=kc*6, vertex.label=kc, vertex.color=colrs[kc])

# 6. Assortativity and Homophily

# Homophily: the tendency of nodes to connec to others who are similar on some variable.

# assortativity_nominal() is for categorical variables (labels)
# assortativity() is for ordinal and above variables
# assortativity_degree() checks assortativity in node degrees

assortativity_nominal(net, V(net)$discipline.type, directed=F)
assortativity(net, V(net)$authority.size, directed=F)
assortativity_degree(net, directed=F)