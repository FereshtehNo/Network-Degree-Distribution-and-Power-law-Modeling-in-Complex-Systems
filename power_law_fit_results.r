# Load the required libraries
library(igraph)
library(poweRlaw)

# Read in the csv file
df <- read.csv("string_interactions_selected.csv", header=TRUE, stringsAsFactors=FALSE)

# Create a network object from the data frame
g <- graph_from_data_frame(df[,c("node1", "node2")], directed=FALSE)

# Create an adjacency matrix
adj_matrix <- as_adjacency_matrix(g)

# Calculate the degree distribution
degree_dist <- apply(adj_matrix, 1, sum)

# Sort the degree distribution in descending order
degree_dist <- sort(degree_dist, decreasing=TRUE)

# Fit the power-law distribution to the degree distribution
fit <- fit_power_law(degree_dist)

# Extract the estimated power-law exponent
gamma <- fit$alpha

# Save the results to a file
results <- data.frame(x=seq_along(degree_dist), y=degree_dist, fit=gamma)

# Add the estimated power-law exponent to the results data frame
results$gamma <- gamma

# Save the results to a file
write.csv(results, "power_law_fit_results.csv", row.names=FALSE)

# Plot the degree distribution and power-law fit
plot(degree_dist, log="xy", pch=20, col="blue", xlab="Degree", ylab="Frequency")
lines(degree(g), (length(degree_dist) / sum(degree_dist)) * degree(g)^(-gamma+1), col="red")
legend("topleft", legend=paste0("gamma = ", round(gamma, 2)), col="red", lty=1, bty="n")