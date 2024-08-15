# Load necessary library
library(vegan)

# Example data (using the built-in 'varespec' dataset)
data(varespec)

# Perform NMDS
nmds <- metaMDS(varespec, k = 2)  # k = 2 for 2D NMDS

# Extract NMDS coordinates
nmds_coords <- scores(nmds)

# Choose the specific point (e.g., the first point)
point_of_interest <- nmds_coords$species[1, ]

# Calculate Euclidean distances from the point of interest to all other points
distances <- apply(nmds_coords$species, 1, function(x) sqrt(sum((x - point_of_interest)^2)))

# Order points by distance to find nearest neighbors
nearest_neighbors <- order(distances)

# View the indices of the nearest neighbors
nearest_neighbors

# For example, to get the 5 nearest neighbors:
nearest_neighbors[1:5]
