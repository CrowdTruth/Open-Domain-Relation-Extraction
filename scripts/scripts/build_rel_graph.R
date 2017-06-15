library(igraph)

rel_counts <- read.csv("/home/anca/Documents/relationsentences/dbo.csv",
                       sep = ",", quote = "\"", dec = ".", fill = TRUE, stringsAsFactors=FALSE)

input_path <- "/home/anca/Documents/relationsentences/rels_tp"
files <- list.files(input_path, pattern = "csv")


graph <- data.frame()

for (i in 1:(length(files) - 1)) {
  file_i <- paste(input_path, files[i], sep = "/")
  tps_i <- read.csv(file_i, header = F, quote = "\t\t\t\t", stringsAsFactors=FALSE)
  
  print(paste("PROCESSING", file_i))
  
  for (j in (i + 1):length(files)) {
    file_j <- paste(input_path, files[j], sep = "/")
    tps_j <- read.csv(file_j, header = F, quote = "\t\t\t\t", stringsAsFactors=FALSE)
    
    weight <- length(intersect(tps_i$V1, tps_j$V1))
    
    if (weight > 0) {
      graph <- rbind(graph,
                     data.frame(idx1 = c(substring(files[i], 1, nchar(files[i]) - 4)),
                                idx2 = c(substring(files[j], 1, nchar(files[j]) - 4)),
                                weight = c(weight),
                                stringsAsFactors=FALSE))
    }

  }
}

ig <- graph_from_data_frame(graph[, c(1,2)], directed=F)
E(ig)$weight <- graph[, 3]

fgreedy <- fastgreedy.community(ig, merges=TRUE, modularity=TRUE, weights = E(ig)$weight)

membership(fgreedy)
sizes(fgreedy)

rel_counts <- rel_counts[rel_counts$rel %in% V(ig)$name, ]
rel_counts <- rel_counts[order(rel_counts$rel), ]
rel_counts$community <- as.numeric(membership(fgreedy))

# write.csv(rel_counts, "/home/anca/Documents/relationsentences/out/rel_counts.csv", row.names=FALSE)
write.csv(graph, "/home/anca/Documents/relationsentences/out/rel_graph.csv", row.names=FALSE)