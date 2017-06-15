
macro_causal_power <- function(sent_vec) {
  
  print("Computing RELATION metrics...")
  
  cond_prob <- function(data, rel1, rel2) {
    inters <- sum(data[, c(rel1)] >= 0.5 & data[, c(rel2)] >= 0.5)
    prob_rel1 <- sum(data[, c(rel1)] >= 0.5)
    return (inters / prob_rel1)
  }
  
  cond_prob_neg <- function(data, rel1, rel2) {
    inters <- sum(data[, c(rel1)] < 0.5 & data[, c(rel2)] >= 0.5)
    prob_rel1 <- sum(data[, c(rel1)] < 0.5)
    return (inters / prob_rel1)
  }
  
  caus_pow <- function(data, rel1, rel2) {
    return((cond_prob(data, rel1, rel2) - cond_prob_neg(data, rel1, rel2)) /
             (1 - cond_prob_neg(data, rel1, rel2)))
  }
  
  caus_pow_results <- sapply(relations, function(rel1) {
    sapply(relations, function(rel2) {
      caus_pow(sent_vec, paste(rel1, "srs", sep = "_"), paste(rel2, "srs", sep = "_"))
    })
  })
  
  return(caus_pow_results)
}

micro_causal_power <- function(crowd_data) {
  
  print("Computing RELATION metrics...")
  
  crowd_data$rellist <- gsub(":", ".", crowd_data$rellist)
  
  work_data <- data.frame(id = crowd_data$X_id, stringsAsFactors = F)
  for (idx in 1:length(relations)) {
    work_data[, relations[idx]] <- rep(0, length(crowd_data[, 1]))
  }
  
  avg_rels <- rep(0, length(crowd_data[, 1]))
  for (idx in 1:length(crowd_data[, 1])) {
    crowd_rels <- strsplit(crowd_data$rellist[idx], "\n")[[1]]
    for (jdx in 1:length(crowd_rels)) {
      work_data[idx, crowd_rels[jdx]] <- 1
    }
    avg_rels[idx] <- length(crowd_rels)
  }
  
  cond_prob <- function(data, rel1, rel2) {
    inters <- sum(data[, c(rel1)] & data[, c(rel2)])
    prob_rel1 <- sum(data[, c(rel1)])
    return (inters / prob_rel1)
  }
  
  cond_prob_neg <- function(data, rel1, rel2) {
    inters <- sum(data[, c(rel1)] == 0 & data[, c(rel2)])
    prob_rel1 <- sum(data[, c(rel1)] == 0)
    return (inters / prob_rel1)
  }
  
  caus_pow <- function(data, rel1, rel2) {
    return((cond_prob(data, rel1, rel2) - cond_prob_neg(data, rel1, rel2)) /
             (1 - cond_prob_neg(data, rel1, rel2)))
  }
  
  caus_pow_results <- sapply(relations, function(rel1) {
    sapply(relations, function(rel2) {
      caus_pow(work_data, rel1, rel2)
    })
  })
  
  return(caus_pow_results)
}
