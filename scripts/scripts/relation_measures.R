causal_power <- function(sent_vec) {
  
  print("Computing RELATION metrics...")
  
  cond_prob <- function(data, rel1, rel2) {
    inters <- sum(data[, c(rel1)] > 0 & data[, c(rel2)] > 0)
    prob_rel1 <- sum(data[, c(rel1)] > 0)
    return (inters / prob_rel1)
  }
  
  cond_prob_neg <- function(data, rel1, rel2) {
    inters <- sum(data[, c(rel1)] == 0 & data[, c(rel2)] > 0)
    prob_rel1 <- sum(data[, c(rel1)] == 0)
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


seed_causal_power <- function(sent_vec) {
  
  cond_prob <- function(data, rel1, rel2) {
    inters <- sum(grepl(rel1, data$seed_relation) * 1 > 0 & data[, c(rel2)] > 0)
    prob_rel1 <- sum(grepl(rel1, data$seed_relation) * 1 > 0)
    return (inters / prob_rel1)
  }
  
  cond_prob_neg <- function(data, rel1, rel2) {
    inters <- sum(grepl(rel1, data$seed_relation) * 1 == 0 & data[, c(rel2)] > 0)
    prob_rel1 <- sum(grepl(rel1, data$seed_relation) * 1 == 0)
    return (inters / prob_rel1)
  }
  
  caus_pow <- function(data, rel1, rel2) {
    return((cond_prob(data, rel1, rel2) - cond_prob_neg(data, rel1, rel2)) /
             (1 - cond_prob_neg(data, rel1, rel2)))
  }
  
  caus_pow_results <- sapply(relations, function(rel1) {
    sapply(relations, function(rel2) {
      caus_pow(sent_vec, rel1, paste(rel2, "srs", sep = "_"))
    })
  })
  
  return(caus_pow_results)
}