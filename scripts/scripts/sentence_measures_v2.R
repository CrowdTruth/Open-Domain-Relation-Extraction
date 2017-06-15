
### cosine computed with all possible work vector combinations

sentence_clarity <- function(crowd_data, baseline_data, relations) {
  sent_vec <- sentence_vector(crowd_data, baseline_data, relations)
  sent_vec$clarity <- -1
  
  
  for (idx in 1:length(sent_vec[, 1])) {
    vec <- c(sent_vec[idx, relations], recursive = T)
    sample <- crowd_data[crowd_data$sent_id %in% c(sent_vec$sent_id[idx]), ]
    
    clarity_vec <- c()
    
    for (rel in 1:length(relations)) {
      
      max_vec_val <- 0
      
      for (jdx in 1:length(sample[, 1])) {
        unit_vec <- relations %in% tolower(strsplit(sample$relations[jdx], " ")[[1]]) * 1
        if (relations[rel] %in% relations[unit_vec * 1:length(relations)]) {
          aux_vec <- vec - unit_vec
          aux <- cosine(aux_vec, unit_vec)[[1]]
          max_vec_val <- max(max_vec_val, aux)
          
          clarity_vec <- append(clarity_vec, aux)
        }
      }
      
      sent_vec[idx, paste(relations[rel], "srs", sep = "_")] <- max_vec_val
    }
    
    sent_vec$clarity[idx] <- mean(clarity_vec)
    
    sent_vec$max_srs[idx] <- max(sent_vec[idx, paste(relations, "srs", sep = "_")])
    sent_vec$max_rel[idx] <- relations[which.max(sent_vec[idx, paste(relations, "srs", sep = "_")])]
    if (which.max(sent_vec[idx, paste(relations, "srs", sep = "_")]) < length(relations)) {
      for (jdx in (which.max(sent_vec[idx, paste(relations, "srs", sep = "_")]) + 1):length(relations)) {
        
        if (sent_vec[idx, paste(relations[jdx], "srs", sep = "_")] == sent_vec$max_srs[idx]) {
          sent_vec$max_rel[idx] <- paste(sent_vec$max_rel[idx], relations[jdx], sep = ",")
        }
      }
    }
  }
  
  for (idx in 1:length(sent_vec[, 1])) {
    sample <- baseline_data[baseline_data$Sent_id %in% c(sent_vec$sent_id[idx]), ]
    sent_vec$seed_relation[idx] <- paste(sample$relation, collapse = ",")
    
    rels <- strsplit(sent_vec$seed_relation[idx], ",")[[1]]
    sent_vec$seed_relation_srs[idx] <- -1
    for (i in 1:length(rels))
      sent_vec$seed_relation_srs[idx] <- max(sent_vec$seed_relation_srs[idx],
                                             sent_vec[idx, paste(rels[i], "srs", sep = "_")])
  }
  
  return(sent_vec$clarity)
}

extended_cos_sentence_measures <- function(crowd_data, baseline_data, relations) {
    
  sent_vec <- sentence_vector(crowd_data, baseline_data, relations)
  sent_vec$clarity <- -1
  
  
  for (idx in 1:length(sent_vec[, 1])) {
    vec <- c(sent_vec[idx, relations], recursive = T)
    sample <- crowd_data[crowd_data$sent_id %in% c(sent_vec$sent_id[idx]), ]
    
    clarity_vec <- c()
    
    for (rel in 1:length(relations)) {
    
      max_vec_val <- 0
      
      for (jdx in 1:length(sample[, 1])) {
        unit_vec <- relations %in% tolower(strsplit(sample$relations[jdx], " ")[[1]]) * 1
        if (relations[rel] %in% relations[unit_vec * 1:length(relations)]) {
          aux_vec <- vec - unit_vec
          aux <- cosine(aux_vec, unit_vec)[[1]]
          max_vec_val <- max(max_vec_val, aux)
          
          clarity_vec <- append(clarity_vec, aux)
        }
      }
      
      sent_vec[idx, paste(relations[rel], "srs", sep = "_")] <- max_vec_val
    }
    
    sent_vec$clarity[idx] <- mean(clarity_vec)
    
    sent_vec$max_srs[idx] <- max(sent_vec[idx, paste(relations, "srs", sep = "_")])
    sent_vec$max_rel[idx] <- relations[which.max(sent_vec[idx, paste(relations, "srs", sep = "_")])]
    if (which.max(sent_vec[idx, paste(relations, "srs", sep = "_")]) < length(relations)) {
      for (jdx in (which.max(sent_vec[idx, paste(relations, "srs", sep = "_")]) + 1):length(relations)) {
        
        if (sent_vec[idx, paste(relations[jdx], "srs", sep = "_")] == sent_vec$max_srs[idx]) {
          sent_vec$max_rel[idx] <- paste(sent_vec$max_rel[idx], relations[jdx], sep = ",")
        }
      }
    }
  }
  
  for (idx in 1:length(sent_vec[, 1])) {
    sample <- baseline_data[baseline_data$Sent_id %in% c(sent_vec$sent_id[idx]), ]
    sent_vec$seed_relation[idx] <- paste(sample$relation, collapse = ",")
    
    rels <- strsplit(sent_vec$seed_relation[idx], ",")[[1]]
    sent_vec$seed_relation_srs[idx] <- -1
    for (i in 1:length(rels))
      sent_vec$seed_relation_srs[idx] <- max(sent_vec$seed_relation_srs[idx],
                                               sent_vec[idx, paste(rels[i], "srs", sep = "_")])
  }
  
  return(sent_vec)

}



### sentence vector as ratio of selected / total

ratio_sentence_measures <- function(crowd_data, baseline_data, relations) {

  sent_vec <- sentence_vector(crowd_data, baseline_data, relations)
  
  for (idx in 1:length(sent_vec[, 1])) {
    vec <- c(sent_vec[idx, relations], recursive = T)
    sent_vec[idx, paste(relations, "srs", sep = "_")] <- vec / sent_vec$workers[idx]
    
    sent_vec$max_srs[idx] <- max(sent_vec[idx, paste(relations, "srs", sep = "_")])
    sent_vec$max_rel[idx] <- relations[which.max(sent_vec[idx, paste(relations, "srs", sep = "_")])]
    if (which.max(sent_vec[idx, paste(relations, "srs", sep = "_")]) < length(relations)) {
      for (jdx in (which.max(sent_vec[idx, paste(relations, "srs", sep = "_")]) + 1):length(relations)) {
        
        if (sent_vec[idx, paste(relations[jdx], "srs", sep = "_")] == sent_vec$max_srs[idx]) {
          sent_vec$max_rel[idx] <- paste(sent_vec$max_rel[idx], relations[jdx], sep = ",")
        }
      }
    }
  }
  
  for (idx in 1:length(sent_vec[, 1])) {
    sample <- baseline_data[baseline_data$Sent_id %in% c(sent_vec$sent_id[idx]), ]
    sent_vec$seed_relation[idx] <- paste(sample$relation, collapse = ",")
    
    rels <- strsplit(sent_vec$seed_relation[idx], ",")[[1]]
    sent_vec$seed_relation_srs[idx] <- -1
    for (i in 1:length(rels))
      sent_vec$seed_relation_srs[idx] <- max(sent_vec$seed_relation_srs[idx],
                                             sent_vec[idx, paste(rels[i], "srs", sep = "_")])
  }
  
  return(sent_vec)
}
