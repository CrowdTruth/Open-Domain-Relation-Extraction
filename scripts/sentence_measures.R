###### CrowdTruth sentence measures ######

sentence_measures <- function(crowd_data, baseline_data, relations) {
  
  print("Computing SENTENCE metrics...")
  
  sent_vec <- data.frame(sent_id = unique(crowd_data$sent_id),
                         stringsAsFactors=FALSE)
  
  # for (idx in 1:length(relations)) {
  #   sent_vec[, c(relations[idx])] <- rep(0, length(sent_vec[, 1]))
  #   sent_vec[, c(paste(relations[idx], "cos", sep = "_"))] <- rep(0, length(sent_vec[, 1]))
  # }
  
  sent_vec[, relations] <- rep(0, length(sent_vec[, 1]))
  sent_vec[, paste(relations, "cos", sep = "_")] <- rep(0, length(sent_vec[, 1]))
  
  sent_vec$workers <- rep(0, length(sent_vec$sent_id))
  sent_vec$max_cos <- rep(0, length(sent_vec$sent_id))
  sent_vec$max_rel <- rep("", length(sent_vec$sent_id))
  sent_vec$seed_relation <- rep("", length(sent_vec$sent_id))
  sent_vec$seed_relation_cos <- rep(-1, length(sent_vec$sent_id))
  
  sent_vec$term1 <- rep("", length(sent_vec$sent_id))
  sent_vec$b1 <- rep("", length(sent_vec$sent_id))
  sent_vec$e1 <- rep("", length(sent_vec$sent_id))
  sent_vec$term2 <- rep("", length(sent_vec$sent_id))
  sent_vec$b2 <- rep("", length(sent_vec$sent_id))
  sent_vec$e2 <- rep("", length(sent_vec$sent_id))
  sent_vec$sentence <- rep("", length(sent_vec$sent_id))
  
  for (idx in 1:length(sent_vec[, 1])) {
    sample <- crowd_data[crowd_data$sent_id %in% c(sent_vec$sent_id[idx]), ]
    sent_vec$term1[idx] <- sample$term1[1]
    sent_vec$b1[idx] <- sample$b1[1]
    sent_vec$e1[idx] <- sample$e1[1]
    sent_vec$term2[idx] <- sample$term2[1]
    sent_vec$b2[idx] <- sample$b2[1]
    sent_vec$e2[idx] <- sample$e2[1]
    sent_vec$sentence[idx] <- sample$sentence[1]
  }
  
  for (idx in 1:length(crowd_data[, 1])) {
    sent_id <- crowd_data$sent_id[idx]
    sent_id_pos <- match(sent_id, sent_vec$sent_id)
    sent_vec$workers[sent_id_pos] <- sent_vec$workers[sent_id_pos] + 1
    
    str <- toString(crowd_data$rellist[idx])
    worker_relations <- gsub("\\]", "", gsub("\\[", "", tolower(strsplit(str, "\n")[[1]])))
    
    for (jdx in 1:length(worker_relations)) {
      sent_vec[sent_id_pos, c(worker_relations[jdx])] <- sent_vec[sent_id_pos, c(worker_relations[jdx])] + 1
    }
  }
  
  for (idx in 1:length(sent_vec[, 1])) {
    vec <- c(sent_vec[idx, relations], recursive = T)
    
    for (jdx in 1:length(relations)) {
      unit_vec <- c()
      if (jdx == 1) {
        unit_vec <- c(1, rep(0, length(relations) - 1))
      }
      else if (jdx == length(relations)) {
        unit_vec <- c(rep(0, length(relations) - 1), 1)
      }
      else {
        unit_vec <- c(rep(0, jdx - 1), 1, rep(0, length(relations) - jdx))
      }
      
      sent_vec[idx, paste(relations[jdx], "cos", sep = "_")] <- cosine(vec, unit_vec)[[1]]
    }
    
    sent_vec$max_cos[idx] <- max(sent_vec[idx, paste(relations, "cos", sep = "_")])
    sent_vec$max_rel[idx] <- relations[which.max(sent_vec[idx, paste(relations, "cos", sep = "_")])]
    if (which.max(sent_vec[idx, paste(relations, "cos", sep = "_")]) < length(relations)) {
      for (jdx in (which.max(sent_vec[idx, paste(relations, "cos", sep = "_")]) + 1):length(relations)) {
        
        if (sent_vec[idx, paste(relations[jdx], "cos", sep = "_")] == sent_vec$max_cos[idx]) {
          sent_vec$max_rel[idx] <- paste(sent_vec$max_rel[idx], relations[jdx], sep = ",")
        }
      }
    }
  }
  
  for (idx in 1:length(sent_vec[, 1])) {
    sample <- baseline_data[baseline_data$sent_id %in% c(sent_vec$sent_id[idx]), ]
    sent_vec$seed_relation[idx] <- paste(sample$relation, collapse = ",")
    
    if (!grepl(",", sent_vec$seed_relation[idx]))
      sent_vec$seed_relation_cos[idx] <- sent_vec[idx, paste(sent_vec$seed_relation[idx], "cos", sep = "_")]
  }

  return(sent_vec)
    
#   sent_vec_pos <- sent_vec[grepl("POS", sent_vec$sent_id), ]
#   sent_vec_neg <- sent_vec[grepl("NEG", sent_vec$sent_id), ]
#   
#   
}
