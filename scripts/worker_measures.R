### CrowdTruth worker metrics ###

worker_measures <- function(crowd_data, sent_vec, relations) {
  
  print("Computing WORKER metrics...")
  
  crowd_data <- crowd_data[order(crowd_data$X_worker_id, crowd_data$sent_id), ]
  
  workers <- unique(crowd_data$X_worker_id)
  
  work_mes <- data.frame(worker_id = workers,
                         stringsAsFactors = F)
  work_mes$sents <- rep(0, length(workers))
  work_mes$work_agr <- rep(0, length(workers))
  work_mes$sent_agr <- rep(0, length(workers))
  work_mes$rels_per_sent <- rep(0, length(workers))
  work_mes$spam <- rep(F, length(workers))
  
  work_vec <- data.frame(worker_id = crowd_data$X_worker_id,
                         sent_id = crowd_data$sent_id,
                         stringsAsFactors = F)
  for (idx in 1:length(relations)) {
    work_vec[, relations[idx]] <- rep(0, length(work_vec[, 1]))
  }
  
  raw_idx <- 0
  
  for (idx in 1:length(workers)) {
    worker <- workers[idx]
    
    work_sample <- crowd_data[crowd_data$X_worker_id %in% c(worker), ]
    work_mes$sents[idx] <- length(work_sample[, 1])
    
    work_sent_cos <- rep(0, length(work_sample[, 1]))
    work_rels_per_sent <- rep(0, length(work_sample[, 1]))
    for (jdx in 1:length(work_sample[, 1])) {
      sent_id <- work_sample$sent_id[jdx]
      sent_vector <- sent_vec[sent_vec$sent_id %in% c(sent_id), relations]
      
      work_vector <- (relations %in% tolower(strsplit(toString(work_sample$rellist[jdx]), "\n")[[1]])) * 1
      
      sent_vector <- sent_vector - work_vector
      work_sent_cos[jdx] <- cosine(c(sent_vector, recursive = T), work_vector)[[1]]
      work_rels_per_sent[jdx] <- length(strsplit(toString(work_sample$rellist[jdx]), "\n")[[1]])
      
      work_vec[raw_idx + jdx, relations] <- work_vector
    }
    
    raw_idx <- raw_idx + length(work_sample[, 1])
    work_mes$sent_agr[idx] <- mean(work_sent_cos)
    work_mes$rels_per_sent[idx] <- mean(work_rels_per_sent)
  }
  
  work_vec_list <- list()
  for (idx in 1:length(workers)) {
    work_vec_list[[idx]] <- work_vec[work_vec$worker_id %in% c(workers[idx]), ]
  }
  
  
  w_pairs <- 0
  # w_pairs <- length(workers) * (length(workers) - 1) / 2
  
  ww_agr <- data.frame()
  for (idx in 1:(length(workers) - 1)) {
    for (jdx in (idx + 1):length(workers)) {
      common_s <- length(intersect(unique(work_vec[work_vec$worker_id %in% c(workers[idx]), ]$sent_id),
                                   unique(work_vec[work_vec$worker_id %in% c(workers[jdx]), ]$sent_id)))
      if (common_s > 0) {
        w_pairs <- w_pairs + 1
        ww_agr <- rbind(ww_agr,
                        data.frame(worker1 = idx,
                                   worker2 = jdx,
                                   common_sents = c(common_s),
                                   common_rels = c(0),
                                   total_rels = c(0),
                                   agr = c(0),
                                   stringsAsFactors = F))
      }
    }
  }
  
  for (idx in 1:length(ww_agr[, 1])) {
    
    w1_id <- ww_agr$worker1[idx]
    w2_id <- ww_agr$worker2[idx]
    
    common_sents <- intersect(unique(work_vec[work_vec$worker_id %in% c(workers[w1_id]), ]$sent_id),
                              unique(work_vec[work_vec$worker_id %in% c(workers[w2_id]), ]$sent_id))
    
    w1_sample <- work_vec_list[[w1_id]][work_vec_list[[w1_id]]$sent_id %in% common_sents, ]
    w2_sample <- work_vec_list[[w2_id]][work_vec_list[[w2_id]]$sent_id %in% common_sents, ]
    
    for (i in 1:length(common_sents)) {
      for (j in 1:length(relations)) {
        ww_agr$total_rels[idx] <- ww_agr$total_rels[idx] + 1 * (w1_sample[i, relations[j]] || w2_sample[i, relations[j]])
        ww_agr$common_rels[idx] <- ww_agr$common_rels[idx] + 1 * (w1_sample[i, relations[j]] && w2_sample[i, relations[j]])
      }
    }
    
    ww_agr$agr[idx] <- ww_agr$common_rels[idx] / ww_agr$total_rels[idx]
  }
  
  for (idx in 1:length(workers)) {
    agr_sample <- rbind(ww_agr[ww_agr$worker1 %in% c(idx), ],
                        ww_agr[ww_agr$worker2 %in% c(idx), ])
    work_mes$work_agr[idx] <- mean(agr_sample$agr)
  }
  
  work_mes <- work_mes[order(work_mes$sent_agr), ]
  
  for (idx in 1:length(workers)) {
    if (work_mes$sents[idx] == 1 ||
        work_mes$sent_agr[idx] < mean(work_mes$sent_agr) - sd(work_mes$sent_agr) ||
        work_mes$work_agr[idx] < mean(work_mes$work_agr) - sd(work_mes$work_agr)) {
      work_mes$spam[idx] <- T
    }
  }
  
  print(paste(length(work_mes[work_mes$spam %in% c(T), ]$spam),
              "/",
              length(workers),
              "SPAM workers found"))
  
  return(work_mes)
}
