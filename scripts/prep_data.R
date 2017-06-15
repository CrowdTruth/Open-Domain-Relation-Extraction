crowd_data <- crowd_data[order(crowd_data$sent_id), ]

work_vectors <- data.frame(
  sentence_id = crowd_data$sent_id,
  worker_id = crowd_data$X_worker_id,
  stringsAsFactors = F
)

sent_vectors <- data.frame(
  sentence_id = unique(crowd_data$sent_id),
  stringsAsFactors = F
)

for (r in relations) {
  sent_vectors[, r] <- 0
  work_vectors[, r] <- 0
}

sent_idx <- 1
for (idx in 1:length(crowd_data[, 1])) {
  if (work_vectors$sentence_id[idx] != sent_vectors$sentence_id[sent_idx]) {
    sent_idx <- sent_idx + 1
  }
  
  rellist <- strsplit(crowd_data$rellist[idx], "\n")[[1]]
  for (r in rellist) {
    r <- tolower(r)
    work_vectors[idx, r] <- work_vectors[idx, r] + 1
    sent_vectors[sent_idx, r] <- sent_vectors[sent_idx, r] + 1
  }
}

work_vectors$term1 <- crowd_data$term1
work_vectors$term2 <- crowd_data$term2
work_vectors$b1 <- crowd_data$b1
work_vectors$b2 <- crowd_data$b2
work_vectors$e1 <- crowd_data$e1
work_vectors$e2 <- crowd_data$e2

# sent_vec <- sent_vec[order(sent_vec$sent_id), ]
# sent_vectors$term1 <- sent_vec$term1
# sent_vectors$term2 <- sent_vec$term2
# sent_vectors$b1 <- sent_vec$b1
# sent_vectors$b2 <- sent_vec$b2
# sent_vectors$e1 <- sent_vec$e1
# sent_vectors$e2 <- sent_vec$e2

write.csv(work_vectors, "../crowdsourcing/output/work_vec.csv", row.names = F)
write.csv(sent_vectors, "../crowdsourcing/output/sent_vec.csv", row.names = F)

rqs <- data.frame(
  relation = relations,
  rqs = 1
)
write.csv(rqs, "../crowdsourcing/output/relations.csv", row.names = F)

