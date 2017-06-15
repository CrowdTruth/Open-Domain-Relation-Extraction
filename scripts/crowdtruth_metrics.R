input_folder <- "/home/anca/Documents/Open-Domain-Relation-Extraction/data/crowdsourcing/"
MERGED_RELATIONS <- FALSE

require(lsa)
source("read_data.R")
source("sentence_measures.R")
source("worker_measures.R")
source("relation_measures.R")

# prep data for metrics v.2.0
source("prep_data.R")

sent_vec <- ratio_sentence_measures(crowd_data, baseline_data, relations)
work_vec <- worker_measures(crowd_data, sent_vec, relations)

# remove spammers
# work_vec <- read.csv(output_work, stringsAsFactors = F)
spam <- work_vec[work_vec$spam %in% c(T), ]$worker_id
crowd_data <- crowd_data[!crowd_data$X_worker_id %in% spam, ]
sent_vec <- sentence_measures(crowd_data, baseline_data, relations)
sent_vec$clarity <- sentence_clarity(crowd_data, baseline_data, relations)

# sent_vec <- read.csv("../crowdsourcing/output/sent-vec.csv", stringsAsFactors = F)
# relations <- gsub(".", ":", relations)
# caus_pow <- causal_power(sent_vec)

# remove numeric terms
# sent_vec <- sent_vec[!grepl("[0-9]+", sent_vec$term1), ]
# sent_vec <- sent_vec[!grepl("[0-9]+", sent_vec$term2), ]

# print data
write.csv(sent_vec, output_sent, row.names = F)
write.csv(work_vec, output_work, row.names = F)
write.csv(caus_pow, "../data/crowdsourcing/output/caus-pow.csv")

# test sets for model

for (rel in relations) {
  test_set <- data.frame(
    sent_id = sent_vec$sent_id,
    baseline = 0,
    term1 = sent_vec$term1,
    b1 = sent_vec$b1,
    e1 = sent_vec$e1,
    term2 = sent_vec$term2,
    b2 = sent_vec$b2,
    e2 = sent_vec$e2,
    sentence = sent_vec$sentence,
    stringsAsFactors = F
  )
  
  # rel <- gsub(":", ".", rel)
  test_set$baseline <- (sent_vec[, paste(rel, "_srs", sep = "")] >= 0.5) * 1
  
  if (rel != "none" && rel != "per:age") {
    write.csv(test_set, paste(input_folder, "/output/", rel, ".csv", sep = ""), row.names = F)
  }
}


# FPs per rel

seed_relations <- data.frame(
  sent_id = unique(crowd_data$sent_id),
  seed_relation = "",
  stringsAsFactors = F
)
sr_idx <- 1
for (idx in 1:length(crowd_data$relation)) {
  if (crowd_data$sent_id[idx] != seed_relations$sent_id[sr_idx]) {
    sr_idx <- sr_idx + 1
  }
  if (seed_relations$seed_relation[sr_idx] == "") {
    seed_relations$seed_relation[sr_idx] <- crowd_data$relation[idx]
  }
}

seed_relations <- seed_relations[order(seed_relations$sent_id), ]
sent_vec <- sent_vec[order(sent_vec$sent_id), ]
sent_vec$seed_relation <- seed_relations$seed_relation

fps <- rep(0, length(relations))
ps  <- rep(0, length(relations))
for (idx in 1:length(sent_vec[, 1])) {
  for (jdx in 1:length(relations)) {
    if (relations[jdx] %in% strsplit(sent_vec$seed_relation[idx], ",")[[1]]) {
      ps[jdx] <- ps[jdx] + 1
      if (sent_vec[idx, paste(relations[jdx], "srs", sep = "_")] < 0.5) {
        fps[jdx] <- fps[jdx] + 1
      }
    }
  }
}

fps_data <- data.frame(
  relation = relations,
  FP = fps,
  total = ps,
  ratio = fps / ps,
  stringsAsFactors = F
)
fps_data <- fps_data[!fps_data$relation %in% c("none"), ]
fps_data <- fps_data[order(fps_data$ratio), ]

par(mar=c(15,5,4,2))
barplot(fps_data$ratio, col = "lightblue", ylab = "ratio FP / P", names = fps_data$relation, las = 2)
