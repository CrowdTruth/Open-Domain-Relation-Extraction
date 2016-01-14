input_folder <- "/home/anca/Documents/Open-Domain-Relation-Extraction/data/pilot/baseline2/"
baseline_file <-  "/home/anca/Documents/Open-Domain-Relation-Extraction/data/pilot/test_1.csv"

require(lsa)
source("read_data.R")
source("sentence_measures.R")
source("worker_measures.R")
source("relation_measures.R")

sent_vec <- sentence_measures(crowd_data, baseline_data, relations)
work_vec <- worker_measures(crowd_data, sent_vec, relations)

# remove spammers
spam <- work_vec[work_vec$spam %in% c(T), ]$worker_id
crowd_data <- crowd_data[!crowd_data$X_worker_id %in% spam, ]
sent_vec <- sentence_measures(crowd_data, baseline_data, relations)

caus_pow <- causal_power(sent_vec)

# print data
write.csv(sent_vec[grepl("POS", sent_vec$sent_id), ], output_sent_pos, row.names = F)
write.csv(sent_vec[grepl("NEG", sent_vec$sent_id), ], output_sent_neg, row.names = F)
write.csv(work_vec, output_work, row.names = F)

