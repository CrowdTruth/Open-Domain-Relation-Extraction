require(lsa)
source("read_data.R")
source("sentence_measures.R")
source("sentence_measures_v2.R")
source("worker_measures.R")
source("relation_measures.R")

# sent_vec <- sentence_measures(crowd_data, baseline_data, relations)
sent_vec <- ratio_sentence_measures(crowd_data, baseline_data, relations)
# sent_vec <- extended_cos_sentence_measures(crowd_data, baseline_data, relations)

work_vec <- worker_measures(crowd_data, sent_vec, relations)

# remove spammers
spam <- work_vec[work_vec$spam %in% c(T), ]$worker_id
crowd_data <- crowd_data[!crowd_data$X_worker_id %in% spam, ]

# sent_vec <- sentence_measures(crowd_data, baseline_data, relations)
sent_vec <- ratio_sentence_measures(crowd_data, baseline_data, relations)
sent_vec$clarity <- sentence_clarity(crowd_data, baseline_data, relations)
# sent_vec <- extended_cos_sentence_measures(crowd_data, baseline_data, relations)

sent_vec <- sent_vec[, c(1:44, 52, 45:51)]
sent_vec <- sent_vec[order(sent_vec$sent_id), ]



caus_pow <- causal_power(sent_vec)

# print data
write.csv(sent_vec[grepl("-A-", sent_vec$sent_id), ], output_sent_pos, row.names = F)
write.csv(sent_vec[grepl("-U-", sent_vec$sent_id), ], output_sent_neg, row.names = F)
write.csv(work_vec, output_work, row.names = F)
# 

