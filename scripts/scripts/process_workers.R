source("read_data.R")

sent_vec <- read.csv("../data/unannotated/output/sent-vec_unambig.csv", stringsAsFactors = F)
sent_vec <- rbind(sent_vec,
                  read.csv("../data/unannotated/output/sent-vec_ambig.csv", stringsAsFactors = F))

work_vec <- read.csv("../data/unannotated/output/work-vec.csv", stringsAsFactors = F)

work_vec$jobs <- ""
for (idx in 1:length(work_vec[, 1])) {
  aux <- crowd_data[crowd_data$X_worker_id %in% c(work_vec$worker_id[idx]), ]
  work_vec$jobs[idx] <- paste(unique(aux$job), collapse = ",")
}
