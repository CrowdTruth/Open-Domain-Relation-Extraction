crowd_folder <- "../crowdsourcing/output/unmerged"
THRESHOLD <- 0.5

# files <- setdiff(list.files(crowd_folder, pattern=".csv"), c("sent-vec.csv", "work-vec.csv"))
# 
# for (idx in 1:length(files)) {
#   file <- files[idx]
#   data <- read.csv(paste(crowd_folder, file, sep = "/"), stringsAsFactors = F)
#   relation <- gsub("\\.", ":", gsub("\\.csv", "", file))
#   for (jdx in 1:length(data[, 1])) {
#     if (data$baseline[jdx] == 0) data$relation[jdx] <- "none"
#     else data$relation[jdx] <- relation
#   }
#   write.csv(data, paste(crowd_folder, "/", relation, ".csv", sep=""), row.names=F)
# }
# 
# 

sent_vec <- read.csv(paste(crowd_folder, "sent-vec.csv", sep = "/"), stringsAsFactors = F)

multiclass_data <- data.frame(
  sent_id = sent_vec$sent_id,
  relation = "",
  term1 = sent_vec$term1,
  b1 = sent_vec$b1,
  e1 = sent_vec$e1,
  term2 = sent_vec$term2,
  b2 = sent_vec$b2,
  e2 = sent_vec$e2,
  sentence = sent_vec$sentence,
  stringsAsFactors = F
)

relations <- setdiff(list.files(crowd_folder, pattern=".csv"), c("sent-vec.csv", "work-vec.csv"))
relations <- gsub("\\.csv", "", relations)

for (idx in 1:length(sent_vec[, 1])) {
  for (r in relations) {
    if (sent_vec[idx, paste(gsub(":", ".", r), "srs", sep = "_")] >= THRESHOLD) {
      if (multiclass_data$relation[idx] == "") multiclass_data$relation[idx] <- r
      else multiclass_data$relation[idx] <- paste(multiclass_data$relation[idx], r, sep = ";")
    }
  }
  if (multiclass_data$relation[idx] == "") {
    multiclass_data$relation[idx] <- "none"
  }
}

write.csv(multiclass_data, paste(crowd_folder, "/Crowd_multiclass_classifier_unmerged.csv", sep = ""), row.names = F)
