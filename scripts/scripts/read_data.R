source("relations.R")

input_folder <- "/home/anca/Documents/Open-Domain-Relation-Extraction/data/unannotated/output/"

input_file <- list.files(input_folder, pattern = "f[0-9]*.csv")
output_sent_pos <- paste(input_folder, "sent-vec_ambig.csv", sep = "")
output_sent_neg <- paste(input_folder, "sent-vec_unambig.csv", sep = "")
output_work <- paste(input_folder, "work-vec.csv", sep = "")


crowd_data <- data.frame(stringsAsFactors = F)

for (i in 1:length(input_file)) {
  cd <- read.csv(paste(input_folder, input_file[i], sep = ""), header = T, sep = ",",
                 dec = ".", fill = TRUE, stringsAsFactors=FALSE)
  cd$job <- input_file[i]
  if (i > 1) {
    cd <- cd[, colnames(crowd_data)]
  }
  
  print(paste(input_folder, input_file[i], sep = ""))
  crowd_data <- rbind(crowd_data, cd)
}

baseline_data <- read.csv("/home/anca/Documents/Open-Domain-Relation-Extraction/data/unannotated/crowd_input_ambig.csv",
                          stringsAsFactors = F)
baseline_data <- rbind(baseline_data,
                       read.csv("/home/anca/Documents/Open-Domain-Relation-Extraction/data/unannotated/crowd_input_unambig.csv",
                                stringsAsFactors = F))
baseline_data <- baseline_data[,2:length(baseline_data[1, ])]
baseline_data <- baseline_data[baseline_data$Sent_id %in% crowd_data$sent_id, ]
