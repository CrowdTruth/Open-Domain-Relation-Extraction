source("relations.R")

input_file <- list.files(input_folder, pattern = "f[0-9]*.csv")[1]
# input_file <- "baseline3.csv"
output_sent_pos <- paste(input_folder, "sent-vec_pos.csv", sep = "")
output_sent_neg <- paste(input_folder, "sent-vec_neg.csv", sep = "")
output_work <- paste(input_folder, "work-vec.csv", sep = "")


crowd_data <- read.csv(paste(input_folder, input_file, sep = ""), header = T, sep = ",",
                       dec = ".", fill = TRUE, stringsAsFactors=FALSE)

baseline_data <- read.csv(baseline_file, header = T, sep = ",",
                          dec = ".", fill = TRUE, stringsAsFactors=FALSE)