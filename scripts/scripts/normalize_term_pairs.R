input_path <- "/home/anca/Documents/relationsentences/rels_tp"

files <- list.files(input_path, pattern = "csv")

for (i in 1:length(files)) {
  file_f <- paste(input_path, files[i], sep = "/")
  print(paste("PROCESSING", file_f))
  
  tps <- read.csv(file_f, header = F, quote = "\t\t\t\t", stringsAsFactors=FALSE)
  tps$V1 <- tolower(tps$V1)
  
  for (j in 1:length(tps[, 1])) {
    tps_arr <- tolower(strsplit(tps[j, ], " \\+\\+\\+ ")[[1]])
    
    if (length(tps_arr) > 1 && tps_arr[1] > tps_arr[2]) {
      tps[j, ] <- paste(tps_arr[2], tps_arr[1], sep = " +++ ")
    }
  }
  
  write.table(file=file_f, tps, sep="\t", col.names=FALSE, quote=FALSE, row.names=FALSE)
}