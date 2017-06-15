input_path <- "/home/anca/Documents/relationsentences"

# source("twrex_dict.R")

rel_counts <- read.csv(paste(input_path, "dbo.csv", sep = "/"),
                       sep = ",", quote = "\"", dec = ".", fill = TRUE, stringsAsFactors=FALSE)

rel_counts <- rel_counts[rel_counts$count >= 800, ]

# for (i in 0:899) {
for (i in 188:899) {
  file_name <- paste(input_path,
                     paste("allwiki.txt_", paste(i, ".sentences.rel", sep = ""), sep = ""),
                     sep = "/")
  
  print(paste("PROCESSING", file_name))
  data <- read.csv(file_name, header = F, sep = "\t", quote = "\t\t\t\t",
                   dec = ".", fill = TRUE, stringsAsFactors=FALSE)
  data <- data[data$V1 %in% rel_counts$rel, ]
  #print(paste("New file size: ", length(data[, 1])))
  
  for (j in 1:length(data[, 1])) {
    
    rel <- data$V1[j]
    rel_tp_file <- paste(input_path, paste("rels_tp", paste(rel, ".csv", sep = ""), sep = "/"), sep = "/")
    
    t1 <- tolower(data$V2[j])
    t2 <- tolower(data$V5[j])
    if (t1 > t2) {
      aux <- t1
      t1 <- t2
      t2 <- aux
    }
    
    tp_v1 <- paste(t1, t2, sep = " +++ ")
    tp_v2 <- paste(t2, t1, sep = " +++ ")
    
    rel_tp <- data.frame()
    if (file.exists(rel_tp_file)) {
      rel_tp <- read.csv(rel_tp_file, header = F, quote = "\t\t\t\t", stringsAsFactors=FALSE)
      
      if ((!(tp_v1 %in% rel_tp$V1) && !(tp_v2 %in% rel_tp$V1))) {
        rel_tp[length(rel_tp$V1) + 1, ] <- tp_v1
      }
    }
    else {
      rel_tp <- data.frame(V1 = c(tp_v1))
    }
    
    write.table(rel_tp, rel_tp_file,
                sep="\t", col.names=FALSE, quote=FALSE, row.names=FALSE)
    
  }
  
#   write.table(data,
#               paste(file_name, "_toprel", sep=""),
#               sep="\t", col.names=FALSE, quote=FALSE, row.names=FALSE)
  
}
