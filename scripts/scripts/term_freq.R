input_path <- "/home/anca/Documents/relationsentences"

relations <- c("dbo-birthplace",
               "dbo-nationality",
               "dbo-stateoforigin",
               "dbo-deathplace",
               "dbo-leadername",
               "dbo-residence",
               "dbo-populationplace")


all_data <- data.frame()
wrong_type_data <- data.frame()

# # for (i in 0:10) {
# for (i in 0:899) {
#   file_name <- paste(input_path,
#                      paste("allwiki.txt_", paste(i, ".sentences.rel", sep = ""), sep = ""),
#                      sep = "/")
#   
#   print(paste("PROCESSING", file_name))
#   data <- read.csv(file_name, header = F, sep = "\t", quote = "\t\t\t\t",
#                    dec = ".", fill = TRUE, stringsAsFactors=FALSE)
#   
#   data <- data[data$V1 %in% relations, ]
#   
#   all_data <- rbind(all_data, data)
# } 

load("../data/chang_corpus.Rdata")
all_data <- all_data[all_data$V1 %in% relations, ]

term_freq <- sapply(relations, function(rel) {
  sampled <- all_data[all_data$V1 %in% c(rel), ]
  table(append(tolower(sampled$V5), tolower(sampled$V2)))[order(- table(append(tolower(sampled$V5), tolower(sampled$V2))))]
})

