sent_vec <- read.csv("../data/unannotated/output/sent-vec.csv")
work_vec <- read.csv("../data/unannotated/output/work-vec.csv")

sent_vec_u <- sent_vec[grepl("-U-", sent_vec$sent_id), ]
sent_vec_a <- sent_vec[grepl("-A-", sent_vec$sent_id), ]

hist(sent_vec_a$clarity, breaks = (1:10)/10, ylim =c(0,80),
     col = "lightblue", main = "Clarity of ambiguous sentences", xlab = "Clarity")
hist(sent_vec_u$clarity, breaks = (1:10)/10, ylim =c(0,80),
     col = "lightblue", main = "Clarity of unambiguous sentences", xlab = "Clarity")


source("relations.R")
relations <- gsub(":", "\\.", relations)
rel_u_sent <- rep(0, length(relations))
rel_a_sent <- rep(0, length(relations))
for (idx in 1:(length(relations) - 1)) {
  png(paste("../fig/first500-analysis/srs-hist/",
            relations[idx], "_seed.png"))
  data_rel <- sent_vec[grepl(relations[idx], sent_vec$seed_relation), ]
  hist(data_rel[, paste(relations[idx], "_srs", sep = "")], main = paste("seed relation", relations[idx]),
       xlab = paste(relations[idx], "sentence-relation score"), col = "lightyellow", breaks = (0:10)/10)
  dev.off()
  
  png(paste("../fig/first500-analysis/srs-hist/",
            relations[idx], "_not_seed.png"))
  data_neg <- sent_vec[!sent_vec$sent_id %in% data_rel$sent_id, ]
  hist(data_neg[, paste(relations[idx], "_srs", sep = "")], main = paste(relations[idx], "not seed relation"),
       xlab = paste(relations[idx], "sentence-relation score"), col = "gray", breaks = (0:10)/10)
  dev.off()
  
  rel_u_sent[idx] <- length(data_rel[grepl("-U-", data_rel$sent_id), ]$sent_id)
  rel_a_sent[idx] <- length(data_rel[grepl("-A-", data_rel$sent_id), ]$sent_id)
}

rel_freq <- data.frame(
  relation = relations,
  ambig = rel_a_sent,
  unambig = rel_u_sent,
  stringsAsFactors = F
)

source("relation_measures.R")
caus_pow <- causal_power(sent_vec)
seed_caus_pow <- seed_causal_power(sent_vec)