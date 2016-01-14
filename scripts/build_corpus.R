require(scales)

input_path <- "/home/anca/Documents/relationsentences"

max_pos <- 15

relations <- c("dbo-birthplace",
               "dbo-nationality",
               "dbo-stateoforigin",
               "dbo-deathplace",
               "dbo-leadername",
               "dbo-residence",
               "dbo-populationplace")


####### helper functions #######

sent_too_short <- function(sent, term1, term2) {
  return(nchar(sent) >= nchar(term1) + nchar(term2) + 20)
}

sent_adds_val <- function(rel_counts, sent_counts, max_pos) {
  adds_val <- F
  aux <- rel_counts + sent_counts
  
  for (idx in 1:length(rel_counts)) {
    if (rel_counts[idx] < aux[idx] && aux[idx] <= max_pos) {
      adds_val <- T
    }
  }
  
  return(adds_val)
}

make_term_pair_unique_per_sent <- function(data) {
  stripped_data <- data.frame()
  
  sent <- unique(data$V8)
  for (idx in 1:length(sent)) {
    sent_data <- data[data$V8 %in% c(sent[idx]), ]
    sample_idx <- sample(1:length(sent_data[, 1]), 1)
    
    term1 <- sent_data$V2[sample_idx]
    b1 <- sent_data$V3[sample_idx]
    e1 <- sent_data$V4[sample_idx]
    term2 <- sent_data$V5[sample_idx]
    b2 <- sent_data$V6[sample_idx]
    e2 <- sent_data$V7[sample_idx]
    
    sent_data <- sent_data[sent_data$V2 %in% c(term1, term2), ]
    sent_data <- sent_data[sent_data$V3 %in% c(b1, b2), ]
    sent_data <- sent_data[sent_data$V5 %in% c(term2, term1), ]
    sent_data <- sent_data[sent_data$V6 %in% c(b2, b1), ]
    
    stripped_data <- rbind(stripped_data, sent_data)
  }
  
  return(stripped_data)
}


####### select a random sample #######
all_data <- data.frame()
wrong_type_data <- data.frame()

# for (i in 0:899) {
for (i in 0:10) {
  file_name <- paste(input_path,
                     paste("allwiki.txt_", paste(i, ".sentences.rel", sep = ""), sep = ""),
                     sep = "/")
  
  print(paste("PROCESSING", file_name))
  data <- read.csv(file_name, header = F, sep = "\t", quote = "\t\t\t\t",
                   dec = ".", fill = TRUE, stringsAsFactors=FALSE)
  
  wrong_type_data <- rbind(wrong_type_data, data[! data$V1 %in% relations, ])
  wrong_type_data <- wrong_type_data[! wrong_type_data$V1 %in% c("dbo-demonym"), ]
  
  data <- data[data$V1 %in% relations, ]
  data <- make_term_pair_unique_per_sent(data)
  
  all_data <- rbind(all_data, data)
} 
all_data <- all_data[sent_too_short(all_data$V8, all_data$V2, all_data$V5), ]
all_data <- all_data[sample(1:length(all_data[, 1]), length(all_data[, 1])), ]

####### remove already tested #######

test_1 <- read.csv("../crowdsourcing/input/test.csv")
test_2 <- read.csv("../crowdsourcing/input/test_2.csv")
all_data <- all_data[! all_data$V8 %in% append(test_1$sentence,
                                               test_2$sentence), ]

id_len <- length(test_1[, 1]) + length(test_2[, 1])

####### pick proportionate rels #######

rel_counts <- rep(0, length(relations))
sentences <- c()

sent <- unique(all_data$V8)

for (idx in 1:length(sent)) {
  extr <- all_data[all_data$V8 %in% c(sent[idx]), ]
  
  if (sent_adds_val(rel_counts, (relations %in% extr$V1) * 1, max_pos)) {
       sentences <- append(sentences, sent[idx])
       rel_counts <- rel_counts + (relations %in% extr$V1) * 1
       
       print(rel_counts)
  }
  
  if (prod(rel_counts >= max_pos) > 0) break
}

if (prod(rel_counts >= max_pos) == 0)
  print("STILL NEEDS DATA")

sampled_data <- all_data[all_data$V8 %in% sentences, ]


####### append data with wrong term types #######

wrong_type_data <- wrong_type_data[sent_too_short(wrong_type_data$V8, wrong_type_data$V2, wrong_type_data$V5), ]
wrong_type_data <- wrong_type_data[!duplicated(wrong_type_data$V8), ]
wrong_type_data <- wrong_type_data[!wrong_type_data$V8 %in% sampled_data$V8, ]

wtd_size <- 100 - length(sentences)
wtd <- wrong_type_data[sample(1:length(wrong_type_data[, 1]), wtd_size), ]
sampled_data <- rbind(sampled_data, wtd)

sentences <- append(sentences, unique(wtd$V8))


####### prettify data frame #######

# sentences <- sentences[1:idx]

names(sampled_data) <- c("seed_rel", "term1", "b1", "e1", "term2", "b2", "e2", "sentence")
sampled_data <- sampled_data[order(sampled_data$sentence), ]
sentences <- sentences[order(sentences)]
sampled_data$Sent_id <- rep(1:length(sentences) + id_len, table(sampled_data$sentence)) + 100000

sampled_data <- sampled_data[, c("Sent_id", "seed_rel", "term1", "b1", "e1", "term2", "b2", "e2", "sentence")]

write.csv(sampled_data, "../crowdsourcing/input/test_3_with_rels.csv", row.names = F)
write.csv(sampled_data[!duplicated(sampled_data$Sent_id), ], "../crowdsourcing/input/test_3.csv", row.names = F)


####### overlap plots #######

for (idx in 1:length(relations)) {
  rel <- relations[idx]
  
  labels <- setdiff(relations, c(rel))
  sent <- all_data[all_data$V1 %in% c(rel), ]$V8
  sent_with_rels <- all_data[all_data$V8 %in% sent, ]
  sent_with_other_rels <- sent_with_rels[sent_with_rels$V1 %in% labels, ]
  sent_counts <- table(sent_with_other_rels$V1)
  
  sents_with_single_rel <- setdiff(sent, sent_with_other_rels$V8)
  sent_counts <- append(sent_counts, length(sents_with_single_rel))
  names(sent_counts)[length(names(sent_counts))] <- "none"
  
  png(paste("../fig/rel-overlap/", rel, ".png", sep = ""))
  x <- barplot(sent_counts,
               col = alpha(rainbow(length(sent_counts)), 0.5),
               main = paste("Overlap of", rel), xaxt="n")
  labs <- paste(names(sent_counts), "\n", sent_counts)
  text(cex=1, x=x+.25, y=-.25, labs, xpd=TRUE, srt=45, pos=2)
  
  dev.off()
}
