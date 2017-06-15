trim_sent_set <- function(data) {
  dupl_sent <- data$sentence[duplicated(data$sentence)]
  
  new_data <- data[data$sentence %in% dupl_sent, ]
  new_data <- new_data[order(new_data$sentence), ]
  new_data <- new_data[!duplicated(new_data), ]
  return(new_data)
}

term_overlap <- function(b1, e1, b2, e2) {
  if (b1 <= b2 && b2 <= e1 && e2 <= e1) return(1)
  if (b2 <= b1 && b1 <= e2 && e1 <= e2) return(2)
  return(0)
}


##### initialize #####

unannotated_data <- read.csv("../data/unannotated/tac-distant-supervision.subset", header = T, sep = "\t",
                            dec = ".", fill = TRUE, stringsAsFactors=FALSE)

relations <- unique(unannotated_data$relation)
ambig_data <- trim_sent_set(unannotated_data)


##### rename relations #####

place_rels <- c("city", "country", "stateorprovince")
places_rels <- c("cities", "countries", "statesorprovinces")

for (idx in 1:length(ambig_data[, 1])) {
  
  for (jdx in 1:length(place_rels)) {
    if (grepl(place_rels[jdx], ambig_data$relation[idx])) {
      ambig_data$relation[idx] <- gsub(place_rels[jdx], "place", ambig_data$relation[idx])
    }
    
    if (grepl(places_rels[jdx], ambig_data$relation[idx])) {
      ambig_data$relation[idx] <- gsub(places_rels[jdx], "places", ambig_data$relation[idx])
    }
  }
  
  if (ambig_data$relation[idx] == "per:children" ||
      ambig_data$relation[idx] == "org:members" ||
      ambig_data$relation[idx] == "org:parents") {
    aux <- ambig_data$term1[idx]
    ambig_data$term1[idx] <- ambig_data$term2[idx]
    ambig_data$term2[idx] <- aux
    
    aux <- ambig_data$b1[idx]
    ambig_data$b1[idx] <- ambig_data$b2[idx]
    ambig_data$b2[idx] <- aux
    
    aux <- ambig_data$e1[idx]
    ambig_data$e1[idx] <- ambig_data$e2[idx]
    ambig_data$e2[idx] <- aux
    
    if (ambig_data$relation[idx] == "per:children") ambig_data$relation[idx] <- "per:parents"
    if (ambig_data$relation[idx] == "org:members") ambig_data$relation[idx] <- "org:member_of"
    if (ambig_data$relation[idx] == "org:parents") ambig_data$relation[idx] <- "org:subsidiaries"
    
  }
}

ambig_data <- ambig_data[!duplicated(ambig_data), ]
ambig_data <- trim_sent_set(ambig_data)
ambig_data_all_terms <- ambig_data


##### find symmetric relations ####

sim_sent_ids <- c()
sim_rels <- c("per:spouse", "per:siblings", "per:alternate_names", "org:alternate_names", "per:other_family")

for (idx in 1:length(ambig_data[, 1])) {
  jdx <- idx + 1
  
  while (jdx <= length(ambig_data[, 1]) && ambig_data$sentence[idx] == ambig_data$sentence[jdx]) {
    overlap1 <- term_overlap(ambig_data$b1[idx], ambig_data$e1[idx], ambig_data$b2[jdx], ambig_data$e2[jdx])
    overlap2 <- term_overlap(ambig_data$b2[idx], ambig_data$e2[idx], ambig_data$b1[jdx], ambig_data$e1[jdx])
    
    if (ambig_data$relation[idx] == ambig_data$relation[jdx] &&
        overlap1 > 0 && overlap2 > 0) {
#         ambig_data$b1[idx] == ambig_data$b2[jdx] &&
#         ambig_data$e1[idx] == ambig_data$e2[jdx] &&
#         ambig_data$b2[idx] == ambig_data$b1[jdx] &&
#         ambig_data$e2[idx] == ambig_data$e1[jdx]) {
      
      if (!ambig_data$relation[idx] %in% sim_rels) {
        sim_sent_ids <- append(sim_sent_ids, c(idx, jdx))
      }
      else {
        if (overlap1 == 1 && overlap2 == 1) {
          sim_sent_ids <- append(sim_sent_ids, jdx)
        }
        else if (overlap1 == 2 && overlap2 == 2) {
          sim_sent_ids <- append(sim_sent_ids, idx)
        }
        else {
          if (overlap1 == 2) {
            ambig_data$b1[idx] <- ambig_data$b2[jdx]
            ambig_data$e1[idx] <- ambig_data$e2[jdx]
            ambig_data$term1[idx] <- ambig_data$term2[jdx]
          }
          else {
            ambig_data$b2[idx] <- ambig_data$b1[jdx]
            ambig_data$e2[idx] <- ambig_data$e1[jdx]
            ambig_data$term2[idx] <- ambig_data$term1[jdx]
          }
          sim_sent_ids <- append(sim_sent_ids, jdx)
        }
      }
    }
    
    jdx <- jdx + 1
  }
  
  # print(idx)
}

test <- ambig_data[sim_sent_ids, ]

ambig_data <- ambig_data[setdiff(1:length(ambig_data[, 1]), sim_sent_ids), ]
ambig_data <- trim_sent_set(ambig_data)


##### keep most complete terms ####

sim_sent_ids <- c()

for (idx in 1:length(ambig_data[, 1])) {
  jdx <- idx + 1
  
  while (jdx <= length(ambig_data[, 1]) && ambig_data$sentence[idx] == ambig_data$sentence[jdx]) {
    overlap1 <- term_overlap(ambig_data$b1[idx], ambig_data$e1[idx], ambig_data$b1[jdx], ambig_data$e1[jdx])
    overlap2 <- term_overlap(ambig_data$b2[idx], ambig_data$e2[idx], ambig_data$b2[jdx], ambig_data$e2[jdx])
    
    if (ambig_data$relation[idx] == ambig_data$relation[jdx] &&
        overlap1 > 0 && overlap2 > 0) {
      
      if (overlap1 == 1 && overlap2 == 1) {
        sim_sent_ids <- append(sim_sent_ids, jdx)
      }
      else if (overlap1 == 2 && overlap2 == 2) {
        sim_sent_ids <- append(sim_sent_ids, idx)
      }
      else {
        if (overlap1 == 2) {
          ambig_data$b1[idx] <- ambig_data$b1[jdx]
          ambig_data$e1[idx] <- ambig_data$e1[jdx]
          ambig_data$term1[idx] <- ambig_data$term1[jdx]
        }
        else {
          ambig_data$b2[idx] <- ambig_data$b2[jdx]
          ambig_data$e2[idx] <- ambig_data$e2[jdx]
          ambig_data$term2[idx] <- ambig_data$term2[jdx]
        }
        sim_sent_ids <- append(sim_sent_ids, jdx)
      }
    }
    
    jdx <- jdx + 1
  }
  
  # print(idx)
}

test <- ambig_data[sim_sent_ids, ]

ambig_data <- ambig_data[setdiff(1:length(ambig_data[, 1]), sim_sent_ids), ]
ambig_data <- trim_sent_set(ambig_data)

relations <- unique(ambig_data$relation)


#### rel overlap ####

rel_overlap <- list()

for (idx in 1:length(relations)) {
  rel_overlap[[idx]] <- rep(0, length(relations))
}

multi_rel_sents <- c()

for (idx in 1:length(ambig_data[, 1])) {
  jdx <- idx + 1
  
  while (jdx <= length(ambig_data[, 1]) && ambig_data$sentence[idx] == ambig_data$sentence[jdx]) {
    
    overlap1 <- term_overlap(ambig_data$b1[idx], ambig_data$e1[idx], ambig_data$b1[jdx], ambig_data$e1[jdx])
    overlap2 <- term_overlap(ambig_data$b2[idx], ambig_data$e2[idx], ambig_data$b2[jdx], ambig_data$e2[jdx])
    
    overlap3 <- term_overlap(ambig_data$b1[idx], ambig_data$e1[idx], ambig_data$b2[jdx], ambig_data$e2[jdx])
    overlap4 <- term_overlap(ambig_data$b2[idx], ambig_data$e2[idx], ambig_data$b1[jdx], ambig_data$e1[jdx])
    
    r1_idx <- match(ambig_data$relation[idx], relations)
    r2_idx <- match(ambig_data$relation[jdx], relations)
    
    if (jdx <= length(ambig_data[, 1]) &&
        ambig_data$relation[idx] != ambig_data$relation[jdx] &&
        ((overlap1 > 0 && overlap2 > 0) || (overlap3 > 0 && overlap4 > 0))) {
      
      rel_overlap[[r1_idx]][r2_idx] <- rel_overlap[[r1_idx]][r2_idx] + 1
      rel_overlap[[r2_idx]][r1_idx] <- rel_overlap[[r2_idx]][r1_idx] + 1
      
      multi_rel_sents <- append(multi_rel_sents, c(idx, jdx))
    }
    
    jdx <- jdx + 1
  }
}

multi_rel_sents <- unique(multi_rel_sents)

#### get crowd input ####

source("relations.R")

multi_rel_data <- ambig_data[multi_rel_sents, ]
multi_rel_data <- multi_rel_data[multi_rel_data$relation %in% crowd_rels, ]
multi_rel_data <- trim_sent_set(multi_rel_data)

unambig_rels <- setdiff(crowd_rels, unique(multi_rel_data$relation))

# fix per:employee_or_member_of direction
for (idx in 1:length(multi_rel_data[, 1])) {
  if (multi_rel_data$relation[idx] == "org:founded_by" ||
      multi_rel_data$relation[idx] == "org:top_members_employees") {
    
    if (multi_rel_data$relation[idx] == "org:founded_by") {
      multi_rel_data$relation[idx] <- "per:founded_org"
    }
    else {
      multi_rel_data$relation[idx] <- "per:top_member_employee_of_org"
    }
    
    aux <- multi_rel_data$term1[idx]
    multi_rel_data$term1[idx] <- multi_rel_data$term2[idx]
    multi_rel_data$term2[idx] <- aux
    
    aux <- multi_rel_data$b1[idx]
    multi_rel_data$b1[idx] <- multi_rel_data$b2[idx]
    multi_rel_data$b2[idx] <- aux
    
    aux <- multi_rel_data$e1[idx]
    multi_rel_data$e1[idx] <- multi_rel_data$e2[idx]
    multi_rel_data$e2[idx] <- aux
  }
}

multi_rel_data_unique_sent <- data.frame(stringsAsFactors = F)
unique_idx <- 1

idx <- 1
while (idx <= length(multi_rel_data[, 1])) {
  jdx <- idx + 1
  
  multi_rel_data_unique_sent <- rbind(multi_rel_data_unique_sent, multi_rel_data[idx, ])
  
  while (jdx <= length(multi_rel_data[, 1]) && multi_rel_data$sentence[idx] == multi_rel_data$sentence[jdx]) {
    
    overlap1 <- term_overlap(multi_rel_data$b1[idx], multi_rel_data$e1[idx], multi_rel_data$b1[jdx], multi_rel_data$e1[jdx])
    overlap2 <- term_overlap(multi_rel_data$b2[idx], multi_rel_data$e2[idx], multi_rel_data$b2[jdx], multi_rel_data$e2[jdx])
    
    overlap3 <- term_overlap(multi_rel_data$b1[idx], multi_rel_data$e1[idx], multi_rel_data$b2[jdx], multi_rel_data$e2[jdx])
    overlap4 <- term_overlap(multi_rel_data$b2[idx], multi_rel_data$e2[idx], multi_rel_data$b1[jdx], multi_rel_data$e1[jdx])
    
    if ((overlap1 > 0 && overlap2 > 0) || (overlap3 > 0 && overlap4 > 0)) {
      multi_rel_data_unique_sent$relation[unique_idx] <- paste(multi_rel_data_unique_sent$relation[unique_idx],
                                                               multi_rel_data$relation[jdx],
                                                               sep = ",")
      
      if (overlap1 == 2) {
        multi_rel_data_unique_sent$term1[unique_idx] <- multi_rel_data$term1[jdx]
        multi_rel_data_unique_sent$b1[unique_idx] <- multi_rel_data$b1[jdx]
        multi_rel_data_unique_sent$e1[unique_idx] <- multi_rel_data$e1[jdx]
      }
      if (overlap2 == 2) {
        multi_rel_data_unique_sent$term2[unique_idx] <- multi_rel_data$term2[jdx]
        multi_rel_data_unique_sent$b2[unique_idx] <- multi_rel_data$b2[jdx]
        multi_rel_data_unique_sent$e2[unique_idx] <- multi_rel_data$e2[jdx]
      }
      if (overlap3 == 2) {
        multi_rel_data_unique_sent$term1[unique_idx] <- multi_rel_data$term2[jdx]
        multi_rel_data_unique_sent$b1[unique_idx] <- multi_rel_data$b2[jdx]
        multi_rel_data_unique_sent$e1[unique_idx] <- multi_rel_data$e2[jdx]
      }
      if (overlap4 == 2) {
        multi_rel_data_unique_sent$term2[unique_idx] <- multi_rel_data$term1[jdx]
        multi_rel_data_unique_sent$b2[unique_idx] <- multi_rel_data$b1[jdx]
        multi_rel_data_unique_sent$e2[unique_idx] <- multi_rel_data$e1[jdx]
      }
    }
    
    jdx <- jdx + 1
  }
  
  idx <- jdx
  unique_idx <- unique_idx + 1
}


#### sample unambiguous sentences/relations ####

ambig_rels <- unique(multi_rel_data$relation)
ambig_rels <- ambig_rels[order(ambig_rels)]
sent_count <- as.vector(table(multi_rel_data$relation))

unambig_rels <- setdiff(unambig_rels, "none")

unambig_data <- data.frame(stringsAsFactors = F)

for (idx in 1:length(unambig_rels)) {
  sample_data <- unannotated_data[unannotated_data$relation %in% c(unambig_rels[idx]), ]
  sample_data <- sample_data[!duplicated(sample_data$sentence), ]
  sample_idx <- sample(1:length(sample_data[, 1]), 100)

  unambig_data <- rbind(unambig_data, sample_data[sample_idx, ])
}

for (idx in 1:length(ambig_rels)) {
  if (100 - sent_count[idx] > 0) {
    sample_data <- unannotated_data[unannotated_data$relation %in% c(ambig_rels[idx]), ]
    sample_data <- sample_data[!duplicated(sample_data$sentence), ]
    sample_data <- sample_data[! sample_data$sentence %in% multi_rel_data$sentence, ]
    sample_idx <- sample(1:length(sample_data[, 1]), 100 - sent_count[idx])
    
    unambig_data <- rbind(unambig_data, sample_data[sample_idx, ])
  }
}
unambig_data <- unambig_data[order(unambig_data$sentence), ]


#### print crowd input files ####

unambig_data$Sent_id <- paste("UAD-U", sprintf("%04d", 1:length(unambig_data[, 1])), sep = "-")
multi_rel_data_unique_sent$Sent_id <- paste("UAD-A", sprintf("%04d", 1:length(multi_rel_data_unique_sent[, 1])), sep = "-")
multi_rel_data$Sent_id <- rep("", length(multi_rel_data[, 1]))

multi_rel_data <- multi_rel_data[c(9, 1:8)]
multi_rel_data_unique_sent <- multi_rel_data_unique_sent[c(9, 1:8)]
unambig_data <- unambig_data[c(9, 1:8)]

jdx <- 1
for (idx in 1:length(multi_rel_data_unique_sent[ , 1])) {
  while (jdx <= length(multi_rel_data[, 1]) &&
         multi_rel_data$sentence[jdx] == multi_rel_data_unique_sent$sentence[idx]) {
    multi_rel_data$Sent_id[jdx] <- multi_rel_data_unique_sent$Sent_id[idx]
    jdx <- jdx + 1
  }
}


write.csv(multi_rel_data_unique_sent, "../data/unannotated/crowd_input_ambig.csv", row.names = F)
write.csv(multi_rel_data, "../data/unannotated/crowd_input_ambig_all_rels.csv", row.names = F)
write.csv(unambig_data, "../data/unannotated/crowd_input_unambig.csv", row.names = F)


#### plots ####

relations <- substr(relations, 5, nchar(relations))

for (idx in 1:length(relations)) {
  
  plot_rel_overlap <- rel_overlap[[idx]]
  leg_rel_overlap <- relations
  if (idx == 1) {
    plot_rel_overlap <- plot_rel_overlap[2:length(relations)]
    leg_rel_overlap <- leg_rel_overlap[2:length(relations)]
  } else if (idx == length(relations)) {
    plot_rel_overlap <- plot_rel_overlap[1:(length(relations) - 1)]
    leg_rel_overlap <- leg_rel_overlap[1:(length(relations) - 1)]
  } else {
    plot_rel_overlap <- plot_rel_overlap[c(1:(idx - 1), (idx + 1):length(relations))]
    leg_rel_overlap <- leg_rel_overlap[c(1:(idx - 1), (idx + 1):length(relations))]
  }
  
  png(paste("../fig/rel_overlap/", relations[idx], ".png", sep = ""))
  par(mar = c(13, 4, 2, 2))
  x <- barplot(plot_rel_overlap, 
               main = paste(relations[idx], "sentence overlap"),
               xaxt = 'n', col = "lightblue")
  text(cex=1, x=x+.5, y=-.25, leg_rel_overlap, xpd=TRUE, srt=80, pos=2)
  dev.off()
}
