trim_sent_set <- function(data) {
  dupl_sent <- data$sentence[duplicated(data$sentence)]
  
  new_data <- data[data$sentence %in% dupl_sent, ]
  new_data <- new_data[order(new_data$sentence), ]
  new_data <- new_data[!duplicated(new_data), ]
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
relations <- substr(relations, 5, nchar(relations))

#### rel overlap ####

rel_overlap <- list()

for (idx in 1:length(relations)) {
  rel_overlap[[idx]] <- rep(0, length(relations))
}



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
    }
    
    jdx <- jdx + 1
  }
}

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
