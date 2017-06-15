unannotated_data <- read.csv("../data/tac-distant-supervision.subset", header = F, sep = "\t",
                             dec = ".", fill = TRUE, stringsAsFactors=FALSE)

crowd_data <- read.csv("../crowdsourcing/output/sent-vec.csv")
unannotated_data <- unannotated_data[! unannotated_data$V9 %in% crowd_data$sentence, ]

library(plyr)
unannotated_data <- rename(unannotated_data, c(
  "V1" = "term1",
  "V2" = "relation",
  "V3" = "term2",
  "V4" = "sent_id",
  "V5" = "b1",
  "V6" = "e1",
  "V7" = "b2",
  "V8" = "e2",
  "V9" = "sentence"))

##### rename relations #####

place_rels <- c("city", "country", "stateorprovince")
places_rels <- c("cities", "countries", "statesorprovinces")

for (idx in 1:length(unannotated_data[, 1])) {
  
  for (jdx in 1:length(place_rels)) {
    if (grepl(place_rels[jdx], unannotated_data$relation[idx])) {
      unannotated_data$relation[idx] <- gsub(place_rels[jdx], "place", unannotated_data$relation[idx])
    }
    
    if (grepl(places_rels[jdx], unannotated_data$relation[idx])) {
      unannotated_data$relation[idx] <- gsub(places_rels[jdx], "places", unannotated_data$relation[idx])
    }
  }
  
  if (unannotated_data$relation[idx] == "per:parents" ||
      unannotated_data$relation[idx] == "org:members" ||
      unannotated_data$relation[idx] == "org:parents" ||
      unannotated_data$relation[idx] == "org:member_of" ||
      unannotated_data$relation[idx] == "org:top_members_employees" ||
      unannotated_data$relation[idx] == "org:founded_by") {
    aux <- unannotated_data$term1[idx]
    unannotated_data$term1[idx] <- unannotated_data$term2[idx]
    unannotated_data$term2[idx] <- aux
    
    aux <- unannotated_data$b1[idx]
    unannotated_data$b1[idx] <- unannotated_data$b2[idx]
    unannotated_data$b2[idx] <- aux
    
    aux <- unannotated_data$e1[idx]
    unannotated_data$e1[idx] <- unannotated_data$e2[idx]
    unannotated_data$e2[idx] <- aux
    
    if (unannotated_data$relation[idx] == "per:parents") unannotated_data$relation[idx] <- "per:children"
    if (unannotated_data$relation[idx] == "org:members") unannotated_data$relation[idx] <- "org:member_of"
    if (unannotated_data$relation[idx] == "org:parents") unannotated_data$relation[idx] <- "org:subsidiaries"
    if (unannotated_data$relation[idx] == "org:member_of") unannotated_data$relation[idx] <- "per:employee_or_member_of"
    if (unannotated_data$relation[idx] == "org:top_members_employees") unannotated_data$relation[idx] <- "per:top_member_employee_of_org"
    if (unannotated_data$relation[idx] == "org:founded_by") unannotated_data$relation[idx] <- "per:founded_org"
    
  }
}

unannotated_data$relation <- gsub("org:alternate_names", "per:alternate_names", unannotated_data$relation)

unannotated_data$sent_id <- ""
unannotated_data <- unannotated_data[!duplicated(unannotated_data), ]
unannotated_data$sent_id <- paste("DS-", sprintf("%06d", 1:length(unannotated_data[, 1])), sep="")


##### find symmetric relations ####

sim_sent_ids <- c()
sim_rels <- c("per:spouse", "per:siblings", "per:alternate_names", "org:alternate_names", "per:other_family")

for (idx in 1:length(unannotated_data[, 1])) {
  jdx <- idx + 1
  
  while (jdx <= length(unannotated_data[, 1]) && unannotated_data$sentence[idx] == unannotated_data$sentence[jdx]) {
    overlap1 <- term_overlap(unannotated_data$b1[idx], unannotated_data$e1[idx], unannotated_data$b2[jdx], unannotated_data$e2[jdx])
    overlap2 <- term_overlap(unannotated_data$b2[idx], unannotated_data$e2[idx], unannotated_data$b1[jdx], unannotated_data$e1[jdx])
    
    if (unannotated_data$relation[idx] == unannotated_data$relation[jdx] &&
        overlap1 > 0 && overlap2 > 0) {
      #         unannotated_data$b1[idx] == unannotated_data$b2[jdx] &&
      #         unannotated_data$e1[idx] == unannotated_data$e2[jdx] &&
      #         unannotated_data$b2[idx] == unannotated_data$b1[jdx] &&
      #         unannotated_data$e2[idx] == unannotated_data$e1[jdx]) {
      
      if (!unannotated_data$relation[idx] %in% sim_rels) {
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
            unannotated_data$b1[idx] <- unannotated_data$b2[jdx]
            unannotated_data$e1[idx] <- unannotated_data$e2[jdx]
            unannotated_data$term1[idx] <- unannotated_data$term2[jdx]
          }
          else {
            unannotated_data$b2[idx] <- unannotated_data$b1[jdx]
            unannotated_data$e2[idx] <- unannotated_data$e1[jdx]
            unannotated_data$term2[idx] <- unannotated_data$term1[jdx]
          }
          sim_sent_ids <- append(sim_sent_ids, jdx)
        }
      }
    }
    
    jdx <- jdx + 1
  }
  
  # print(idx)
}

test <- unannotated_data[sim_sent_ids, ]

unannotated_data <- unannotated_data[setdiff(1:length(unannotated_data[, 1]), sim_sent_ids), ]


##### keep most complete terms ####


term_overlap <- function(b1, e1, b2, e2) {
  if (b1 <= b2 && b2 <= e1 && e2 <= e1) return(1)
  if (b2 <= b1 && b1 <= e2 && e1 <= e2) return(2)
  return(0)
}

sim_sent_ids <- c()

for (idx in 1:length(unannotated_data[, 1])) {
  jdx <- idx + 1
  
  while (jdx <= length(unannotated_data[, 1]) && unannotated_data$sentence[idx] == unannotated_data$sentence[jdx]) {
    overlap1 <- term_overlap(unannotated_data$b1[idx], unannotated_data$e1[idx], unannotated_data$b1[jdx], unannotated_data$e1[jdx])
    overlap2 <- term_overlap(unannotated_data$b2[idx], unannotated_data$e2[idx], unannotated_data$b2[jdx], unannotated_data$e2[jdx])
    
    if (unannotated_data$relation[idx] == unannotated_data$relation[jdx] &&
        overlap1 > 0 && overlap2 > 0) {
      
      if (overlap1 == 1 && overlap2 == 1) {
        sim_sent_ids <- append(sim_sent_ids, jdx)
      }
      else if (overlap1 == 2 && overlap2 == 2) {
        sim_sent_ids <- append(sim_sent_ids, idx)
      }
      else {
        if (overlap1 == 2) {
          unannotated_data$b1[idx] <- unannotated_data$b1[jdx]
          unannotated_data$e1[idx] <- unannotated_data$e1[jdx]
          unannotated_data$term1[idx] <- unannotated_data$term1[jdx]
        }
        else {
          unannotated_data$b2[idx] <- unannotated_data$b2[jdx]
          unannotated_data$e2[idx] <- unannotated_data$e2[jdx]
          unannotated_data$term2[idx] <- unannotated_data$term2[jdx]
        }
        sim_sent_ids <- append(sim_sent_ids, jdx)
      }
    }
    
    jdx <- jdx + 1
  }
  
  # print(idx)
}

test <- unannotated_data[sim_sent_ids, ]

unannotated_data <- unannotated_data[setdiff(1:length(unannotated_data[, 1]), sim_sent_ids), ]


##### flatten rel list ####

unannotated_data <- unannotated_data[order(unannotated_data$sentence, unannotated_data$term1, unannotated_data$term2), ]
sentences <- unique(unannotated_data$sentence)
comb_data <- data.frame(stringsAsFactors = F)

idx <- 1
rels <- unannotated_data$relation[idx]
unannotated_data$baseline <- 0

while (idx < length(unannotated_data[,1 ])) {
  idx <- idx + 1
  
  if ((unannotated_data$term1[idx] == unannotated_data$term1[idx - 1]) &&
      (unannotated_data$term2[idx] == unannotated_data$term2[idx - 1]) &&
      (unannotated_data$sentence[idx] == unannotated_data$sentence[idx - 1])) {
    if (!grepl(unannotated_data$relation[idx], rels)) rels <- paste(rels, unannotated_data$relation[idx], sep = ";")
  }
  
  else {
    unannotated_data$relation[idx - 1] <- rels
    unannotated_data$baseline[idx - 1] <- 1
    rels <- unannotated_data$relation[idx]
  }
  
  if (idx %% 1000 == 0) print(paste("Sentence", idx, "..."))
}
unannotated_data$relation[length(unannotated_data$relation)] <- rels

unannotated_data <- unannotated_data[unannotated_data$baseline %in% c(1), ]


#### replace extra rels with none ####

source("relations.R")
all_relations <- unique(unlist(strsplit(unannotated_data$relation, ";")))
none_relations <- c(setdiff(all_relations, relations), "per:age")

for (idx in 1:length(none_relations)) {
  unannotated_data$relation <- gsub(none_relations[idx], "none", unannotated_data$relation)
}

unannotated_data$relation <- gsub("none;", "", unannotated_data$relation)
unannotated_data$relation <- gsub(";none", "", unannotated_data$relation)

unannotated_data$sent_id <- ""
unannotated_data <- unannotated_data[!duplicated(unannotated_data), ]
unannotated_data$sent_id <- paste("DS-", sprintf("%06d", 1:length(unannotated_data[, 1])), sep="")


#### strict deduplication based on alphabetic characters in sentences ####

strict_sent <- gsub("[^A-Za-z]", "", unannotated_data$sentence)
strict_data <- data.frame(sentence = strict_sent, term1 = unannotated_data$term1, term2 = unannotated_data$term2, sent_id = unannotated_data$sent_id, stringsAsFactors = F)
strict_data <- strict_data[!duplicated(strict_data[c("sentence", "term1", "term2")]), ]
unannotated_data <- unannotated_data[unannotated_data$sent_id %in% strict_data$sent_id, ]


#### save data ####

write.csv(unannotated_data, "../data/DS_multiclass_classifier_unmerged.csv", row.names = F)


#### data v.2.0 : merge origin, mirror top employee ####

unannotated_data$relation <- gsub("per:alternate_names", "org:alternate_names", unannotated_data$relation)
unannotated_data$relation <- gsub("org:alternate_names;org:alternate_names", "org:alternate_names", unannotated_data$relation)

unannotated_data$relation <- gsub("per:origin", "per:place_of_birth", unannotated_data$relation)
unannotated_data$relation <- gsub("per:top_member_employee_of_org", "per:top_member_employee_of_org;per:employee_or_member_of", unannotated_data$relation)

for (idx in 1:length(unannotated_data[, 1])) {
  rels <- strsplit(unannotated_data$relation[idx], ";")[[1]]
  rels <- rels[order(rels)]
  unannotated_data$relation[idx] <- paste(rels, sep = ";", collapse=";")
}

unannotated_data$relation <- gsub("per:employee_or_member_of;per:employee_or_member_of", "per:employee_or_member_of", unannotated_data$relation)
unannotated_data$relation <- gsub("per:place_of_birth;per:place_of_birth", "per:place_of_birth", unannotated_data$relation)
write.csv(unannotated_data, "../data/DS_multiclass_classifier_merged_rels.csv", row.names = F)


#### binary classifier input ####

unannotated_data <- read.csv("../data/DS_multiclass_classifier_merged_rels.csv", stringsAsFactors = F)

relations <- setdiff(unique(unlist(strsplit(unannotated_data$relation, ";"))), "none")

for (relation in relations) {
  print(relation)
  binary_classifier <- unannotated_data
  binary_classifier$relation <- "none"
  binary_classifier[grepl(relation, unannotated_data$relation), "relation"] <- relation
  
  pos_data <- binary_classifier[!binary_classifier$relation %in% c("none"), ]
  neg_data <- binary_classifier[binary_classifier$relation %in% c("none"), ]
  max_size <- min(length(pos_data[, 1]), length(neg_data[, 1]))
  
  binary_classifier_ratio50 <- rbind(
    pos_data[sample(1:length(pos_data[, 1]), max_size), ],
    neg_data[sample(1:length(neg_data[, 1]), max_size), ]
  )
  
  write.csv(binary_classifier_ratio50, paste("../data/", relation, ".csv", sep = ""), row.names = F)
}
