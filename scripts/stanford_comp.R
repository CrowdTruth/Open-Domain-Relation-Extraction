stanford <- read.csv("../data/stanford_data/mimlre-2014-07-17-data/annotated_sentences.csv", stringsAsFactors = F)
stanford <- stanford[!stanford$relation %in% c("relation"), ]
# mccallum <- read.csv("../data/ds/tac-distant-supervision.subset", sep = "\t", header = F, stringsAsFactors = F)

source("relations.R")
stanford$relation <- gsub("statesorprovinces", "places", stanford$relation)
stanford$relation <- gsub("cities", "places", stanford$relation)
stanford$relation <- gsub("countries", "places", stanford$relation)
stanford$relation <- gsub("city", "place", stanford$relation)
stanford$relation <- gsub("country", "place", stanford$relation)
stanford$relation <- gsub("stateorprovince", "place", stanford$relation)
stanford$relation <- gsub("no_relation", "none", stanford$relation)
stanford$relation <- gsub("/", "_", stanford$relation)
stanford$relation <- gsub("per:employee_of", "per:employee_or_member_of", stanford$relation)
stanford$relation <- gsub("per:stateorprovinces_of_residence", "per:statesorprovinces_of_residence", stanford$relation)

stanford_rels <- unique(stanford$relation)
# 
# mccallum_rels <- unique(mccallum$V2)
# mccallum_rels <- gsub("org:date_founded", "org:founded", mccallum_rels)
# mccallum_rels <- gsub("org:date_dissolved", "org:dissolved", mccallum_rels)
# 
# setdiff(stanford_rels, mccallum_rels)
# setdiff(mccallum_rels, stanford_rels)
# 
# stanford_sent <- gsub("^[:alnum:]", "", tolower(unique(stanford$sentence)))
# mccallum_sent <- gsub("^[:alnum:]", "", tolower(unique(mccallum$V9)))
# 
# length(setdiff(stanford_sent, mccallum_sent))
# length(setdiff(mccallum_sent, stanford_sent))

crowd_rels <- relations

setdiff(stanford_rels, crowd_rels)
setdiff(crowd_rels, stanford_rels)


number_of_words <- function(sentence) {
  sent_split <- strsplit(sentence, " ")
  return(sapply(sent_split, length))
}

stanford <- stanford[number_of_words(stanford$sentence) < 50, ]
stanford$entity <- trimws(stanford$entity)
stanford$slotValue <- trimws(stanford$slotValue)

stanford$b1 <- number_of_words(substr(stanford$sentence, 1, as.numeric(stanford$entityCharOffsetBegin)))
stanford$e1 <- stanford$b1 + number_of_words(stanford$entity)
stanford$b2 <- number_of_words(substr(stanford$sentence, 1, as.numeric(stanford$slotValueCharOffsetBegin)))
stanford$e2 <- stanford$b2 + number_of_words(stanford$slotValue)

cols <- c("sentence", "entity", "slotValue")

for (idx in 1:length(cols)) {
  stanford[, cols[idx]] <- gsub("&lt;", "<", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&gt;", ">", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&quot;", "\"", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&#44;", ",", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&aacute;", "á", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&ndash;", "-", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&atilde;", "ã", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&rsquo;", "'", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&eacute;", "é", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&mdash;", "-", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&iacute;", "í", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&amp;quot;", "\"", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&ccedil;", "ç", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&otilde;", "õ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&scaron;", "š", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&ldquo;", "\"", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&agrave;", "à", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&oacute;", "ó", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&rdquo;", "\"", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&amp;", "&", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&uuml;", "ü", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&egrave;", "è", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&acirc;", "â", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&ocirc;", "ô", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&auml;", "ä", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&ouml;", "ö", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&euml;", "ë", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&amp;amp;", "&", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&uacute;", "ú", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&eth", "ð", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&ograve;", "ò", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&hellip;", "…", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&yen;", "￥", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Eacute;", "É", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&sect;", "§", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&ntilde;", "ñ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Aacute;", "Á", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&aelig;", "æ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Ouml;", "Ö", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&pound;", "£", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&oslash;", "ø", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Alpha;", "A", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&nu;", "ν", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&omicron;", "ο", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&iota;", "ι", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&tau;", "τ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&kappa;", "κ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Pi;", "Π", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&alpha;", "α", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&pi;", "π", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&epsilon;", "ε", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&sigma;", "σ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&mu;", "μ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&rho;", " 	ρ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&upsilon;", "υ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Kappa;", "Κ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&aring;", "å", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&yacute;", "ý", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&szlig;", "ß", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&lsquo;", "'", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&gamma;", "γ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Zeta;", "Z", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&THORN;", "Þ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Aring;", "Å", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Scaron;", "Š", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Oacute;", "Ó", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&AElig;", "Æ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&euro;", "€", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Ntilde;", "Ñ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&lambda;", "λ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&sigmaf;", "ς", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&bull;", "•", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&middot;", "·", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&oelig;", "œ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&ecirc;", "ê", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&iuml;", "ï", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Chi;", "X", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Sigma;", "Σ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&phi;", " 	φ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Uacute;", "Ú", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Oslash;", "Ø", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Epsilon;", "E", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&theta;", "θ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&eta;", "η", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Upsilon;", "Y", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&raquo;", "»", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&ordm;", "º", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Uuml;", "Ü", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&ugrave;", "ù", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Icirc;", "Î", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&laquo;", "«", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&icirc;", "î", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&reg;", "®", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&igrave;", "ì", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&acute;", "´", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&diams;", "♦", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Iacute;", "Í", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&shy;", "-", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&dagger;", "†", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&trade;", "™", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Gamma;", "Γ", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&omega;", "ω", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Auml;", "Ä", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Agrave;", "Á", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Mu;", "M", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&deg;", "°", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&copy;", "©", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&ordf;", "ª", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&minus;", "-", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&uml;", "¨", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&ucirc;", "û", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Ccedil;", "Ç", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&rarr;", "→", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&times;", "×", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&prime;", "'", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Prime;", "″", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&bdquo;", "\"", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&Dagger;", "‡", stanford[, cols[idx]])
  
  # stanford[, cols[idx]] <- gsub("&;", "", stanford[, cols[idx]])
}

for (idx in 1:length(cols)) {
  stanford[, cols[idx]] <- gsub("&lt;", "<", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&gt;", ">", stanford[, cols[idx]])
  stanford[, cols[idx]] <- gsub("&amp;", "&", stanford[, cols[idx]])
}

# unique(unlist(regmatches(stanford$sentence, gregexpr("&[[:alpha:]]+;", stanford$sentence))))
# 
# write.csv(stanford, "../data/stanford_annotated.csv", row.names = F)

apostr_sent <- gsub("'s", " 's", stanford$sentence)
sentences <- strsplit(apostr_sent, " ")

stanford$entity <- gsub("'s", " 's", stanford$entity)
stanford$slotValue <- gsub("'s", " 's", stanford$slotValue)

stanford$b2[2142] <- 24
stanford$e2[2142] <- 25
stanford$b1[3649] <- 40
stanford$e1[3649] <- 43
stanford$b1[6585] <- 40
stanford$e1[6585] <- 42
stanford$b2[6585] <- 36
stanford$e2[6585] <- 37
stanford$b2[9172] <- 38
stanford$e2[9172] <- 39
stanford$b2[10238] <- 15
stanford$e2[10238] <- 17
stanford$b1[11243] <- 33
stanford$e1[11243] <- 34
stanford$b2[11243] <- 30
stanford$e2[11243] <- 32
stanford$b1[13773] <- 49
stanford$e1[13773] <- 50
stanford$b2[13773] <- 52
stanford$e2[13773] <- 54

for (idx in 1:length(sentences)) {
  b1 <- c(stanford$b1[idx], stanford$b1[idx] + 1, stanford$b1[idx] + 2, stanford$b1[idx] + 3)
  if (stanford$b1[idx] > 0) b1 <- append(stanford$b1[idx] - 1, b1)
  if (stanford$b1[idx] > 1) b1 <- append(stanford$b1[idx] - 2, b1)
  if (stanford$b1[idx] > 2) b1 <- append(stanford$b1[idx] - 3, b1)
  e1 <- c(stanford$e1[idx] - 2, stanford$e1[idx] - 1, stanford$e1[idx], stanford$e1[idx] + 1, stanford$e1[idx] + 2, stanford$e1[idx] + 3)
  b2 <- c(stanford$b2[idx], stanford$b2[idx] + 1, stanford$b2[idx] + 2, stanford$b2[idx] + 3)
  if (stanford$b2[idx] > 0) b2 <- append(stanford$b2[idx] - 1, b2)
  if (stanford$b2[idx] > 1) b2 <- append(stanford$b2[idx] - 2, b2)
  if (stanford$b2[idx] > 2) b2 <- append(stanford$b2[idx] - 3, b2)
  e2 <- c(stanford$e2[idx] - 2, stanford$e2[idx] - 1, stanford$e2[idx], stanford$e2[idx] + 1, stanford$e2[idx] + 2, stanford$e2[idx] + 3)
  
  for (bidx in 1:length(b1)) {
    for (eidx in 1:length(e1)) {
      if (b1[bidx] < e1[eidx]) {
        term1 <- paste(sentences[[idx]][(b1[bidx] + 1):e1[eidx]], collapse = " ")
        if (term1 == stanford$entity[idx]) {
          stanford$b1[idx] <- b1[bidx]
          stanford$e1[idx] <- e1[eidx]
        }
      }
    }
  }
  
  for (bidx in 1:length(b2)) {
    for (eidx in 1:length(e2)) {
      if (b2[bidx] < e2[eidx]) {
        term2 <- paste(sentences[[idx]][(b2[bidx] + 1):e2[eidx]], collapse = " ")
        if (term2 == stanford$slotValue[idx]) {
          stanford$b2[idx] <- b2[bidx]
          stanford$e2[idx] <- e2[eidx]
        }
      }
    }
  }
  
  term1 <- paste(sentences[[idx]][(stanford$b1[idx] + 1):stanford$e1[idx]], collapse = " ")
  term2 <- paste(sentences[[idx]][(stanford$b2[idx] + 1):stanford$e2[idx]], collapse = " ")
 
  if (term1 != stanford$entity[idx]) print(paste("TERM1 = ", term1, "->" , stanford$entity[idx], ";", idx))
  if (term2 != stanford$slotValue[idx]) print(paste("TERM2 = ", term2, "->" , stanford$slotValue[idx], ";", idx))
}

for (idx in 1:length(stanford[, 1])) {
  if (stanford$relation[idx] == "org:parents") {
    stanford$relation[idx] <- "org:subsidiaries"
    
    aux <- stanford$term1
    stanford$term1 <- stanford$term2
    stanford$term2 <- aux
    
    aux <- stanford$b1
    stanford$b1 <- stanford$b2
    stanford$b2 <- aux
    
    aux <- stanford$e1
    stanford$e1 <- stanford$e2
    stanford$e2 <- aux
  }
}
stanford$sentence <- gsub("'s", " 's", stanford$sentence)

write.csv(stanford, "../data/stanford_annotated.csv", row.names = F)

#### Print train data####

stanford <- read.csv("../data/stanford_annotated.csv", stringsAsFactors = F)
stanford <- stanford[!is.na(as.numeric(stanford$confidence)), ]

rels <- relations

stanford$relation <- gsub("per:origin", "per:place_of_birth", stanford$relation)
stanford$relation <- gsub("per:alternate_names", "org:alternate_names", stanford$relation)
stanford$relation <- gsub("org:founded_by", "per:founded_org", stanford$relation)
stanford$relation <- gsub("org:top_members_employees", "per:top_member_employee_of_org", stanford$relation)
rels <- setdiff(rels, c("per:age", "per:charges", "per:origin", "none", "per:alternate_names"))

stanford_multiclass <- data.frame(
  sent_id = stanford$key,
  relation = stanford$relation,
  term1 = stanford$entity,
  b1 = stanford$b1,
  e1 = stanford$e1,
  term2 = stanford$slotValue,
  b2 = stanford$b2,
  e2 = stanford$e2,
  sentence = stanford$sentence,
  baseline = 1,
  confidence = stanford$confidence,
  stringsAsFactors = F
)
write.csv(stanford_multiclass, "../data/stanford_data/stanford_train/stanford_multiclass_classifier_unmerged.csv", row.names = F)

stanford_multiclass$relation <- gsub("per:origin", "per:place_of_birth", stanford_multiclass$relation)
stanford_multiclass$relation <- gsub("per:top_member_employee_of_org",
                                     "per:top_member_employee_of_org;per:employee_or_member_of", stanford_multiclass$relation)
write.csv(stanford_multiclass, "../data/stanford_data/stanford_train/stanford_multiclass_classifier_merged_rels.csv", row.names = F)

train_data_list <- list()
for (idx in 1:length(rels)) {
  relation <- rels[idx]
  pos_data <- stanford[stanford$relation %in% c(relation), ]
  neg_data <- stanford[!stanford$sentence %in% pos_data$sentence, ]
    
  if (relation == "per:employee_or_member_of") {
    neg_data <- neg_data[!neg_data$relation %in% c("per:top_member_employee_of_org"), ]
    pos_data <- rbind(pos_data, stanford[stanford$relation %in% c("per:top_member_employee_of_org"), ])
  }
  # if (relation == "per:top_member_employee_of_org") neg_data <- neg_data[!neg_data$relation %in% c("per:employee_or_member_of"), ]
  if (relation == "per:origin") neg_data <- neg_data[!neg_data$relation %in% c("per:place_of_birth"), ]
  if (relation == "per:place_of_birth") neg_data <- neg_data[!neg_data$relation %in% c("per:origin"), ]
    
  if (relation == "per:top_member_employee_of_org" || relation == "per:founded_org") {
    aux <- pos_data$term1
    pos_data$term1 <- pos_data$term2
    pos_data$term2 <- aux
    
    aux <- pos_data$b1
    pos_data$b1 <- pos_data$b2
    pos_data$b2 <- aux
      
    aux <- pos_data$e1
    pos_data$e1 <- pos_data$e2
    pos_data$e2 <- aux
  }
    
  train_data <- data.frame(
    sent_id = c(pos_data$key, neg_data$key),
    # baseline = c(rep(1, length(pos_data[, 1])), rep(0, length(neg_data[, 1]))),
    relation = c(rep(relation, length(pos_data[, 1])), rep("none", length(neg_data[, 1]))),
    term1 = c(pos_data$entity, neg_data$entity),
    b1 = c(pos_data$b1, neg_data$b1),
    e1 = c(pos_data$e1, neg_data$e1),
    term2 = c(pos_data$slotValue, neg_data$slotValue),
    b2 = c(pos_data$b2, neg_data$b2),
    e2 = c(pos_data$e2, neg_data$e2),
    sentence = c(pos_data$sentence, neg_data$sentence),
    baseline = c(rep(1, length(pos_data[, 1])), rep(0, length(neg_data[, 1]))),
    confidence = c(pos_data$confidence, neg_data$confidence),
    stringsAsFactors = F
  )
  train_data <- train_data[sample(1:length(train_data[, 1]), length(train_data[, 1])), ]
  train_data_list[[idx]] <- train_data
  
  write.csv(train_data, paste("../data/stanford_data/stanford_train/", relation, ".csv", sep = ""), row.names = F)
}

pos_sum <- sapply(train_data_list, function(x) {
  sum(x$baseline == 1)
})
total_sum <- sapply(train_data_list, function(x) {
  length(x$baseline)
})

rel_stats <- data.frame(
  relation = rels,
  positive_sentences = pos_sum,
  negative_sentences = total_sum - pos_sum,
  stringsAsFactors = F
)
