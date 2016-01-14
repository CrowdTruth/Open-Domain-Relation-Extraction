FILE <- "/home/anca/Documents/Open-Domain-Relation-Extraction/data/pilot/test_1.csv"

relations <- c("per:title",
               "org:founded_by",
               "per:place_of_birth",
               "per:children",
               "per:alternate_names",
               "per:cause_of_death",
               "org:top_members_employees",
               "per:employee_or_member_of",
               "per:spouse",
               "org:alternate_names",
               "per:age",
               "org:subsidiaries",
               "per:place_of_death",
               "per:schools_attended",
               "org:place_of_headquarters",
               "per:charges",
               "per:origin",
               "per:places_of_residence")

baseline_file <-  FILE

baseline_data <- read.csv(baseline_file, header = T, sep = ",",
                          dec = ".", fill = TRUE, stringsAsFactors=FALSE)

for (idx in 1:length(baseline_data[, 1])) {
  if (baseline_data$seed_relation[idx] == "org:parents") {
 #     baseline_data$seed_relation[idx] == "org:parents") {
    
#     aux <- baseline_data$term1[idx]
#     baseline_data$term1[idx] <- baseline_data$term2[idx]
#     baseline_data$term2[idx] <- aux
#     
#     aux <- baseline_data$b1[idx]
#     baseline_data$b1[idx] <- baseline_data$b2[idx]
#     baseline_data$b2[idx] <- aux
#     
#     aux <- baseline_data$e1[idx]
#     baseline_data$e1[idx] <- baseline_data$e2[idx]
#     baseline_data$e2[idx] <- aux
    
   # if (baseline_data$seed_relation[idx] == "per:parents")
      baseline_data$relation[idx] <- "org:subsidiaries"
  #  else 
  #    baseline_data$relation[idx] <- "org:subsidiaries"
  }
}

# baseline_data <- baseline_data[baseline_data$seed_relation %in% relations, ]

write.csv(baseline_data, FILE, row.names = F)
