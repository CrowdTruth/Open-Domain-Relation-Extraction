source("relations.R")

input_files <- list.files(paste(input_folder, "/input/", sep = ""), pattern = "f[0-9]*.csv")

crowd_data <- data.frame(stringsAsFactors = F)
for (idx in 1:length(input_files)) {
  file <- paste("/input/", input_files[idx], sep = "")
  aux <- read.csv(paste(input_folder, file, sep = ""), header = T, sep = ",",
                  dec = ".", fill = TRUE, stringsAsFactors=FALSE)
  
  # print(paste("FILE", file, ":", length(aux[, 1]), "judg.; ", length(unique(aux$sent_id)), "sent."))
  
  crowd_data <- rbind(crowd_data, aux)
}

baseline_data <- crowd_data[!duplicated(crowd_data$sent_id), ]
baseline_data <- baseline_data[, c("sent_id", "relation", "term1", "b1", "e1", "term2", "b2", "e2", "sentence")]

output_sent <- paste(input_folder, "/output/sent-vec.csv", sep = "")
output_work <- paste(input_folder, "/output/work-vec.csv", sep = "")

#### remove symmetrical relations ####

for (idx in 1:length(crowd_data[, 1])) {
  
  if (MERGED_RELATIONS) {
    if (grepl( "per:origin",          crowd_data$rellist[idx]) &&
        !grepl("per:place_of_birth", crowd_data$rellist[idx])) {
      crowd_data$rellist[idx] <- paste(crowd_data$rellist[idx], "per:place_of_birth", sep = "\n")
    }
    if (grepl( "per:top_member_employee_of_org", crowd_data$rellist[idx]) &&
        !grepl("per:employee_or_member_of"     , crowd_data$rellist[idx])) {
      crowd_data$rellist[idx] <- paste(crowd_data$rellist[idx], "per:employee_or_member_of", sep = "\n")
    }
  }
  if (grepl( "per:alternate_names", crowd_data$rellist[idx]) &&
      !grepl("org:alternate_names", crowd_data$rellist[idx])) {
    crowd_data$rellist[idx] <- paste(crowd_data$rellist[idx], "org:alternate_names", sep = "\n")
  }
}

relations <- setdiff(relations, c("per:alternate_names"))
crowd_data$rellist <- gsub("per:alternate_names\n", "", crowd_data$rellist)
crowd_data$rellist <- gsub("per:alternate_names", "", crowd_data$rellist)

if (MERGED_RELATIONS) {
  relations <- setdiff(relations, c("per:origin"))
  crowd_data$rellist <- gsub("per:origin\n", "", crowd_data$rellist)
  crowd_data$rellist <- gsub("per:origin", "", crowd_data$rellist)
}
