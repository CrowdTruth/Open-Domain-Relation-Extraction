input_folder <- "../data/classifier_eval/"

correct_data <- read.csv("../crowdsourcing/output/sent-vec.csv", stringsAsFactors = F)

files = list.files(path = input_folder, pattern = ".csv")

accuracy_at_clarity <- matrix(
  rep(0, 99 * length(files)),
  ncol = length(files), nrow = 99, byrow = T,
  dimnames = list((1:99)/100, gsub(".csv", "", files))
)

for (idx in 1:length(files)) {
  file <- paste(input_folder, files[idx], sep = "")
  data <- read.csv2(file, stringsAsFactors = F, header = T, sep = ",")
  relation <- gsub(".csv", "", gsub(":", ".", files[idx]))
  
  correct_data <- correct_data[correct_data$sent_id %in% data$sent_id, ]
  correct_data <- correct_data[order(correct_data$sent_id), ]
  data <- data[order(data$sent_id), ]
  data$clarity <- correct_data$clarity
  
  data$clarity <- as.numeric(data$clarity)
  data$Correct <- as.numeric(data$Correct)
  
  for (jdx in 1:99) {
    clarity <- jdx / 100
    aux <- data[data$clarity >= clarity, ]
    aux <- rbind(aux, data[data[, paste(relation, "_srs", sep = "")] >= 0.5, ])
    aux <- aux[!duplicated(aux$sent_id), ]
    
    if (length(aux[, 1]) > 0) {
      accuracy_at_clarity[jdx, idx] <- sum(aux$Correct == 1) / length(aux[ , 1])
    }
  }
}

sentences_at_clarity <- rep(0, 99)
for (jdx in 1:99) {
  clarity <- jdx / 100
  aux <- data[data$clarity >= clarity, ]
  sentences_at_clarity[jdx] <- length(aux[, 1])
}

plot((1:99)/100, sentences_at_clarity, type = "l", xlab = "clarity", ylab = "# sentences")
col <- colors()[sample(length(colors()), length(files))]
 
# plot((60:99)/100, accuracy_at_clarity[60:99, 1], type = "l", lwd = 3, col = col[1],
#      xlab = "clarity", ylab = "accuracy", ylim = c(min(accuracy_at_clarity) - 0.01, 1))
par(mar=c(5,4,4,5)+.1)

for (idx in 1:length(files)) {
  png(paste("../fig/acc_at_clarity_", files[idx], ".png", sep = ""))
  plot((1:99)/100, accuracy_at_clarity[1:99, idx], type = "l", lwd = 3, col = "red",
       xlab = "clarity", ylab = "accuracy", ylim = c(0, 1),
       main = files[idx])
  par(new=TRUE)
  plot((1:99)/100, sentences_at_clarity, type = "l", col = "blue", xaxt="n",yaxt="n",xlab="",ylab="")
  axis(4)
  mtext("# sentences",side=4,line=3)
  legend("bottomleft",col=c("red","blue"),lty=1,legend=c("accuracy", "# sentences"))
  dev.off()
}
