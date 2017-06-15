work_vec <- read.csv("../crowdsourcing/output/merged/work-vec.csv", stringsAsFactors = F)
plot(work_vec$work_agr * work_vec$sent_agr)
points(log2(1 + work_vec$work_agr * work_vec$sent_agr), pch = 20, col = "red")
points(log10(1 + work_vec$work_agr * work_vec$sent_agr * 9), pch = 22, col = "blue")

hist(work_vec$work_agr * work_vec$sent_agr)
hist(log2(1 + work_vec$work_agr * work_vec$sent_agr))
hist(log10(1 + work_vec$work_agr * work_vec$sent_agr * 9))

sent_vec <- read.csv("../crowdsourcing/output/merged/sent-vec.csv", stringsAsFactors = F)
sent_vec <- sent_vec[order(sent_vec$clarity), ]
plot(sent_vec$clarity)
points(log2(1 + sent_vec$clarity), pch = 20, col = "red")
points(log10(1 + sent_vec$clarity * 9), pch = 22, col = "blue")
# points((exp(sent_vec$clarity) - exp(0)) / (exp(1) - exp(0)), pch = 22, col = "blue")


wqs <- read.csv("../crowdsourcing/output/wqs.csv", stringsAsFactors = F)
sqs <- read.csv("../crowdsourcing/output/sqs.csv", stringsAsFactors = F)

work_vec <- work_vec[order(work_vec$worker_id), ]
wqs <- wqs[order(wqs$worker_id), ]
sent_vec <- sent_vec[order(sent_vec$sent_id), ]
sqs <- sqs[order(sqs$sentence_id), ]
work_vec$wqs <- wqs$wqs
work_vec$wwa2 <- wqs$wwa
work_vec$wsa2 <- wqs$wsa
sent_vec$sqs <- sqs$sqs


work_vec <- work_vec[order(work_vec$work_agr), ]
plot(work_vec$work_agr * work_vec$sent_agr, main = "Worker Quality Score", ylab = "WQS = WWA * WSA")
points(work_vec$wqs, pch = 20, col = "red")
points(work_vec$wwa2, pch = 18, col = "blue")
points(work_vec$work_agr, pch = 19, col = "lightblue")
legend("bottomright", c("v.1.0", "v.2.0", "wwa 1.0", "wwa 2.0"),
       pch = c(1, 20, 19, 18), col = c("black", "red", "lightblue", "blue"))

plot(work_vec$work_agr * work_vec$sent_agr, main = "Worker Quality Score", ylab = "WQS = WWA * WSA")
points(work_vec$wqs, pch = 20, col = "red")
points(work_vec$sent_agr, pch = 19, col = "green")
points(work_vec$wsa2, pch = 18, col = "darkgreen")
legend("bottomright", c("v.1.0", "v.2.0", "wsa 1.0", "wsa 2.0"),
       pch = c(1, 20, 19, 18), col = c("black", "red", "green", "darkgreen"))


sent_vec <- sent_vec[order(sent_vec$clarity), ]
plot(sent_vec$clarity, main = "Sentence Quality Score", ylab = "SQS = mean WWA", ylim = c(0,1))
points(sent_vec$sqs, pch = 20, col = "red")
legend("bottomright", c("v.1.0", "v.2.0"), pch = c(1, 20), col = c("black", "red"))

sent_vec <- sent_vec[order(sent_vec$clarity), ]
sent_vec$clarity_rank <- 1:length(sent_vec$clarity)
sent_vec <- sent_vec[order(sent_vec$sqs), ]
sent_vec$sqs_rank <- 1:length(sent_vec$clarity)
sent_vec$rank_diff <- sent_vec$clarity_rank - sent_vec$sqs_rank
sent_vec <- sent_vec[order(sent_vec$rank_diff), ]

write.csv(sent_vec, "../crowdsourcing/output/merged/sent-vec-sqs.csv", row.names = F)
# write.csv(work_vec, "../crowdsourcing/output/merged/work-vec-wqs.csv", row.names = F)
