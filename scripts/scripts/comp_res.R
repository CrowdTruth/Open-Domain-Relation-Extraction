baseline_v1_pos <- read.csv("../data/pilot/baseline/sent-vec_pos.csv", header = T, sep = ",",
                            dec = ".", fill = TRUE, stringsAsFactors=FALSE)
baseline_v1_neg <- read.csv("../data/pilot/baseline/sent-vec_neg.csv", header = T, sep = ",",
                            dec = ".", fill = TRUE, stringsAsFactors=FALSE)

baseline_v3_pos <- read.csv("../data/pilot/baseline3/sent-vec_pos.csv", header = T, sep = ",",
                            dec = ".", fill = TRUE, stringsAsFactors=FALSE)
baseline_v3_neg <- read.csv("../data/pilot/baseline3/sent-vec_neg.csv", header = T, sep = ",",
                            dec = ".", fill = TRUE, stringsAsFactors=FALSE)

baseline_v1_pos <- baseline_v1_pos[order(baseline_v1_pos$sent_id), ]
baseline_v1_neg <- baseline_v1_neg[order(baseline_v1_neg$sent_id), ]
baseline_v3_pos <- baseline_v3_pos[order(baseline_v3_pos$sent_id), ]
baseline_v3_neg <- baseline_v3_neg[order(baseline_v3_neg$sent_id), ]

comp <- baseline_v1_pos$max_cos - baseline_v3_pos$max_cos
comp <- append(comp, baseline_v1_neg$max_cos - baseline_v3_neg$max_cos)
comp <- comp[order(comp)]
plot(comp)