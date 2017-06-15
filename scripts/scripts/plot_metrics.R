source("crowdtruth_metrics.R")

fig_folder <- "/home/anca/Documents/Open-Domain-Relation-Extraction/fig/test/baseline"

####### plots #######

for (idx in 1:(length(relations) - 1)) {
  rel <- relations[idx]
  sent_vec <- sent_vec[order(sent_vec[, paste(rel, "cos", sep = "_")]), ]
  
  ## plot agreement sent-rel score
  baseline_pos_id <- baseline_data[baseline_data$relation %in% c(rel), ]$sent_id
  crowd_pos <- sent_vec[sent_vec$sent_id %in% baseline_pos_id, ]
  crowd_neg <- sent_vec[! sent_vec$sent_id %in% baseline_pos_id, ]
  
  png(paste(fig_folder, "/rel-overlap/", rel, ".png", sep = ""))
  plot((1:length(crowd_pos[, 1])) / length(crowd_pos[, 1]), crowd_pos[, paste(rel, "cos", sep = "_")] + 0.02,
       pch = 20, col = "darkblue", ylab = "sen-rel score", xlab = "",
       main = paste("test results:", rel), ylim = c(0, 1))
  points((1:length(crowd_neg[, 1])) / length(crowd_neg[, 1]), crowd_neg[, paste(rel, "cos", sep = "_")],
         pch = 18, col = "red")
  legend("topleft", c("positive", "negative"), title = "distant supervision", pch = c(20, 18),
         col = c("darkblue", "red"))
  dev.off()
  
  ## plot rel overlap
  comparison_rels <- setdiff(relations, c(rel))
  comparison_rels_count <- rep(0, length(setdiff(relations, c(rel))))
  for (jdx in 1:length(sent_vec[, 1])) {
    top_rels <- strsplit(sent_vec$max_rel[jdx], ",")[[1]]
    #     print(top_rels)
    if (rel %in% top_rels) {
      for (kdx in 1:length(comparison_rels)) {
        if (comparison_rels[kdx] %in% top_rels) {
          comparison_rels_count[kdx] <- comparison_rels_count[kdx] + 1
        }
      }
    }
  }
}
