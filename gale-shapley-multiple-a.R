
library(matchingR)


calc_spouse_cors <- function(n, a_min, a_max) {
  men <- cbind(x1 = rnorm(n), x2 = rnorm(n), a = runif(n, a_min, a_max))
  women <- cbind(x1 = rnorm(n), x2 = rnorm(n), a = runif(n, a_min, a_max))
  
  proposer_utils <- matrix(NA, n, n)
  reviewer_utils <- matrix(NA, n, n)
  
  for (m in 1:n) {
    his_a <- men[[m, "a"]]
    his_utility_from_women <- his_a * women[,"x1"] + (1 - his_a) * women[, "x2"]
    proposer_utils[, m] <- his_utility_from_women
  }
  
  for (f in 1:n) {
    her_a <- women[[f, "a"]]
    her_utility_from_men <- her_a * men[,"x1"] + (1 - her_a) * men[, "x2"]
    reviewer_utils[, f] <- her_utility_from_men
  }
  
  
  result <- galeShapley.marriageMarket(proposerUtils = proposer_utils, reviewerUtils = reviewer_utils)
  
  spouses <- women[result$proposals, ]
  colnames(spouses) <- c("spouse_x1", "spouse_x2", "spouse_a")
  couples <- as.data.frame(cbind(men, spouses))
  cor_matrix <- cor(couples)
  
  c(
    x1x1 = cor_matrix["x1", "spouse_x1"],
    x1x2 = cor_matrix["x1", "spouse_x2"],
    ax1  = cor_matrix["a", "spouse_x1"],
    ax2  = cor_matrix["a", "spouse_x2"],
    aa  = cor_matrix["a", "spouse_a"]
  )
}

rm(all_res)
for (a_min in seq(0, 1.0, 0.1)) for (a_max in seq(a_min, 1.0, 0.1)) {
  res <- replicate(20, calc_spouse_cors(n = 250, a_min = a_min, a_max = a_max)) 
  res <- rowMeans(res)
  res <- c(a_min = a_min, a_max = a_max, res)
  all_res <- if (! exists("all_res")) res else rbind(all_res, res)
}
all_res <- as.data.frame(all_res)


# basically, variation doesn't make much difference; what matters is the
# mean of a_min and a_max; correlations are largest at 0.5
ggplot(all_res, aes(a_min, a_max, colour = x1x2)) + geom_point(size = 5)

all_res$a_mean <- all_res$a_min/2 + all_res$a_max/2
all_res$a_gap <- all_res$a_max - all_res$a_min
ggplot(all_res, aes(a_mean, a_gap, colour = x1x2)) + geom_point(size = 5)
