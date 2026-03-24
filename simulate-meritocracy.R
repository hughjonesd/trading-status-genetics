

## setup ####

library(tidyr)
library(purrr)
library(dplyr)
library(ggplot2)

match_couples <- function (a, pop) {
  attractiveness <- a*pop$x_1 + (1-a)*pop$x_2
  pop <- pop[order(attractiveness),]
  
  females <- pop[! pop$male, c("x_1", "x_2")] # ordered by attr
  names(females) <- c("y_1", "y_2")
  couples <- cbind(pop[pop$male,],  females) # match males ordered by attr
  
  return(couples)
}

children <- function (tau, theta, gamma, couples) {
  n <- nrow(couples) * 2
  child_x_1 <- rep(couples$x_1/2 + couples$y_1/2, each = 2) * tau + rnorm(n)
  child_x_2 <- rep(couples$x_2/2 + couples$y_2/2, each = 2) * theta + gamma * child_x_1 + rnorm(n)
  male_line_id <- rep(couples$male_line_id, each = 2)
  
  # one male one female per couple:
  data.frame(x_1 = child_x_1, x_2 = child_x_2, male = rep(c(FALSE, TRUE), n/2), male_line_id = male_line_id) 
}

simulate <- function(n, a, tau, theta, gamma, n_gens) {
  
  x_1 <- rnorm(n)
  x_2 <- rnorm(n)
  male <- rep(c(FALSE, TRUE), n/2)
  male_line_id <- 1:n
  pop <- data.frame(x_1 = x_1, x_2 = x_2, male = male, male_line_id = male_line_id)
  
  # get to equilibrium, we hope
  for (gen in 1:n_gens) {
    couples <- match_couples(a = a, pop = pop)
    pop <- children(tau = tau, theta = theta, gamma = gamma, couples = couples)
    # if (! gen %% 10) {
    #   cat(sd(pop$x_1), sd(pop$x_2), cor(pop$x_1, pop$x_2), "\n", sep = " ")
    # }
  }
  
  # grandparents, parents, children
  grandparents <- pop
  grandparents$id <- 1:n
  gp_couples <- match_couples(a = a, pop = grandparents) # will have an id of the males only
  parents <- children(tau = tau, theta = theta, gamma = gamma, couples = gp_couples)
  parents$id <- rep(gp_couples$id, each = 2) # male grandparent's id
  parent_couples <- match_couples(a = a, pop = parents) # again will have the id from the male
  kids <- children(tau = tau, theta = theta, gamma = gamma, couples = parent_couples)
  kids$id <- rep(parent_couples$id, each = 2) # male grandparent's id
  
  names(grandparents) <- c("gp_x_1", "gp_x_2", "gp_male", "gp_id")
  names(parents) <- c("parent_x_1", "parent_x_2", "parent_male", "parent_id")
  parent_couples$mean_x_1 <- parent_couples$x_1/2 + parent_couples$y_1/2
  parent_couples$mean_x_2 <- parent_couples$x_2/2 + parent_couples$y_2/2
  # merges male gp only
  gp_parents <- merge(grandparents, parent_couples[c("id", "mean_x_1", "mean_x_2")], 
                      by.x = "gp_id", by.y = "id", sort = FALSE)
  
  gp_p_kids <- merge(gp_parents, kids, by.x = "gp_id", by.y = "id", sort = FALSE)
  
  gp_kid_cor <- cor(gp_p_kids$gp_x_2, gp_p_kids$x_2)
  m <- lm(x_2 ~ gp_x_2 + mean_x_2, data = gp_p_kids)
  gp_kid_partial <- coef(m)[2]
  
  x1_x2_cor <- cor(kids$x_1, kids$x_2)
  
  results <- data.frame(
    gp_kid_cor = gp_kid_cor, 
    gp_kid_partial = gp_kid_partial,
    x1_x2_cor = x1_x2_cor
    )
  
  return(results)
}



## simulate a policy shock ####

simulate_shock <- function (n, a, tau, theta, gamma, n_gens_pre, n_gens_shock, n_gens_post,
                            theta_shock = theta, gamma_shock = gamma) {
  
  x_1 <- rnorm(n)
  x_2 <- rnorm(n)
  male <- rep(c(FALSE, TRUE), n/2)
  male_line_id <- 1:n
  pop <- data.frame(x_1 = x_1, x_2 = x_2, male = male, male_line_id = male_line_id)
  
  # stats <- data.frame(gen = integer(0), x_1_x_2_cor = numeric(0), x_1_sd = numeric(0), x_2_sd = numeric(0))
  # update_stats <- function(stats, pop, couples) {
  #   x_2_parent_child <- if (! is.null(couples)) {
  #     tmp <- left_join(pop, couples, by = "male_line_id", suffix = c(".child", ".parent"))
  #     cor(tmp$x_2.parent, tmp$x_2.child)
  #   } else {
  #     NA
  #   }
  #   new_stats <- data.frame(
  #     gen = nrow(stats) + 1,
  #     x_1_x_2_cor = cor(pop$x_1, pop$x_2),
  #     x_1_sd = sd(pop$x_1),
  #     x_2_sd = sd(pop$x_2),
  #     x_2_parent_child = x_2_parent_child
  #   )
  #   rbind(stats, new_stats)
  # }
  stats <- pop[0, ] # zero row data to start
  stats$gen <- numeric(0)
  update_stats <- function(stats, pop, couples) {
    pop$gen <- if (length(stats$gen)) {max(stats$gen) + 1} else 1
    rbind(stats, pop)
  }
  stats <- update_stats(stats, pop, NULL)
  
  # get to equilibrium, we hope
  # first generation is before matching
  for (gen in seq_len(n_gens_pre - 1)) {
    couples <- match_couples(a = a, pop = pop)
    pop <- children(tau = tau, theta = theta, gamma = gamma, couples = couples)
    stats <- update_stats(stats, pop, couples)
  }
  
  for (gen in seq_len(n_gens_shock)) {
    couples <- match_couples(a = a, pop = pop)
    pop <- children(tau = tau, theta = theta_shock, gamma = gamma_shock, couples = couples)
    stats <- update_stats(stats, pop, couples)
  }
  
  for (gen in seq_len(n_gens_post)) {
    couples <- match_couples(a = a, pop = pop)
    pop <- children(tau = tau, theta = theta, gamma = gamma, couples = couples)
    stats <- update_stats(stats, pop, couples)
  }
  
  stats$state <- cut(stats$gen, c(1, n_gens_pre, 
                   #                 n_gens_pre+n_gens_shock, 
                                    n_gens_pre + n_gens_shock + n_gens_post),
                     include.lowest = TRUE)
  
  stats
}

n <- 10000
tau <- 0.98
theta <- 0.8
gamma <- 0.1
theta_shock <- 0.8
gamma_shock <- 0.1
n_gens_pre <- 100
n_gens_shock <- 3
n_gens_post <- 50
stats_sgam <- simulate_shock(n = n, a = 0.3, tau = tau, 
                             theta = theta, gamma = gamma, 
                             n_gens_pre = n_gens_pre, n_gens_shock = n_gens_shock, 
                             n_gens_post = n_gens_post,
                             theta_shock = theta_shock, gamma_shock = gamma_shock)

stats_gam  <- simulate_shock(n = n, a = 1, tau = tau, 
                             theta = theta, gamma = gamma, 
                             n_gens_pre = n_gens_pre, n_gens_shock = n_gens_shock, 
                             n_gens_post = n_gens_post,
                             theta_shock = theta_shock, gamma_shock = gamma_shock)

stats_all <- bind_rows(GAM = stats_gam, SGAM = stats_sgam, .id = "model")

gen100 <- stats_all |> filter(gen == 100, male == TRUE)
stats_all <- left_join(stats_all, 
                       gen100 |> select(model, male_line_id, x_1, x_2),
                       by = c("model", "male_line_id"),
                       suffix = c("", "_gen100"))
stats_all |> 
  filter( male == TRUE) |> 
  mutate(.by = model,
    qtile_gen100 = cut(x_2_gen100, 4, labels = 1:4)
  ) |> 
  mutate(.by = c(model, gen),
            x_1_norm = c(scale(x_1)),
            x_2_norm = c(scale(x_2)),
            ) |> 
  summarize(.by = c(model, gen, qtile_gen100),
            mean_x_1 = mean(x_1_norm),
            mean_x_2 = mean(x_2_norm)
            ) |> 
  ggplot(aes(gen, mean_x_2, colour = qtile_gen100)) + 
  geom_line() + facet_wrap(vars(model))

ggplot(stats_all, aes(gen, x_2_parent_child, color = state, shape = model)) + 
  geom_point() + geom_line()


## run 100 generations, look at results ####
params <- tidyr::crossing(a      = seq(0, 1, 0.1), 
                          theta  = seq(0, 0.4, 0.2), 
                          gamma  = seq(0, 0.8, 0.1), 
                          n      = 500, 
                          tau    = 0.98,
                          n_gens = 100,
                          rep    = 1:20)
res <- params |> select(-rep) |> pmap(simulate, .progress = TRUE)
res <- list_rbind(res)
res <- cbind(params, res)

res |> 
  summarize(.by = c(a, theta, gamma), 
            gp_kid_cor = mean(gp_kid_cor),
            gp_kid_partial = mean(gp_kid_partial),
            x1_x2_cor = mean(x1_x2_cor)
            ) |> 
  ggplot(aes(a, x1_x2_cor, colour = gamma, group = gamma)) + 
    geom_line() + facet_wrap(vars(theta), labeller = label_both)
