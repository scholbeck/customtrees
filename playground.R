# devtools::install_github("christophM/iml", ref = "marginal")
library(mlr)
library(devtools)
library(tidyverse)
library(Rmalschains)
library(iml)
library(ranger)
library(kmlShape)
library(dtw)
load_all()

lrn = makeLearner("regr.rpart")

library(mlr)
library(printr)
library(MASS)
n = nrow(Boston)
data = Boston[sample(1:nrow(Boston), n), ]
data$chas = as.numeric(data$chas)
tsk = makeRegrTask(data = data, target = "medv")
mod = train("regr.randomForest", tsk)

marginalEffects <- function(mod, data, feature, step) {
  pred_init <- predict(mod$learner.model, data)
  data_intervened <- data
  data_intervened[ , feature] <- data_intervened[ , feature] + step
  
  pred_intervened <- predict(mod$learner.model, data_intervened)
  
  marginal_effects <- pred_init - pred_intervened
  
  # mse <- lapply(1:nrow(data), FUN = function(obs) {
  #   interval_width <- step / k
  #   slope = marginal_effects[obs] / step
  #   preds_secant <- pred_init[obs] + slope * interval_width*(1:k)
  #
  #   preds_fn <- unlist(lapply(1:k, FUN = function(i) {
  #     feature_values <- data[obs, ]
  #     feature_values[, feature] <- data[obs, feature] + i * interval_width
  #     predict(mod$learner.model, feature_values)
  #   }))
  #
  #   # mse = mean(sqrt((preds_secant - preds_fn)^2))
  #   return(mse)
  # })
  
  # mse <- unlist(mse)
  # universal_effects <- data.frame("MarginalEffect" = marginal_effects,
  # "MSE" = mse)
  # return(universal_effects)
  return(marginal_effects)
}
SS = function(x, y) {
  var(y)
}

# marginals = marginalEffects(mod, data, "rm", step = 1)
# parent_node = data

ame_list = list()

recursive_binary_split = function(parent_node) {
  str(parent_node)
  marginals = marginalEffects(mod, parent_node, "rm", step = 1)
  split = split_parent_node(marginals, parent_node, objective = SS, optimizer = find_best_binary_split)
  child.nodes = create_child_nodes(split)
  child.node.1 = child.nodes[[1]]
  child.node.2 = child.nodes[[2]]
  marginals.child.1 = marginalEffects(mod, child.node.1, "rm", step = 1)
  # print(var(marginals.child.1))
  marginals.child.2 = marginalEffects(mod, child.node.2, "rm", step = 1)
  # print(var(marginals.child.2))
  
  if (var(marginals.child.1) >= 5) {
    recursive_binary_split(parent_node = child.node.1)
  } else {
    ame_list = append(ame_list, mean(marginals.child.1))
  }
  if (var(marginals.child.2) >= 5) {
    recursive_binary_split(parent_node = child.node.2)
  } else {
    ame_list = append(ame_list, mean(marginals.child.2))
  }
  return(ame_list)
}


recursive_binary_split(data)

split_parent_node(marginals.child.1, childs[[1]], objective = SS, optimizer = find_best_binary_split)
split_parent_node(marginals.child.2, childs[[2]], objective = SS, optimizer = find_best_binary_split)



split.list = list()

for (i in 1:2) {
  split = split_parent_node(res, data, objective = SS, optimizer = find_best_binary_split)
  childs = create_child_nodes(split)
  
  marginals.child.1 = marginalEffects(mod, childs[[1]], "rm", step = 1)
  print(var(marginals.child.1))
  marginals.child.2 = marginalEffects(mod, childs[[2]], "rm", step = 1)
  print(var(marginals.child.2))
  
  if (var(marginals.child.1) >= 1) {
    split.child.1 = split_parent_node(marginals.child.1, childs[[1]], objective = SS, optimizer = find_best_binary_split)
    childs = create_child_nodes(split.child.1)
    marginals.child.1 = marginalEffects(mod, childs[[1]], "rm", step = 1)
    print(var(marginals.child.1))
    marginals.child.2 = marginalEffects(mod, childs[[2]], "rm", step = 1)
    print(var(marginals.child.2))
  } else {
    return(list(split.child.1)
  }
  if (var(marginals.child.1) >= 1) {
    split.child.2 = split_parent_node(marginals.child.2, childs[[2]], objective = SS, optimizer = find_best_binary_split)
  } else {
    return(split.child.2)
  }
}

  

summary(res)
sd(res)
var(res)
SS = function(x, y) {
  var(y)
}
var(res)
split = split_parent_node(res, data.subset.2, objective = SS, optimizer = find_best_binary_split)
