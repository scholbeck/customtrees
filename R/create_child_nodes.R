create_child_nodes = function(parent_split_result) {
  
  split.feature = parent_split_result$feature[parent_split_result$best.split]
  print(split.feature)
  if (length(split.feature) > 1) {
    split.feature = sample(split.feature, 1)
  } else {
  }
  split.value = parent_split_result$split.points[which(parent_split_result$feature == split.feature)]
  print(split.value)
  
  data.subset.1 = data[data[ , split.feature] <= split.value, ]
  print(nrow(data.subset.1))
  data.subset.2 = data[data[ , split.feature] > split.value, ]
  print(nrow(data.subset.2))
  
  return(list(data.subset.1, data.subset.2))
}