# Split vector into train, cv and test @ 60:20:20
splitter = function(vec, train.ratio)
{
  require(caTools)
  split = sample.split(vec, SplitRatio = (train.ratio/100))
  
  # Split up the data using subset
  train = subset(vec, split==TRUE)
  test.all = subset(vec, split==FALSE)
  
  # Split into cv and test
  split = sample.split(test.all, SplitRatio = 0.50)
  cv = subset(test.all, split==TRUE)
  test = subset(test.all, split==FALSE)
  
  # Output split sets
  splits <- list(train = train, cv = cv, test = test)
  return(splits)
}

# Analysis of confusion matirx 

conMatrixStats = function(cm)
{
  accuracy = 100 * (cm['FALSE','FALSE'] + cm['TRUE','TRUE']) / cm['Sum','Sum'] # how often corret
  misclassification = 100 * (cm['FALSE','TRUE'] + cm['TRUE','FALSE']) / cm['Sum','Sum'] # how often incorrect
  true.pos = 100 * (cm['TRUE','TRUE']/ cm['TRUE','Sum']) # aka sensitivity or recall
  false.pos = 100 * (cm['FALSE','TRUE'] / cm['FALSE','Sum'])
  specificity = 100 * (cm['FALSE','FALSE'] / cm['FALSE','Sum']) # ie true neg (= 1 - false.pos)
  precision = 100 * (cm['TRUE','TRUE'] / cm['Sum','TRUE']) # when predicts yes, how often is it correct
  prevalence = 100 * (cm['TRUE','Sum'] / cm['Sum','Sum']) # how often does the yes condition actually occur in the sample
  cmStats = data.frame(rbind(accuracy, misclassification, prevalence, precision, true.pos, false.pos, specificity))
  names(cmStats) = "Results"
  return(cmStats)
}
