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
