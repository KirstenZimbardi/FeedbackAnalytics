#Useful list functions - developed for UQM Usage paper

ls.names = function(ls)
{
  n = NULL
  n = as.list(n)
  for (i in 1:length(ls))
    n[[i]] = names(ls[[i]])
  print(n)
}


ls.rename = function(ls, col, name)
{
  n = names(ls[[1]])
  n[col] = name
  for (l in 1:length(ls))
    names(ls[[l]]) = n
  return(ls)
}

ls.dim = function(ls)
{
  for (l in 1:length(ls))
    print(dim(ls[[l]]))
}
