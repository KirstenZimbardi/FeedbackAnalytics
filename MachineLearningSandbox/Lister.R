# need to melt, cast and merge 4 times for 4 courseXsem - trying to abstract to function

lister = function(df) {
  cs = as.factor(paste(df$course, df$sem))
  cs.ls = NULL
  cs.ls = split(df, cs)
  
  last.ls = NULL
  for (l in 1:length(cs.ls)) {
    ls.11 = NULL
    for (i in 1:length(dv)) {
      ls.11[[i]] = melt(cs.ls[[l]][,c(1,4, (i+4))], id=c("StudentID", "final"), value.name = dv[i])
    }
    
    ls.11b = NULL
    for (i in 1:length(dv)) {
      ls.11b[[i]] = dcast(ls.11[[i]], StudentID~variable+final)
    }
    
    df2 = ls.11b[[1]]
    for (i in 2:length(ls.11b)) {
      df2 = merge(df2, ls.11b[[i]], by = "StudentID", all.x=T)
    }
    
    df2$cs = names(cs.ls)[l]
    
    last.ls[[l]] = df2
  }
  
}