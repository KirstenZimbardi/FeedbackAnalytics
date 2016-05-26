#useful/helper functions

"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y

#automating

dup = function(df, col.dup, col.return) {
  return(unique(df[(which(duplicated(df[,col.dup]))),col.return]))
}

rename = function(df, col, name)
{
  n = names(df)
  n[col] = name
  names(df) = n
  return(df)
}

remover = function(df, col)
{
  df2 = df
  remove = rev(which(is.na(df[,col])))
  if (length(remove) > 0)
  {
    df2 = df[-remove,]
  }
  return(df2)
}

summarise = function(df, dv, adj, iv, var)
{
  df2 = as.data.frame(tapply(df[,dv], df[,iv], sum, na.rm=T))
  df2 = df2/adj
  SubmissionID = rownames(df2)
  df2 = cbind(SubmissionID, df2)
  df2 = rename(df2, 2, var)
  return(df2)
}

notfinal = function(df, new.col, project.col)
{
  df[,new.col] = "notfinal"
  for (i in 1:nrow(df))
  {
    if (df[i,project.col] == "BIOL1040Sem1Report 4")
      df[i,new.col] = "final"
    if (df[i,project.col] == "BIOL1040Sem2Report 3")
      df[i,new.col] = "final"
    if (df[i,project.col] == "BIOM2011Sem1Report 2")
      df[i,new.col] = "final"
    if (df[i,project.col] == "BIOM2011Sem2Report 2")
      df[i,new.col] = "final"
  }
  df = rename(df, new.col, "final.report")
  return(df)
}


#descrptive stats

ls.descriptives = function(ls)
{
  ls.mean = sapply(ls, mean)
  ls.sd = sapply(ls, sd)
  ls.n = sapply(ls, length)
  ls.se = ls.sd/ls.n
  df = as.data.frame(cbind(ls.mean, ls.sd, ls.n, ls.se))
  return(df)
}

se <- function(df, col)
{
  x = df[,col]
  sem = sd(x, na.rm=T)/sqrt(length(x))
  return(sem)
}

stats.mean.sem = function(df, dv, iv)
{
  means = tapply(df[,dv], df[,iv], mean, na.rm=T)
  st.dev = tapply(df[,dv], df[,iv], sd, na.rm=T)
  n = tapply(df[,dv], df[,iv], length)
  SEM = st.dev/sqrt(n)
  stats = as.data.frame(means)
  stats = cbind(stats, SEM)
  return(stats)
}

stats.mean.sem.n = function(df, dv, iv)
{
  means = tapply(df[,dv], df[,iv], mean, na.rm=T)
  st.dev = tapply(df[,dv], df[,iv], sd, na.rm=T)
  n = tapply(df[,dv], df[,iv], length)
  SEM = st.dev/sqrt(n)
  stats = as.data.frame(means)
  stats = cbind(stats, SEM, n)
  return(stats)
}

dep.variables = function()
{
  criteria = "Hypoth,Methods.writing,Methods.details,Methods.design,Results.text,Results.figures,Results.legends,Disc.knowlede,Disc.InterpFindings,Disc.Evidence,Refs,Writing"
}

#comparitive stats
#t test p value extraction
tp = function(t) {
  if(t$p.value <= 0.001) { t$p.value = "<0.001" }
  return(t$p.value)
}

#anova p value extraction
ap = function(a) {
  if(summary(a)[[1]][1,5] <= 0.001) { p = "<0.001" }
  return(p)
}

