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




#list functions

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



#graphing functions




default.plot <- function()
{
  par(oma = c(0,0,0,0))
  par(mar = c(5.1, 4.1, 4.1, 2.1))
  par(mfrow = c(1,1))
  par(bg = "white")
}

plot.2boxplots = function(...)
{
  par(mfrow=c(1,2))
  par(oma=c(0,0,0,0))
  par(mar = c(2.5, 4.1, 2.1, 1.5))
}

uqm.plot1 = function(ls1, main1, ylab1, ls2, main2, ylab2)
{
  default.plot()
  par(oma = c(0, 0, 1.0, 0))
  par(mfrow=c(1,2))
  par(mar = c(4.5, 4.1, 2.1, 1.5))
  boxplot(ls1, main = main1, xlab = "", ylab = ylab1, las = 2, names = report.names, col=report.col)
  boxplot(ls2, main = main2,  xlab = "", ylab = ylab2, las = 2, names = report.names, col=report.col)
}


legend.top = function(leg.names, bar.col)
{
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("top", legend = leg.names,  xpd = TRUE, horiz = TRUE, inset = c(0, 0), fill = bar.col, bty="n")
}

plot.topper.dual = function()
{
  default.plot()
  par(oma = c(0, 0, 1.0, 0))
  par(mfrow=c(1,2))
  par(mar = c(4.5, 4.1, 2.1, 1.5))
}

plot.mean.sem = function(df, dv, iv, y.max, bar.col, x.names, y.name)
{
  means = tapply(df[,dv], df[,iv], mean, na.rm=T)
  st.dev = tapply(df[,dv], df[,iv], sd, na.rm=T)
  n = tapply(df[,dv], df[,iv], length)
  sem = st.dev/sqrt(n)
  
  bars <- barplot(means, names = "", ylab = y.name, ylim = c(0,y.max), col = bar.col)
  for (i in 1:length(bars)) 
  {
    arrows(bars[i],means[i],bars[i],means[i]+sem[i],angle=90,length=.09,lwd = 1.5)
    arrows(bars[i],means[i],bars[i],means[i]-sem[i],angle=90,length=.09,lwd = 1.5)
  }
  axis(1, at = bars, labels = x.names, las = 2)
}


plot.mean.sem.adj = function(df, dv, iv, y.max, bar.col, x.names, y.name, adjustor)
{
  means = tapply(df[,dv], df[,iv], mean, na.rm=T)
  means = means/adjustor
  st.dev = tapply(df[,dv], df[,iv], sd, na.rm=T)
  n = tapply(df[,dv], df[,iv], length)
  sem = st.dev/sqrt(n)
  sem = sem/adjustor
  
  bars <- barplot(means, names = "", ylab = y.name, ylim = c(0,y.max), col = bar.col)
  for (i in 1:length(bars)) 
  {
    arrows(bars[i],means[i],bars[i],means[i]+sem[i],angle=90,length=.125,lwd = 1.5)
    arrows(bars[i],means[i],bars[i],means[i]-sem[i],angle=90,length=.125,lwd = 1.5)
  }
  axis(1, at = bars, labels = x.names, las = 2)
}



#log sequences 

my.logs = function(start, end)
{
  logs = seq(start, end, 1) 
  if (start < -4){
    msec.end = which(logs == -5)
    sec.start = which(logs == -4) 
  } else  { sec.start = 1 }
  
  sec.end = which(logs == -1)
  min.start = which(logs == 0)
  min.end = which(logs == 4)
  hr.start = which(logs == 5)
  if (end <= 7){
    hr.end = length(logs)
    day.start = NULL
    day.end = NULL
  }
  
  if (end > 7){
    hr.end = which(logs == 7)
    day.start = which(logs == 8)
    day.end = length(logs)
  }
    
  l = sapply(logs, exp)
  names(l) = logs
  if (start < -4){
  for (i in 1:msec.end)
    l[i] = l[i]*(1000*60)  
  }
  for (i in sec.start:sec.end)
    l[i] = l[i]*60
  for (i in hr.start:hr.end)
    l[i] = l[i]/60
  if (end > 7){
    for (i in day.start:day.end)
      l[i] = l[i]/(60*24)
  }
  l = round(l,0)
  
  l = as.character(l)

  if (start < -4){
    for (i in 1:msec.end)
      l[i] = paste(l[i], "msec")
  }
  for (i in sec.start:sec.end)
    l[i] = paste(l[i], "sec")
  for (i in min.start:min.end)
    l[i] = paste(l[i], "min")
  for (i in hr.start:hr.end)
    l[i] = paste(l[i], "hr")
  
  if (end > 7){
    for (i in day.start:day.end)
      l[i] = paste(l[i], "days")
  }
  
  names(l) = logs
  return(l)
}
