#useful/helper functions

#automating

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


#converting list to df

ls.df = function(ls, adj)
{
  for (l in 1:length(ls))
  {
    col = length(ls[[l]])
    cols = seq(col+1, col+4, 1)
    l2 = l + adj
    ls[[l]][,cols[1]] = project.names.formative[l2]
    ls[[l]][,cols[2]] = substr(ls[[l]][,cols[1]], 1, 8)
    ls[[l]][,cols[3]] = substr(ls[[l]][,cols[1]], 9, 12)
    ls[[l]][,cols[4]] = substr(ls[[l]][,cols[1]], 13, 20)
  } 
  col.names = c("project", "course", "sem", "report")
  for (l in 1:length(ls))
    for (i in 1:length(cols))
  {
    ls[[l]] = rename(ls[[l]], cols[i], col.names[i])
  }
  df = ls[[1]]
  for (l in 2:length(ls))
  {
    df = rbind(df, ls[[l]])
  }
  return(df)
}


#list functions

mark.convert = function(ls, col1, col2)
{
  for (l in 1:length(ls))
    for (j in col1:col2)
      for (i in 1:nrow(ls[[l]]))
      {
        if (ls[[l]][i,j] == 100)
          ls[[l]][i,j] = 4
        if (ls[[l]][i,j] == 80)
          ls[[l]][i,j] = 3
        if (ls[[l]][i,j] == 60)
          ls[[l]][i,j] = 2
        if (ls[[l]][i,j] == 40)
          ls[[l]][i,j] = 1
        if (ls[[l]][i,j] == 20)
          ls[[l]][i,j] = 0
      }
  return(ls)
}


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

dep.variables = function()
{
  criteria = "Hypoth,Methods.writing,Methods.details,Methods.design,Results.text,Results.figures,Results.legends,Disc.knowlede,Disc.InterpFindings,Disc.Evidence,Refs,Writing"
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

my.logs = function(start, end, sec.end, hr.start)
{
  logs = seq(start, end, 1)  
  hr.end = length(logs)
  logs.t = sapply(logs, exp)
  names(logs.t) = logs
  logs.convert = logs.t
  l = logs.convert
  for (i in 1:sec.end)
    logs.convert[i] = l[i]*60
  for (i in hr.start:hr.end)
    logs.convert[i] = l[i]/60
  logs.convert = round(logs.convert,0)
  
  min.start = sec.end+1
  min.end = hr.start-1
  l = logs.convert
  l.n = as.character(l)
  for (i in 1:sec.end)
    l.n[i] = paste(l[i], "sec")
  for (i in min.start:min.end)
    l.n[i] = paste(l[i], "min")
  for (i in hr.start:hr.end)
    l.n[i] = paste(l[i], "hr")
  names(l.n) = logs
  return(l.n)
}
