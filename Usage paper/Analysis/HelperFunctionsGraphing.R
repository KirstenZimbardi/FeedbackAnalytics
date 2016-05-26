#Useful graphing functions - developed for UQM usage paper

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

legend.top.hatch = function(leg.names, bar.col, c, a, d)
{
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("top", legend = leg.names,  xpd = TRUE, horiz = TRUE, inset = c(0, 0), fill = TRUE , col = c, angle=a, density=d, bty="n")
}

plot.topper.dual = function()
{
  default.plot()
  par(oma = c(0, 0, 1.0, 0))
  par(mfrow=c(1,2))
  par(mar = c(4.5, 4.1, 2.1, 1.5))
}

plot.topper.dual.longY = function()
{
  default.plot()
  par(oma = c(0, 0, 1.0, 0))
  par(mfrow=c(1,2))
  par(mar = c(4.5, 6.1, 2.1, 1.5))
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
