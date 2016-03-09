out.aov.acp.open = function() { 

df = dcast(AcPFbUbins, StudentID + project + open.bin ~ report, value.var = "Final.Grade")
#head(df)

open.bins = c("unopened", "short", "medium", "long")

#BIOL1040 Sem 1
df = subset(AcPFbUbins, report == "Report 0")
df = df[,c(2,5,10:11)]

#default.plot()
#with(df, boxplot(Final.Grade ~ open.bin))

df[,4] = factor(df[,4], open.bins)

#default.plot()
#plot.mean.sem(df, 2, 4, 100, kz.col, open.bins, "Report mark (%)")
#legend.top(open.bins,  kz.col)

df2 = df[,c(1,4,2)]
df2 = rename(df2, 3, "Report0.Grade")

df2 = merge(df2, subset(AcPFbUbins, report == "Report 1")[,c(2,5)], by="StudentID")
df2 = rename(df2, 4, "Report1.Grade")

df2 = merge(df2, subset(AcPFbUbins, report == "Report 2")[,c(2,5)], by="StudentID")
df2 = rename(df2, 5, "Report2.Grade")

df2 = merge(df2, subset(AcPFbUbins, report == "Report 3")[,c(2,5)], by="StudentID")
df2 = rename(df2, 6, "Report3.Grade")

df3 = melt(df2)
#head(df3)
#with(df3, tapply(value, open.bin, mean))

stats = stats.mean.sem(df3, 4, 2:3)
stats = round(stats,1)
#stats

stats.R0 = stats.mean.sem.n(df2, 3, 2)
stats.R1 = stats.mean.sem.n(df2, 4, 2)
stats.R2 = stats.mean.sem.n(df2, 5, 2)
stats.R3 = stats.mean.sem.n(df2, 6, 2)

stats.R0[,4] = "Report 0"
stats.R1[,4] = "Report 1"
stats.R2[,4] = "Report 2"
stats.R3[,4] = "Report 3"

stats.R0[,5] = row.names(stats.R0)
stats.R1[,5] = row.names(stats.R1)
stats.R2[,5] = row.names(stats.R2)
stats.R3[,5] = row.names(stats.R3)

stats.R = rbind(stats.R0, stats.R1, stats.R2, stats.R3)
stats.R = rename(stats.R, 4:5, c("report", "R0bin"))

stats.R = stats.R[,c(4,5,1:3)]
row.names(stats.R) = 1:nrow(stats.R)

##2 way ANOVA

#df4 = AcP %>% filter(course == "Level 1", sem == "Semester 1")
t = aov(df3[,4] ~ df3[,2]*df3[,3])
summary(t)
p = ap(t)
tuk = TukeyHSD(t)
tuk2 = as.data.frame(tuk[[3]])
tuk2[,5] = row.names(tuk2)


pos1 = gregexpr(":", tuk2[,5])
pos2 = NULL
pos3 = NULL
for (l in 1:length(pos1)) {
  pos2[l] = pos1[[l]][1]
  pos3[l] = pos1[[l]][2]
}

pos1.2 = gregexpr("-", tuk2[,5])
pos2.2 = NULL
pos3.2 = NULL
for (l in 1:length(pos1)) {
  pos2.2[l] = pos1.2[[l]][1]
  pos3.2[l] = nchar(tuk2[l,5])
}
tuk2[,6] = substr(tuk2[,5], 1, (pos2-1))
tuk2[,7] = substr(tuk2[,5], (pos2+1), (pos2.2-1))
tuk2[,8] = substr(tuk2[,5], (pos2.2+1), (pos3-1))
tuk2[,9] = substr(tuk2[,5], (pos3+1), pos3.2)

tuk2[,10] = tuk2[,4] < 0.05
row.names(tuk2) = 1:nrow(tuk2)

#tuk2[which(tuk2[,10] == TRUE),c(4,6:10)]
tuk2 = tuk2[,c(8,9,6,7,4,10)]
tuk2 = rename(tuk2, 1:6, c("R0binA", "reportA", "R0binB", "reportB", "p-adj", "sig"))
#table(tuk2[,6])
tuk3 = subset(tuk2, sig == TRUE)

##outputs - infulence of open bin
out.biol1.1 = NULL
out.biol1.1[[1]] = subset(tuk2, R0binA == "unopened" & R0binB == "unopened")
out.biol1.1[[2]] = subset(tuk3, R0binA == "unopened" & R0binB == "unopened")
out.biol1.1[[3]] = subset(stats.R, R0bin == "unopened")
#so if they did not open R0, regardless of what they did later, they only improved from R1 to R2 and then plateued

out.biol1.2 = NULL
out.biol1.2[[1]] = subset(tuk2, R0binA == "short" & R0binB == "short")
out.biol1.2[[2]] = subset(tuk3, R0binA == "short" & R0binB == "short")
out.biol1.2[[3]] = subset(stats.R, R0bin == "short")
#if they looked at R0 for less than 1 minute then they never improved (there are 26 students in this category so not trivial or too small for stats - altho R3 may be a jumpt up from R0 and R1 if the p-adjusted should be trusted at 0.05 instead of 0.05/120)

out.biol1.3 = NULL
out.biol1.3[[1]] = subset(tuk2, R0binA == "medium" & R0binB == "medium")
out.biol1.3[[2]] = subset(tuk3, R0binA == "medium" & R0binB == "medium")
out.biol1.3[[3]] = subset(stats.R, R0bin == "medium")
#all sig improvements except R0 to R1 and R2 to R3

out.biol1.4 = NULL
out.biol1.4[[1]] = subset(tuk2, R0binA == "long" & R0binB == "long")
out.biol1.4[[2]] = subset(tuk3, R0binA == "long" & R0binB == "long")
out.biol1.4[[3]] = subset(stats.R, R0bin == "long")
#all sig improvements except R2 to R3

##outputs - for each report, are the open bins different?
out.biol1.5 = NULL
out.biol1.5[[1]] = subset(tuk2, reportA == "Report0.Grade" & reportB == "Report0.Grade")
out.biol1.5[[2]] = subset(tuk3, reportA == "Report0.Grade" & reportB == "Report0.Grade")
out.biol1.5[[3]] = subset(stats.R, report == "Report 0")
#all start off the same at R0 

out.biol1.6 = NULL
out.biol1.6[[1]] = subset(tuk2, reportA == "Report1.Grade" & reportB == "Report1.Grade")
out.biol1.6[[2]] = subset(tuk3, reportA == "Report1.Grade" & reportB == "Report1.Grade")
out.biol1.6[[3]] = subset(stats.R, report == "Report 1")
##long starts to surge ahead of all other groups by R1

out.biol1.7 = NULL
out.biol1.7[[1]] = subset(tuk2, reportA == "Report2.Grade" & reportB == "Report2.Grade")
out.biol1.7[[2]] = subset(tuk3, reportA == "Report2.Grade" & reportB == "Report2.Grade")
out.biol1.7[[3]] = subset(stats.R, report == "Report 2")
#not diff at R2 - too much variation or intervening factors? need to look at impact of R1 open time on R2 AcP

out.biol1.8 = NULL
out.biol1.8[[1]] = subset(tuk2, reportA == "Report3.Grade" & reportB == "Report3.Grade")
out.biol1.8[[2]] = subset(tuk3, reportA == "Report3.Grade" & reportB == "Report3.Grade")
out.biol1.8[[3]] = subset(stats.R, report == "Report 3")
#still ot diff at R3 - def check how change to R1 bin category influences R2, and R2 open bin influences R3

out.biol1 = NULL
out.biol1[[1]] = out.biol1.1
out.biol1[[2]] = out.biol1.2
out.biol1[[3]] = out.biol1.3
out.biol1[[4]] = out.biol1.4
out.biol1[[5]] = out.biol1.5
out.biol1[[6]] = out.biol1.6
out.biol1[[7]] = out.biol1.7
out.biol1[[8]] = out.biol1.8 

  
#BIOL1040 Sem 2
df = subset(AcPFbUbins, report == "Report 1" & course == "Level 1" & sem == "Semester 2")
df = df[,c(2,5,10:11)]

#default.plot()
#with(df, boxplot(Final.Grade ~ open.bin))

df[,4] = factor(df[,4], open.bins)

#default.plot()
#plot.mean.sem(df, 2, 4, 100, kz.col, open.bins, "Report mark (%)")
#legend.top(open.bins,  kz.col)

df2 = df[,c(1,4,2)]
df2 = rename(df2, 3, "Report1.Grade")

df2 = merge(df2, subset(AcPFbUbins, report == "Report 2" & course == "Level 1" & sem == "Semester 2")[,c(2,5)], by="StudentID")
df2 = rename(df2, 4, "Report2.Grade")

df2 = merge(df2, subset(AcPFbUbins, report == "Report 3" & course == "Level 1" & sem == "Semester 2")[,c(2,5)], by="StudentID")
df2 = rename(df2, 5, "Report3.Grade")

df3 = melt(df2)
head(df3)
with(df3, tapply(value, open.bin, mean))

stats = stats.mean.sem(df3, 4, 2:3)
stats = round(stats,1)
stats

stats.R1 = stats.mean.sem.n(df2, 3, 2)
stats.R2 = stats.mean.sem.n(df2, 4, 2)
stats.R3 = stats.mean.sem.n(df2, 5, 2)
#stats.R3 = stats.mean.sem.n(df2, 6, 2)

#stats.R0[,4] = "Report 0"
stats.R1[,4] = "Report 1"
stats.R2[,4] = "Report 2"
stats.R3[,4] = "Report 3"

#stats.R0[,5] = row.names(stats.R0)
stats.R1[,5] = row.names(stats.R1)
stats.R2[,5] = row.names(stats.R2)
stats.R3[,5] = row.names(stats.R3)

stats.R = rbind(stats.R1, stats.R2, stats.R3)
stats.R = rename(stats.R, 4:5, c("report", "R1bin"))

stats.R = stats.R[,c(4,5,1:3)]
row.names(stats.R) = 1:nrow(stats.R)

##2 way ANOVA

t = aov(df3[,4] ~ df3[,2]*df3[,3])
summary(t)
p = ap(t)
tuk = TukeyHSD(t)
tuk2 = as.data.frame(tuk[[3]])
tuk2[,5] = row.names(tuk2)


pos1 = gregexpr(":", tuk2[,5])
pos2 = NULL
pos3 = NULL
for (l in 1:length(pos1)) {
  pos2[l] = pos1[[l]][1]
  pos3[l] = pos1[[l]][2]
}

pos1.2 = gregexpr("-", tuk2[,5])
pos2.2 = NULL
pos3.2 = NULL
for (l in 1:length(pos1)) {
  pos2.2[l] = pos1.2[[l]][1]
  pos3.2[l] = nchar(tuk2[l,5])
}
tuk2[,6] = substr(tuk2[,5], 1, (pos2-1))
tuk2[,7] = substr(tuk2[,5], (pos2+1), (pos2.2-1))
tuk2[,8] = substr(tuk2[,5], (pos2.2+1), (pos3-1))
tuk2[,9] = substr(tuk2[,5], (pos3+1), pos3.2)

tuk2[,10] = tuk2[,4] < 0.05
row.names(tuk2) = 1:nrow(tuk2)

#tuk2[which(tuk2[,10] == TRUE),c(4,6:10)]
tuk2 = tuk2[,c(8,9,6,7,4,10)]
tuk2 = rename(tuk2, 1:6, c("R1binA", "reportA", "R1binB", "reportB", "p-adj", "sig"))
table(tuk2[,6])
tuk3 = subset(tuk2, sig == TRUE)

##outputs

out.biol2.1 = NULL
out.biol2.1[[1]] = subset(tuk2, R1binA == "unopened" & R1binB == "unopened")
out.biol2.1[[2]] = subset(tuk3, R1binA == "unopened" & R1binB == "unopened")
out.biol2.1[[3]] = subset(stats.R, R1bin == "unopened")
#so if they did not open R1, regardless of what they did later, they never improved

out.biol2.2 = NULL
out.biol2.2[[1]] = subset(tuk2, R1binA == "short" & R1binB == "short")
out.biol2.2[[2]] = subset(tuk3, R1binA == "short" & R1binB == "short")
out.biol2.2[[3]] = subset(stats.R, R1bin == "short")
#if they looked at R1 for less than 1 minute then they never improved (there are 23 students in this category so not trivial or too small for stats - altho R3 may be a jumpt up from R0 and R1 if the p-adjusted should be trusted at 0.05 instead of 0.05/120)

out.biol2.3 = NULL
out.biol2.3[[1]] = subset(tuk2, R1binA == "medium" & R1binB == "medium")
out.biol2.3[[2]] = subset(tuk3, R1binA == "medium" & R1binB == "medium")
out.biol2.3[[3]] = subset(stats.R, R1bin == "medium")
#all sig improvements for each successive report!

out.biol2.4 = NULL
out.biol2.4[[1]] = subset(tuk2, R1binA == "long" & R1binB == "long")
out.biol2.4[[2]] = subset(tuk3, R1binA == "long" & R1binB == "long")
out.biol2.4[[3]] = subset(stats.R, R1bin == "long")
#all sig improvements except R2 to R3 (potential ceiling over 85%)

##outputs - for each report, are the open bins different?
out.biol2.5 = NULL
out.biol2.5[[1]] = subset(tuk2, reportA == "Report1.Grade" & reportB == "Report1.Grade")
out.biol2.5[[2]] = subset(tuk3, reportA == "Report1.Grade" & reportB == "Report1.Grade")
out.biol2.5[[3]] = subset(stats.R, report == "Report 1")
#unopened trailing behind medium and long at Report 1 (beginning)

out.biol2.6 = NULL
out.biol2.6[[1]] = subset(tuk2, reportA == "Report2.Grade" & reportB == "Report2.Grade")
out.biol2.6[[2]] = subset(tuk3, reportA == "Report2.Grade" & reportB == "Report2.Grade")
out.biol2.6[[3]] = subset(stats.R, report == "Report 2")
#unopened still trailing behind medium and long
#short fallen behind medium and long
#medium fallen behind long too 

out.biol2.7 = NULL
out.biol2.7[[1]] = subset(tuk2, reportA == "Report3.Grade" & reportB == "Report3.Grade")
out.biol2.7[[2]] = subset(tuk3, reportA == "Report3.Grade" & reportB == "Report3.Grade")
out.biol2.7[[3]] = subset(stats.R, report == "Report 3")
#unopened still trailing behind medium and long
#short still trailing behind medium and long
#medium caught up to long  

out.biol2 = NULL
out.biol2[[1]] = out.biol2.1
out.biol2[[2]] = out.biol2.2
out.biol2[[3]] = out.biol2.3
out.biol2[[4]] = out.biol2.4
out.biol2[[5]] = out.biol2.5
out.biol2[[6]] = out.biol2.6
out.biol2[[7]] = out.biol2.7


#BIOM2011 Sem 1
df = subset(AcPFbUbins, report == "Report 1" & course == "Level 2" & sem == "Semester 1")
df = df[,c(2,5,10:11)]

#default.plot()
#with(df, boxplot(Final.Grade ~ open.bin))

df[,4] = factor(df[,4], open.bins)

#default.plot()
#plot.mean.sem(df, 2, 4, 100, kz.col, open.bins, "Report mark (%)")
#legend.top(open.bins,  kz.col)

df2 = df[,c(1,4,2)]
df2 = rename(df2, 3, "Report1.Grade")

df2 = merge(df2, subset(AcPFbUbins, report == "Report 2" & course == "Level 2" & sem == "Semester 1")[,c(2,5)], by="StudentID")
df2 = rename(df2, 4, "Report2.Grade")

df3 = melt(df2)
head(df3)
with(df3, tapply(value, open.bin, mean))

stats = stats.mean.sem(df3, 4, 2:3)
stats = round(stats,1)
stats

stats.R1 = stats.mean.sem.n(df2, 3, 2)
stats.R2 = stats.mean.sem.n(df2, 4, 2)
#stats.R3 = stats.mean.sem.n(df2, 5, 2)
#stats.R3 = stats.mean.sem.n(df2, 6, 2)

#stats.R0[,4] = "Report 0"
stats.R1[,4] = "Report 1"
stats.R2[,4] = "Report 2"
#stats.R3[,4] = "Report 3"

#stats.R0[,5] = row.names(stats.R0)
stats.R1[,5] = row.names(stats.R1)
stats.R2[,5] = row.names(stats.R2)
#stats.R3[,5] = row.names(stats.R3)

stats.R = rbind(stats.R1, stats.R2)
stats.R = rename(stats.R, 4:5, c("report", "R1bin"))

stats.R = stats.R[,c(4,5,1:3)]
row.names(stats.R) = 1:nrow(stats.R)

##2 way ANOVA

t = aov(df3[,4] ~ df3[,2]*df3[,3])
summary(t)
p = ap(t)
tuk = TukeyHSD(t)
tuk2 = as.data.frame(tuk[[3]])
tuk2[,5] = row.names(tuk2)


pos1 = gregexpr(":", tuk2[,5])
pos2 = NULL
pos3 = NULL
for (l in 1:length(pos1)) {
  pos2[l] = pos1[[l]][1]
  pos3[l] = pos1[[l]][2]
}

pos1.2 = gregexpr("-", tuk2[,5])
pos2.2 = NULL
pos3.2 = NULL
for (l in 1:length(pos1)) {
  pos2.2[l] = pos1.2[[l]][1]
  pos3.2[l] = nchar(tuk2[l,5])
}
tuk2[,6] = substr(tuk2[,5], 1, (pos2-1))
tuk2[,7] = substr(tuk2[,5], (pos2+1), (pos2.2-1))
tuk2[,8] = substr(tuk2[,5], (pos2.2+1), (pos3-1))
tuk2[,9] = substr(tuk2[,5], (pos3+1), pos3.2)

tuk2[,10] = tuk2[,4] < (0.05/nrow(tuk[[3]]))
row.names(tuk2) = 1:nrow(tuk2)

#tuk2[which(tuk2[,10] == TRUE),c(4,6:10)]
tuk2 = tuk2[,c(8,9,6,7,4,10)]
tuk2 = rename(tuk2, 1:6, c("R1binA", "reportA", "R1binB", "reportB", "p-adj", "sig"))
table(tuk2[,6])
tuk3 = subset(tuk2, sig == TRUE)

##outputs

subset(tuk2, R1binA == "unopened" & R1binB == "unopened")
subset(tuk3, R1binA == "unopened" & R1binB == "unopened")
subset(stats.R, R1bin == "unopened")
#so if they did not open R1, they still improved

subset(tuk2, R1binA == "short" & R1binB == "short")
subset(tuk3, R1binA == "short" & R1binB == "short")
subset(stats.R, R1bin == "short")
#if they looked at R1 for less than 1 minute then they do NOT improve 
#(there are 2 students in this category so too small for stats)

subset(tuk2, R1binA == "medium" & R1binB == "medium")
subset(tuk3, R1binA == "medium" & R1binB == "medium")
subset(stats.R, R1bin == "medium")
#not sig improvement - dep on p-adj

subset(tuk2, R1binA == "long" & R1binB == "long")
subset(tuk3, R1binA == "long" & R1binB == "long")
subset(stats.R, R1bin == "long")
#not sig improvement - dep on p-adj


#BIOM2011 Sem 2
df = subset(AcPFbUbins, report == "Report 1" & course == "Level 2" & sem == "Semester 2")
df = df[,c(2,5,10:11)]

#default.plot()
#with(df, boxplot(Final.Grade ~ open.bin))

df[,4] = factor(df[,4], open.bins)

#default.plot()
#plot.mean.sem(df, 2, 4, 100, kz.col, open.bins, "Report mark (%)")
#legend.top(open.bins,  kz.col)

df2 = df[,c(1,4,2)]
df2 = rename(df2, 3, "Report1.Grade")

df2 = merge(df2, subset(AcPFbUbins, report == "Report 2" & course == "Level 2" & sem == "Semester 2")[,c(2,5)], by="StudentID")
df2 = rename(df2, 4, "Report2.Grade")

df3 = melt(df2)
head(df3)
with(df3, tapply(value, open.bin, mean))

stats = stats.mean.sem(df3, 4, 2:3)
stats = round(stats,1)
stats

stats.R1 = stats.mean.sem.n(df2, 3, 2)
stats.R2 = stats.mean.sem.n(df2, 4, 2)
#stats.R3 = stats.mean.sem.n(df2, 5, 2)
#stats.R3 = stats.mean.sem.n(df2, 6, 2)

#stats.R0[,4] = "Report 0"
stats.R1[,4] = "Report 1"
stats.R2[,4] = "Report 2"
#stats.R3[,4] = "Report 3"

#stats.R0[,5] = row.names(stats.R0)
stats.R1[,5] = row.names(stats.R1)
stats.R2[,5] = row.names(stats.R2)
#stats.R3[,5] = row.names(stats.R3)

stats.R = rbind(stats.R1, stats.R2)
stats.R = rename(stats.R, 4:5, c("report", "R1bin"))

stats.R = stats.R[,c(4,5,1:3)]
row.names(stats.R) = 1:nrow(stats.R)

##2 way ANOVA

t = aov(df3[,4] ~ df3[,2]*df3[,3])
summary(t)
p = ap(t)
tuk = TukeyHSD(t)
tuk2 = as.data.frame(tuk[[3]])
tuk2[,5] = row.names(tuk2)


pos1 = gregexpr(":", tuk2[,5])
pos2 = NULL
pos3 = NULL
for (l in 1:length(pos1)) {
  pos2[l] = pos1[[l]][1]
  pos3[l] = pos1[[l]][2]
}

pos1.2 = gregexpr("-", tuk2[,5])
pos2.2 = NULL
pos3.2 = NULL
for (l in 1:length(pos1)) {
  pos2.2[l] = pos1.2[[l]][1]
  pos3.2[l] = nchar(tuk2[l,5])
}
tuk2[,6] = substr(tuk2[,5], 1, (pos2-1))
tuk2[,7] = substr(tuk2[,5], (pos2+1), (pos2.2-1))
tuk2[,8] = substr(tuk2[,5], (pos2.2+1), (pos3-1))
tuk2[,9] = substr(tuk2[,5], (pos3+1), pos3.2)

tuk2[,10] = tuk2[,4] < (0.05/nrow(tuk[[3]]))
row.names(tuk2) = 1:nrow(tuk2)

#tuk2[which(tuk2[,10] == TRUE),c(4,6:10)]
tuk2 = tuk2[,c(8,9,6,7,4,10)]
tuk2 = rename(tuk2, 1:6, c("R1binA", "reportA", "R1binB", "reportB", "p-adj", "sig"))
table(tuk2[,6])
tuk3 = subset(tuk2, sig == TRUE)

##outputs

subset(tuk2, R1binA == "unopened" & R1binB == "unopened")
subset(tuk3, R1binA == "unopened" & R1binB == "unopened")
subset(stats.R, R1bin == "unopened")
#so if they did not open R1, they did not improve

subset(tuk2, R1binA == "short" & R1binB == "short")
subset(tuk3, R1binA == "short" & R1binB == "short")
subset(stats.R, R1bin == "short")
#if they looked at R1 for less than 1 minute then they do NOT improve 
#(there are 2 students in this category so too small for stats)

subset(tuk2, R1binA == "medium" & R1binB == "medium")
subset(tuk3, R1binA == "medium" & R1binB == "medium")
subset(stats.R, R1bin == "medium")
#not sig improvement - dep on p-adj

subset(tuk2, R1binA == "long" & R1binB == "long")
subset(tuk3, R1binA == "long" & R1binB == "long")
subset(stats.R, R1bin == "long")
#not sig improvement - dep on p-adj

#BIOM2011 Sem combined
df = subset(AcPFbUbins, report == "Report 1" & course == "Level 2")
df = df[,c(2,5,10:11)]

#default.plot()
#with(df, boxplot(Final.Grade ~ open.bin))

df[,4] = factor(df[,4], open.bins)

#default.plot()
#plot.mean.sem(df, 2, 4, 100, kz.col, open.bins, "Report mark (%)")
#legend.top(open.bins,  kz.col)

df2 = df[,c(1,4,2)]
df2 = rename(df2, 3, "Report1.Grade")

df2 = merge(df2, subset(AcPFbUbins, report == "Report 2" & course == "Level 2")[,c(2,5)], by="StudentID")
df2 = rename(df2, 4, "Report2.Grade")

df3 = melt(df2)
head(df3)
with(df3, tapply(value, open.bin, mean))

stats = stats.mean.sem(df3, 4, 2:3)
stats = round(stats,1)
stats

stats.R1 = stats.mean.sem.n(df2, 3, 2)
stats.R2 = stats.mean.sem.n(df2, 4, 2)
#stats.R3 = stats.mean.sem.n(df2, 5, 2)
#stats.R3 = stats.mean.sem.n(df2, 6, 2)

#stats.R0[,4] = "Report 0"
stats.R1[,4] = "Report 1"
stats.R2[,4] = "Report 2"
#stats.R3[,4] = "Report 3"

#stats.R0[,5] = row.names(stats.R0)
stats.R1[,5] = row.names(stats.R1)
stats.R2[,5] = row.names(stats.R2)
#stats.R3[,5] = row.names(stats.R3)

stats.R = rbind(stats.R1, stats.R2)
stats.R = rename(stats.R, 4:5, c("report", "R1bin"))

stats.R = stats.R[,c(4,5,1:3)]
row.names(stats.R) = 1:nrow(stats.R)

##2 way ANOVA

t = aov(df3[,4] ~ df3[,2]*df3[,3])
summary(t)
p = ap(t)
tuk = TukeyHSD(t)
tuk2 = as.data.frame(tuk[[3]])
tuk2[,5] = row.names(tuk2)


pos1 = gregexpr(":", tuk2[,5])
pos2 = NULL
pos3 = NULL
for (l in 1:length(pos1)) {
  pos2[l] = pos1[[l]][1]
  pos3[l] = pos1[[l]][2]
}

pos1.2 = gregexpr("-", tuk2[,5])
pos2.2 = NULL
pos3.2 = NULL
for (l in 1:length(pos1)) {
  pos2.2[l] = pos1.2[[l]][1]
  pos3.2[l] = nchar(tuk2[l,5])
}
tuk2[,6] = substr(tuk2[,5], 1, (pos2-1))
tuk2[,7] = substr(tuk2[,5], (pos2+1), (pos2.2-1))
tuk2[,8] = substr(tuk2[,5], (pos2.2+1), (pos3-1))
tuk2[,9] = substr(tuk2[,5], (pos3+1), pos3.2)

tuk2[,10] = tuk2[,4] < 0.05
row.names(tuk2) = 1:nrow(tuk2)

#tuk2[which(tuk2[,10] == TRUE),c(4,6:10)]
tuk2 = tuk2[,c(8,9,6,7,4,10)]
tuk2 = rename(tuk2, 1:6, c("R1binA", "reportA", "R1binB", "reportB", "p-adj", "sig"))
table(tuk2[,6])
tuk3 = subset(tuk2, sig == TRUE)

##outputs

out.biom1 = NULL
out.biom1[[1]] = subset(tuk2, R1binA == "unopened" & R1binB == "unopened")
out.biom1[[2]] = subset(tuk3, R1binA == "unopened" & R1binB == "unopened")
out.biom1[[3]] = subset(stats.R, R1bin == "unopened")
#so if they did not open R1, they still improve

out.biom2 = NULL
out.biom2[[1]] = subset(tuk2, R1binA == "short" & R1binB == "short")
out.biom2[[2]] = subset(tuk3, R1binA == "short" & R1binB == "short")
out.biom2[[3]] = subset(stats.R, R1bin == "short")
#if they looked at R1 for less than 1 minute then they do NOT improve 
#(there are 5 students in this category so too small for stats)

out.biom3 = NULL
out.biom3[[1]] = subset(tuk2, R1binA == "medium" & R1binB == "medium")
out.biom3[[2]] = subset(tuk3, R1binA == "medium" & R1binB == "medium")
out.biom3[[3]] = subset(stats.R, R1bin == "medium")
#sig improvement 

out.biom4 = NULL
out.biom4[[1]] = subset(tuk2, R1binA == "long" & R1binB == "long")
out.biom4[[2]] = subset(tuk3, R1binA == "long" & R1binB == "long")
out.biom4[[3]] = subset(stats.R, R1bin == "long")
#sig improvement 

##outputs - for each report, are the open bins different?
out.biom5 = NULL
out.biom5[[1]] = subset(tuk2, reportA == "Report1.Grade" & reportB == "Report1.Grade")
out.biom5[[2]] = subset(tuk3, reportA == "Report1.Grade" & reportB == "Report1.Grade")
out.biom5[[3]] = subset(stats.R, report == "Report 1")
#unopened trailing behind long at Report 1 (beginning)

out.biom6 = NULL
out.biom6[[1]] = subset(tuk2, reportA == "Report2.Grade" & reportB == "Report2.Grade")
out.biom6[[2]] = subset(tuk3, reportA == "Report2.Grade" & reportB == "Report2.Grade")
out.biom6[[3]] = subset(stats.R, report == "Report 2")
#no sig diff by Report 2??? - maybe if remove the 5 short with the large variation
#pos unopened behind long (p-adj 0.02) and trend for short to be behind long (p-adj 0.07)

out.biom = NULL
out.biom[[1]] = out.biom1
out.biom[[2]] = out.biom2
out.biom[[3]] = out.biom3
out.biom[[4]] = out.biom4
out.biom[[5]] = out.biom5
out.biom[[6]] = out.biom6

out.acp.open = NULL
out.acp.open[[1]] = out.biol1
out.acp.open[[2]] = out.biol2
out.acp.open[[3]] = out.biom

return(out.acp.open) 
}