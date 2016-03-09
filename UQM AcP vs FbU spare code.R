
```{r, echo=FALSE}

#df = dcast(AcPFbUbins, StudentID + project ~ open.bin, value.var = "Final.Grade")
#df = dcast(AcPFbUbins, StudentID + course + sem + open.bin ~ report, value.var = "Final.Grade")
#df = dcast(AcPFbUbins, StudentID + open.bin ~ project, value.var = "Final.Grade")
#df = dcast(AcPFbUbins, StudentID + course + sem + open.bin ~ report, value.var = "Final.Grade")
#head(df)
#df[which(df$StudentID == "S8013163"),]

df = dcast(AcPFbUbins, StudentID + project + open.bin ~ report, value.var = "Final.Grade")
head(df)

df2 = melt(df)
head(df2)
df2 = remover(df2, 5)
head(df2)

df2 = rename(df2, 4:5, c("report", "Final.Grade"))
df3 = dcast(df2, StudentID ~ report, value.var = "Final.Grade", sum, na.rm=T)
head(df3)

df4 = df3[(which(df3[,3] <= 100)),]
head(df4)
max(df4[,4])
df4 = df4[(which(df4[,4] <= 100)),]
head(df4)
write.csv(df4, "StudentFinalGradeSummary.csv", row.names = F)

R0bins = AcPFbUbins %>% filter(project == "BIOL1040Sem1Report 0")
R0bins = R0bins %>% select(StudentID, open.bin)
R0.open = merge(df4, R0bins)
head(R0.open)
head(df4)
R0.open = rename(R0.open, 6, "Report0.open.bin")

statsR11 = stats.mean.sem(R0.open, 2, 6)
statsR11 = round(statsR11,1)
statsR11

df11 = AcP %>% filter(course == "Level 1", sem == "Semester 1")
tR11 = aov(R0.open[,2] ~ R0.open[,6])
summary(tR11)
pR11 = ap(tR11)
tukR11 = TukeyHSD(tR11)
tukR11

R0.open.m = melt(R0.open)
head(R0.open.m)


dfx = subset(R0.open.m, Report0.open.bin == "unopened")
stats.x = stats.mean.sem(dfx, 4, 3)
stats.x = round(stats.x,1)
stats.x

t.x = aov(dfx[,4] ~ dfx[,3])
summary(t.x)
p.x = ap(t.x)
tuk.x = TukeyHSD(t.x)
tuk.x





```
