#UQM specific stuff

projects = function(...)
{
  project.num = c(rev(ProjectID[1:7,2]), ProjectID[8:11,2])
  project.num <<- project.num
  biol.names = rev(paste0(ProjectID[1:7,3], "Sem", ProjectID[1:7,5], ProjectID[1:7,6]))
  biom.names = paste0(ProjectID[8:11,3], "Sem", ProjectID[8:11,5], ProjectID[8:11,6])
  project.names = c(biol.names, biom.names)
  project.names <<- project.names
  project.names.formative = project.names
  project.names.formative = c("BIOL1040Sem1Report 0",  project.names.formative[c(1:3,5:11)])
  project.names.formative <<- project.names.formative
}

graphs = function(...)
{
  report.names = NULL
  for (i in 1:length(project.names.formative))
    report.names[i] = substr(project.names.formative[i], 13,20)
  report.names <<- report.names
  
  kz.col = c("#CAFF70", "#66CD00", "#1874CD", "#000080")
  kz.col <<-  kz.col
  report.col = c(rep.int("#CAFF70", 4), rep.int("#66CD00", 3), rep.int("#1874CD", 2), rep.int("#000080", 2))
  report.col <<- report.col
  
  sem.names = c("Level 1 Sem 1", "Level 1 Sem 2", "Level 2 Sem 1", "Level 2 Sem 2")
  sem.names <<- sem.names
}

df.mark.convert = function(df, col1, col2)
{
  for (j in col1:col2)
    for (i in 1:nrow(df))
    {
      if (df[i,j] == 100)
        df[i,j] = 4
      if (df[i,j] == 80)
        df[i,j] = 3
      if (df[i,j] == 60)
        df[i,j] = 2
      if (df[i,j] == 40)
        df[i,j] = 1
      if (df[i,j] == 20)
        df[i,j] = 0
    }
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

#Generalising course names for publication
gen = function(df, col.course, col.sem){
  df[,col.course] = gsub("BIOL1040", "Level 1", df[,col.course])
  df[,col.course] = gsub("BIOM2011", "Level 2", df[,col.course])
  if (is.numeric(df[1,col.sem])) { 
    df[,col.sem] = gsub("1", "Semester 1", df[,col.sem])
    df[,col.sem] = gsub("2", "Semester 2", df[,col.sem])
  }
  if (is.character(df[1,col.sem])) { 
  df[,col.sem] = gsub("Sem1", "Semester 1", df[,col.sem])
  df[,col.sem] = gsub("Sem2", "Semester 2", df[,col.sem])
  }
  return(df)
}

uqm.csv = function(file){
  df = read.csv(paste0(folder, file, ".csv"), header=TRUE, stringsAsFactors=FALSE)
  return(df)
}
