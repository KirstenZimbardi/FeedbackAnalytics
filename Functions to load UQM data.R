#functions to load data

Ac.performance = function(projects, project.names, course)
{      
  file.names <- lapply(projects, function(x){paste0("AcP", x, "DeID.csv")})
  AcP <- lapply(file.names, read.csv, header=TRUE, stringsAsFactors=FALSE)
  names(AcP) <- project.names
  if (course == "biol")
  {
    for (i in 1:7)
      AcP[[i]] = AcP[[i]][,2:21]
  }
  else if (course == "biom")
  {
    for (i in 1:4)
      AcP[[i]] = AcP[[i]][,c(3,2,4:20)]
  }
  return(AcP)
}


Fb.provision = function(projects, project.names)
{
  file.names <- lapply(projects, function(x){paste0("FbP", x, "DeID.csv")})
  FbP <- lapply(file.names, read.csv, header=TRUE, stringsAsFactors=FALSE)
  names(FbP) <- project.names
  for (i in 1:length(FbP))
  {
    FbP[[i]] = FbP[[i]][,2:13]
  }
  return(FbP)
}

Fb.use = function(projects, project.names)
{
  file.names <- lapply(projects, function(x){paste0("FbU", x, "DeID.csv")})
  FbU <- lapply(file.names, read.csv, header=TRUE, stringsAsFactors=FALSE)
  names(FbU) <- project.names
  for (i in 1:7)
  {
    FbU[[i]] = FbU[[i]][,c(2:9,13:24)]
  }
  for (i in 8:11)
  {
    FbU[[i]] = FbU[[i]][,2:21]
  }
  return(FbU)
}

Demog = function(...)
{
  file.names <- lapply(1:2, function(x){paste0("BIOL", x, "demogDeID.csv")})
  demog.biol <- lapply(file.names, read.csv, header=TRUE, stringsAsFactors=FALSE)
  
  file.names <- lapply(1:2, function(x){paste0("BIOM", x, "demogDeID.csv")})
  demog.biom <- lapply(file.names, read.csv, header=TRUE, stringsAsFactors=FALSE)
  
  demog = c(demog.biol, demog.biom)
  names(demog) <- c("BIOLSem1","BIOLSem2", "BIOMSem1","BIOMSem2")
  for (i in 1:4)
  {
    demog[[i]] = demog[[i]][,2:20]
  }
  n = names(demog[[1]])
  n[1] = "StudentID"
  for (i in 1:4)
  {
    names(demog[[i]]) = n
  }
  return(demog)
}


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
  for (i in 1:length(project.names))
    report.names[i] = substr(project.names[i], 13,20)
  report.names <<- report.names
  
  kz.col = c("cyan1", "chartreuse", "yellow", "darkorange")
  kz.col <<-  kz.col
  report.col = c(rep.int("cyan1", 4), rep.int("chartreuse", 3), rep.int("yellow", 2), rep.int("darkorange", 2))
  report.col <<- report.col
  
  sem.names = c("Level 1 Sem 1", "Level 1 Sem 2", "Level 2 Sem 1", "Level 2 Sem 2")
  sem.names <<- sem.names
}

dep.variables = function()
{
  criteria = "Hypoth,Methods.writing,Methods.details,Methods.design,Results.text,Results.figures,Results.legends,Disc.knowlede,Disc.InterpFindings,Disc.Evidence,Refs,Writing"
}