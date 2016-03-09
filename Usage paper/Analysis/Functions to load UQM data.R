#functions to load data

folder = "./Data Stage 1/"

Ac.performance = function(projects, project.names, course)
{      
  file.names <- lapply(projects, function(x){paste0(folder, "AcP", x, "DeID.csv")})
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
  file.names <- lapply(projects, function(x){paste0(folder, "FbP", x, "DeID.csv")})
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
  file.names <- lapply(projects, function(x){paste0(folder, "FbU", x, "DeID.csv")})
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
  file.names <- lapply(1:2, function(x){paste0(folder, "BIOL", x, "demogDeID.csv")})
  demog.biol <- lapply(file.names, read.csv, header=TRUE, stringsAsFactors=FALSE)
  
  file.names <- lapply(1:2, function(x){paste0(folder, "BIOM", x, "demogDeID.csv")})
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

