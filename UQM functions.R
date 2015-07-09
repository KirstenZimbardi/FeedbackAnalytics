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
