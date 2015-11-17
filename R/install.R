
.libPaths(new = '/home/hklingen/projects/TMP/R')

Method.primary = c('DESeq2','edgeR','samr')


Method.Replacements = list(DESeq2 = 'DESeq', edgeR = 'limma', samr = '')

Method.Replacements.Type = list(DESeq2 = 'bio', edgeR = 'bio', samr = '')




biocondURL = "https://bioconductor.org/biocLite.R"
biocondURLnoHTTPS = "http://bioconductor.org/biocLite.R"


source(biocondURLnoHTTPS)


install.additional.packages <- function(PackageVec,TypeVec)
{
.nPackages2test = length(PackageVec)
  for (i in 1:.nPackages2test)
  {
  .res = check.testforpackage(test.package = PackageVec[i], LOAD.FLAG = FALSE)
    if (!.res)
    {
    cat('downloading ',PackageVec[i],'\n',sep = '')
      if (TypeVec[i] == 'bio')
      {
        biocLite(PackageVec[i], lib = '/home/hklingen/projects/TMP/R')
      }
      
      else
      {
        install.packages(PackageVec[i], lib = '/home/hklingen/projects/TMP/R')
      }
      #try to load library, if error...
      
    }
  }
}