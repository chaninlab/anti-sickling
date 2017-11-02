library(rcdk)
library(rcdklibs)
library(rJava)
library(fingerprint)
source("https://bioconductor.org/biocLite.R")
biocLite("Rcpi")
require(Rcpi)
require(ChemmineR)
mols <- load.molecules('mol.sdf')
desc.names <- c(
  'org.openscience.cdk.qsar.descriptors.molecular.WeightDescriptor',
  'org.openscience.cdk.qsar.descriptors.molecular.HBondDonorCountDescriptor',
  'org.openscience.cdk.qsar.descriptors.molecular.HBondAcceptorCountDescriptor',
  'org.openscience.cdk.qsar.descriptors.molecular.ALOGPDescriptor'
)
desc.list <- list()
for (i in 1:length(mols)) {
  tmp <- c()
  for (j in 1:length(desc.names)) {
    values <- eval.desc(mols[i], desc.names[j])
    tmp <- c(tmp, values)
  }
  desc.list[[i]] <- tmp
}
desc.data <- as.data.frame(do.call('rbind', desc.list))
result = desc.data[,-c(5,6)]
lipinski_result <- data.frame(lapply(result, as.character), stringsAsFactors=FALSE)
write.csv(lipinski_result, "lipinski.csv", row.names=TRUE, na="")
data = read.csv("lipinski.csv", header = TRUE)
lipinski_result = cbind(data,activity)
write.csv(lipinski_result, "lipinski_results.csv", row.names=TRUE, na="")