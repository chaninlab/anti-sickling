## system ("java -jar PaDEL-Descriptor.jar -fingerprints -descriptortypes descriptors.xml -dir ./ -file output.csv")
data <- read.csv("Descriptors.csv", header = TRUE)
activity <- read.csv("activity.csv", header = TRUE)
name <- data[,1]
df <- data[,-1]
## Fingerprinter Descriptors
Fingerprinter <- df[,1:1024]
Fingerprinter_name <- cbind(name,Fingerprinter,activity)
write.csv(Fingerprinter_name,"Fingerprinter.csv", row.names = FALSE, na="")
## ExtendedFingerprinter Descriptors
df1 <- df[,-1:-1024]
ExtendedFingerprinter <- df1[,1:1024]
ExtendedFingerprinter_name <- cbind(name,ExtendedFingerprinter,activity)
write.csv(ExtendedFingerprinter_name,"ExtendedFingerprinter.csv", row.names = FALSE, na="")
## EStateFingerprinter Descriptors
df2 <- df1[,-1:-1024]
EStateFingerprinter <- df2[,1:79]
EStateFingerprinter_name <- cbind(name,EStateFingerprinter,activity)
write.csv(EStateFingerprinter_name,"EStateFingerprinter.csv", row.names = FALSE, na="")
## GraphOnlyFingerprinter Descriptors
df3 <- df2[,-1:-79]
GraphOnlyFingerprinter <- df3[,1:1024]
GraphOnlyFingerprinter_name <- cbind(name,GraphOnlyFingerprinter,activity)
write.csv(GraphOnlyFingerprinter_name,"GraphOnlyFingerprinter.csv", row.names = FALSE, na="")
## MACCSFingerprinter Descriptors
df4 <- df3[,-1:-1024]
MACCSFingerprinter <- df4[,1:166]
MACCSFingerprinter_name <- cbind(name,MACCSFingerprinter,activity)
write.csv(MACCSFingerprinter_name,"MACCSFingerprinter.csv", row.names = FALSE, na="")
## PubchemFingerprinter Descriptors
df5 <- df4[,-1:-166]
PubchemFingerprinter <- df5[,1:881]
PubchemFingerprinter_name <- cbind(name,PubchemFingerprinter,activity)
write.csv(PubchemFingerprinter_name,"PubchemFingerprinter.csv", row.names = FALSE, na="")
## SubstructureFingerprinter Descriptors
df6 <- df5[,-1:-881]
SubstructureFingerprinter <- df6[,1:307]
SubSF <- SubstructureFingerprinter[,-c(289,290,300,301,307)]
SubstructureFingerprinter_namecut <- cbind(name,SubSF,activity)
SubstructureFingerprinter_name <- cbind(name,SubstructureFingerprinter,activity)
write.csv(SubstructureFingerprinter_name,"SubstructureFingerprinter.csv", row.names = FALSE, na="")
write.csv(SubstructureFingerprinter_namecut,"SubstructureFingerprinter(cut).csv", row.names = FALSE, na="")
## KlekotaRothFingerprinter Descriptors
df7 <- df6[,-1:-307]
KlekotaRothFingerprinter <- df7[,1:4860]
KlekotaRothFingerprinter_name <- cbind(name,KlekotaRothFingerprinter,activity)
write.csv(KlekotaRothFingerprinter_name,"KlekotaRothFingerprinter.csv", row.names = FALSE, na="")
## AtomPairs2DFingerprinter Descriptors
df8 <- df7[,-1:-4860]
AtomPairs2DFingerprinter <- df8[,1:780]
AtomPairs2DFingerprinter_name <- cbind(name,AtomPairs2DFingerprinter,activity)
write.csv(AtomPairs2DFingerprinter_name,"AtomPairs2DFingerprinter.csv", row.names = FALSE, na="")
## SubstructureFingerprintCount Descriptors
df9 <- df8[,-1:-780]
SubstructureFingerprintCount <- df9[,1:307]
SubSFC <- SubstructureFingerprintCount[,-c(289,290,300,301,307)]
SubstructureFingerprintCount_namecut <- cbind(name,SubSFC,activity)
SubstructureFingerprintCount_name <- cbind(name,SubstructureFingerprintCount,activity)
write.csv(SubstructureFingerprintCount_name,"SubstructureFingerprintCount.csv", row.names = FALSE, na="")
write.csv(SubstructureFingerprintCount_namecut,"SubstructureFingerprintCount(cut).csv", row.names = FALSE, na="")
## KlekotaRothFingerprintCount Descriptors
df10 <- df9[,-1:-307]
KlekotaRothFingerprintCount <- df10[,1:4860]
KlekotaRothFingerprintCount_name <- cbind(name,KlekotaRothFingerprintCount,activity)
write.csv(KlekotaRothFingerprintCount_name,"KlekotaRothFingerprintCount.csv", row.names = FALSE, na="")
## AtomPairs2DFingerprintCount Descriptors
df11 <- df10[,-1:-4860]
AtomPairs2DFingerprintCount <- df11[,1:ncol(df11)]
AtomPairs2DFingerprintCount_name <- cbind(name,AtomPairs2DFingerprintCount,activity)
write.csv(AtomPairs2DFingerprintCount_name,"AtomPairs2DFingerprintCount.csv", row.names = FALSE, na="")