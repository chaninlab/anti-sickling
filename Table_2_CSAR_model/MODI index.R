## AtomPairs2DFingerprintCount.csv = 0.7895331
## AtomPairs2DFingerprinter.csv = 0.7594127
## EStateFingerprinter.csv = 0.7390813
## ExtendedFingerprinter.csv = 0.7558358
## Fingerprinter.csv = 0.7714608
## GraphOnlyFingerprinter.csv = 0.7185617
## KlekotaRothFingerprintCount.csv = 0.7004895
## KlekotaRothFingerprinter.csv = 0.8399849
## MACCSFingerprinter.csv = 0.8111822
## PubchemFingerprinter.csv = 0.7654367
## SubstructureFingerprintCount.csv = 0.7402108
## SubstructureFingerprinter.csv = 0.8123117
## ranking = 0.840 - 0.700
df1 <- read.csv("SubstructureFingerprinter.csv", header = TRUE) 
Dat = df1[,-1]
n = ncol(Dat)
m= n-1
AA <- Dat[,1:m]
Activity = as.numeric(Dat[,ncol(Dat)])

d1 <- dist(AA, upper=TRUE, diag=TRUE, method = "euclidean")
nd1 <- scale(d1)
nd2 = ((nd1-min(nd1))/(max(nd1)-min(nd1)))
MOBI <- matrix(nrow = nrow(Dat), ncol = 1)

for (i in 1:nrow(Dat)){
  MOBI[i,] <- Dat[order(nd2[i,]),][2,n]
}

result = data.frame(Activity,MOBI)
X <- subset(result, result[,1] == '1')
Y <- subset(result, result[,1] == '2')

(table(X)[1]/nrow(X)+ table(Y)[2]/nrow(Y))/2
