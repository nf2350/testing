## Bias and Correlation of the results 

out111 <- read.csv("~/Desktop/out111.csv")
rownames(out111) <- out111$X
out111 <- out111[,-1]

a1s <- out111[grep("a1_", rownames(out111)),]
a2s <- out111[grep("a2_", rownames(out111)),]
b1s <- out111[grep("b1_", rownames(out111)),]
b2s <- out111[grep("b2_", rownames(out111)),]
b3s <- out111[grep("b3_", rownames(out111)),]
b4s <- out111[grep("b4_", rownames(out111)),]

Cor_a1 <- cor(a[c(1:20),1],rowMeans(a1s[c(1:20),]))
Cor_a2 <- cor(a[c(21:40),2],rowMeans(a2s[c(21:40),]))
Cor_b1 <- cor(d[,1],rowMeans(b1s))
Cor_b2 <- cor(d[,2],rowMeans(b2s))
Cor_b3 <- cor(d[,3],rowMeans(b3s))
Cor_b4 <- cor(d[,4],rowMeans(b4s))

cor111 <- cbind(Cor_a1, Cor_a2, Cor_b1, Cor_b2, Cor_b3, Cor_b4)

cor111

bias <- function(x,y) {
  mean((apply(((x-y)^2), 1, mean)))
}

bias_a1 <- bias(a[c(1:20),1],a1s[c(1:20),])
bias_a2 <- bias(a[c(21:40),2],a2s[c(21:40),])
bias_b1 <- bias(d[,1],b1s)
bias_b2 <- bias(d[,2],b2s)
bias_b3 <- bias(d[,3],b3s)
bias_b4 <- bias(d[,4],b4s) 

bias111 <- cbind(bias_a1, bias_a2, bias_b1, bias_b2, bias_b3, bias_b4)

bias111

###112

out112 <- read.csv("~/Desktop/output112.csv")
rownames(out112) <- out112$X
out112 <- out112[,-1]
a1s <- out112[grep("a1_", rownames(out112)),]
a2s <- out112[grep("a2_", rownames(out112)),]
b1s <- out112[grep("b1_", rownames(out112)),]
b2s <- out112[grep("b2_", rownames(out112)),]
b3s <- out112[grep("b3_", rownames(out112)),]
b4s <- out112[grep("b4_", rownames(out112)),]



Cor_a1 <- cor(a[c(1:20),1],rowMeans(a1s[c(1:20),]))
Cor_a2 <- cor(a[c(21:40),2],rowMeans(a2s[c(21:40),]))
Cor_b1 <- cor(d[,1],rowMeans(b1s))
Cor_b2 <- cor(d[,2],rowMeans(b2s))
Cor_b3 <- cor(d[,3],rowMeans(b3s))
Cor_b4 <- cor(d[,4],rowMeans(b4s))

cor112 <- cbind(Cor_a1, Cor_a2, Cor_b1, Cor_b2, Cor_b3, Cor_b4)

cor112

bias <- function(x,y) {
  mean((apply(((x-y)), 1, mean)))
}

bias_a1 <- bias(a[c(1:20),1],a1s[c(1:20),])
bias_a2 <- bias(a[c(21:40),2],a2s[c(21:40),])
bias_b1 <- bias(d[,1],b1s)
bias_b2 <- bias(d[,2],b2s)
bias_b3 <- bias(d[,3],b3s)
bias_b4 <- bias(d[,4],b4s) 

bias112 <- cbind(bias_a1, bias_a2, bias_b1, bias_b2, bias_b3, bias_b4)

bias112

#221
out221 <- read.csv("~/Desktop/output221.csv")
rownames(out221) <- out221$X
out221 <- out221[,-1]
a1s <- out221[grep("a1_", rownames(out221)),]
a2s <- out221[grep("a2_", rownames(out221)),]
b1s <- out221[grep("b1_", rownames(out221)),]
b2s <- out221[grep("b2_", rownames(out221)),]
b3s <- out221[grep("b3_", rownames(out221)),]
b4s <- out221[grep("b4_", rownames(out221)),]



Cor_a1 <- cor(a[c(1:20),1],rowMeans(a1s[c(1:20),]))
Cor_a2 <- cor(a[c(21:40),2],rowMeans(a2s[c(21:40),]))
Cor_b1 <- cor(d[,1],rowMeans(b1s))
Cor_b2 <- cor(d[,2],rowMeans(b2s))
Cor_b3 <- cor(d[,3],rowMeans(b3s))
Cor_b4 <- cor(d[,4],rowMeans(b4s))

cor221 <- cbind(Cor_a1, Cor_a2, Cor_b1, Cor_b2, Cor_b3, Cor_b4)

cor221

bias <- function(x,y) {
  mean((apply(((x-y)), 1, mean)))
}

bias_a1 <- bias(a[c(1:20),1],a1s[c(1:20),])
bias_a2 <- bias(a[c(21:40),2],a2s[c(21:40),])
bias_b1 <- bias(d[,1],b1s)
bias_b2 <- bias(d[,2],b2s)
bias_b3 <- bias(d[,3],b3s)
bias_b4 <- bias(d[,4],b4s) 

bias221 <- cbind(bias_a1, bias_a2, bias_b1, bias_b2, bias_b3, bias_b4)

bias221

#222
out222 <- read.csv("~/Desktop/output222.csv")
rownames(out222) <- out222$X
out222 <- out222[,-1]
a1s <- out222[grep("a1_", rownames(out222)),]
a2s <- out222[grep("a2_", rownames(out222)),]
b1s <- out222[grep("b1_", rownames(out222)),]
b2s <- out222[grep("b2_", rownames(out222)),]
b3s <- out222[grep("b3_", rownames(out222)),]
b4s <- out222[grep("b4_", rownames(out222)),]



Cor_a1 <- cor(a[c(1:20),1],rowMeans(a1s[c(1:20),]))
Cor_a2 <- cor(a[c(21:40),2],rowMeans(a2s[c(21:40),]))
Cor_b1 <- cor(d[,1],rowMeans(b1s))
Cor_b2 <- cor(d[,2],rowMeans(b2s))
Cor_b3 <- cor(d[,3],rowMeans(b3s))
Cor_b4 <- cor(d[,4],rowMeans(b4s))

cor222 <- cbind(Cor_a1, Cor_a2, Cor_b1, Cor_b2, Cor_b3, Cor_b4)

cor222

bias <- function(x,y) {
  mean((apply(((x-y)), 1, mean)))
}

bias_a1 <- bias(a[c(1:20),1],a1s[c(1:20),])
bias_a2 <- bias(a[c(21:40),2],a2s[c(21:40),])
bias_b1 <- bias(d[,1],b1s)
bias_b2 <- bias(d[,2],b2s)
bias_b3 <- bias(d[,3],b3s)
bias_b4 <- bias(d[,4],b4s) 

bias222 <- cbind(bias_a1, bias_a2, bias_b1, bias_b2, bias_b3, bias_b4)

bias222

##331

out331 <- read.csv("~/Desktop/output331.csv")
rownames(out331) <- out331$X
out331 <- out331[,-1]

a1s <- out331[grep("a1_", rownames(out331)),]
a2s <- out331[grep("a2_", rownames(out331)),]
b1s <- out331[grep("b1_", rownames(out331)),]
b2s <- out331[grep("b2_", rownames(out331)),]
b3s <- out331[grep("b3_", rownames(out331)),]
b4s <- out331[grep("b4_", rownames(out331)),]

Cor_a1 <- cor(a[c(1:20),1],rowMeans(a1s[c(1:20),]))
Cor_a2 <- cor(a[c(21:40),2],rowMeans(a2s[c(21:40),]))
Cor_b1 <- cor(d[,1],rowMeans(b1s))
Cor_b2 <- cor(d[,2],rowMeans(b2s))
Cor_b3 <- cor(d[,3],rowMeans(b3s))
Cor_b4 <- cor(d[,4],rowMeans(b4s))

cor331 <- cbind(Cor_a1, Cor_a2, Cor_b1, Cor_b2, Cor_b3, Cor_b4)

cor331

bias <- function(x,y) {
  mean((apply(((x-y)^2), 1, mean)))
}

bias_a1 <- bias(a[c(1:20),1],a1s[c(1:20),])
bias_a2 <- bias(a[c(21:40),2],a2s[c(21:40),])
bias_b1 <- bias(d[,1],b1s)
bias_b2 <- bias(d[,2],b2s)
bias_b3 <- bias(d[,3],b3s)
bias_b4 <- bias(d[,4],b4s) 

bias331 <- cbind(bias_a1, bias_a2, bias_b1, bias_b2, bias_b3, bias_b4)

bias331

##332
out332 <- read.csv("~/Desktop/output332.csv")
rownames(out332) <- out332$X
out332 <- out332[,-1]

a1s <- out332[grep("a1_", rownames(out332)),]
a2s <- out332[grep("a2_", rownames(out332)),]
b1s <- out332[grep("b1_", rownames(out332)),]
b2s <- out332[grep("b2_", rownames(out332)),]
b3s <- out332[grep("b3_", rownames(out332)),]
b4s <- out332[grep("b4_", rownames(out332)),]

Cor_a1 <- cor(a[c(1:20),1],rowMeans(a1s[c(1:20),]))
Cor_a2 <- cor(a[c(21:40),2],rowMeans(a2s[c(21:40),]))
Cor_b1 <- cor(d[,1],rowMeans(b1s))
Cor_b2 <- cor(d[,2],rowMeans(b2s))
Cor_b3 <- cor(d[,3],rowMeans(b3s))
Cor_b4 <- cor(d[,4],rowMeans(b4s))

cor332 <- cbind(Cor_a1, Cor_a2, Cor_b1, Cor_b2, Cor_b3, Cor_b4)

cor332

bias <- function(x,y) {
  mean((apply(((x-y)^2), 1, mean)))
}

bias_a1 <- bias(a[c(1:20),1],a1s[c(1:20),])
bias_a2 <- bias(a[c(21:40),2],a2s[c(21:40),])
bias_b1 <- bias(d[,1],b1s)
bias_b2 <- bias(d[,2],b2s)
bias_b3 <- bias(d[,3],b3s)
bias_b4 <- bias(d[,4],b4s) 

bias332 <- cbind(bias_a1, bias_a2, bias_b1, bias_b2, bias_b3, bias_b4)

bias332

##441
out441 <- read.csv("~/Desktop/output441.csv")
rownames(out441) <- out441$X
out441 <- out441[,-1]

a1s <- out441[grep("a1_", rownames(out441)),]
a2s <- out441[grep("a2_", rownames(out441)),]
b1s <- out441[grep("b1_", rownames(out441)),]
b2s <- out441[grep("b2_", rownames(out441)),]
b3s <- out441[grep("b3_", rownames(out441)),]
b4s <- out441[grep("b4_", rownames(out441)),]

Cor_a1 <- cor(a[c(1:20),1],rowMeans(a1s[c(1:20),]))
Cor_a2 <- cor(a[c(21:40),2],rowMeans(a2s[c(21:40),]))
Cor_b1 <- cor(d[,1],rowMeans(b1s))
Cor_b2 <- cor(d[,2],rowMeans(b2s))
Cor_b3 <- cor(d[,3],rowMeans(b3s))
Cor_b4 <- cor(d[,4],rowMeans(b4s))

cor441 <- cbind(Cor_a1, Cor_a2, Cor_b1, Cor_b2, Cor_b3, Cor_b4)

cor441

bias <- function(x,y) {
  mean((apply(((x-y)^2), 1, mean)))
}

bias_a1 <- bias(a[c(1:20),1],a1s[c(1:20),])
bias_a2 <- bias(a[c(21:40),2],a2s[c(21:40),])
bias_b1 <- bias(d[,1],b1s)
bias_b2 <- bias(d[,2],b2s)
bias_b3 <- bias(d[,3],b3s)
bias_b4 <- bias(d[,4],b4s) 

bias441 <- cbind(bias_a1, bias_a2, bias_b1, bias_b2, bias_b3, bias_b4)

bias441

##442

out442 <- read.csv("~/Desktop/output442.csv")
rownames(out442) <- out442$X
out442 <- out442[,-1]

a1s <- out442[grep("a1_", rownames(out442)),]
a2s <- out442[grep("a2_", rownames(out442)),]
b1s <- out442[grep("b1_", rownames(out442)),]
b2s <- out442[grep("b2_", rownames(out442)),]
b3s <- out442[grep("b3_", rownames(out442)),]
b4s <- out442[grep("b4_", rownames(out442)),]

Cor_a1 <- cor(a[c(1:20),1],rowMeans(a1s[c(1:20),]))
Cor_a2 <- cor(a[c(21:40),2],rowMeans(a2s[c(21:40),]))
Cor_b1 <- cor(d[,1],rowMeans(b1s))
Cor_b2 <- cor(d[,2],rowMeans(b2s))
Cor_b3 <- cor(d[,3],rowMeans(b3s))
Cor_b4 <- cor(d[,4],rowMeans(b4s))

cor442 <- cbind(Cor_a1, Cor_a2, Cor_b1, Cor_b2, Cor_b3, Cor_b4)

cor442

bias <- function(x,y) {
  mean((apply(((x-y)^2), 1, mean)))
}

bias_a1 <- bias(a[c(1:20),1],a1s[c(1:20),])
bias_a2 <- bias(a[c(21:40),2],a2s[c(21:40),])
bias_b1 <- bias(d[,1],b1s)
bias_b2 <- bias(d[,2],b2s)
bias_b3 <- bias(d[,3],b3s)
bias_b4 <- bias(d[,4],b4s) 

bias442 <- cbind(bias_a1, bias_a2, bias_b1, bias_b2, bias_b3, bias_b4)

bias442

###BIAS results 

bias <- rbind(bias111, bias112, bias221, bias222, bias331, bias332, bias441, bias442)

write.csv(bias, "~/Desktop/biasMLE.csv")

##COR results
cor <- rbind(cor111, cor112, cor221, cor222, cor331, cor332, cor441, cor442)
write.csv(cor, "~/Desktop/corMLE.csv")

