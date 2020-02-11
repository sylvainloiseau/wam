### R code from vignette source 'wam.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: wam_setup
###################################################
library(wam);


###################################################
### code chunk number 2: wam_data
###################################################
data(robespierre, package="wam")
head(robespierre)


###################################################
### code chunk number 3: wam_wam
###################################################
wam.res <- wam(data=robespierre, measure=c("loglikelihood", "specificities"))


###################################################
### code chunk number 4: wam_wam3
###################################################
print(wam.res, from=1, to=10, sort.by="specificities", parts="D4");


###################################################
### code chunk number 5: wam_argument_setup
###################################################
data(robespierre, package="wam")
head(robespierre)

peuple_D4 <- robespierre[robespierre$types=="peuple" & robespierre$parts == "D4",]
peuple_D4
N <- peuple_D4$N
n <- peuple_D4$n
K <- peuple_D4$K
k <- peuple_D4$k
maxk <- min(K,n)
maxk
allk <- 0:maxk

expected = round(K * n / N)
expected


###################################################
### code chunk number 6: comparison
###################################################
# data(ar)
# ar <- ar[1,]
# attach(ar)
op <- par(mfrow = c(4, 2)) #pty = "s", fin =c(4,6)
x <- "frequency"
y <- "Association strength"
# square plotting region,independent of device size

plot(1:K, wam.MI(N, n, K, 1:K), type="l", main="Mutual information", xlab = x, ylab = y)
points(expected, wam.MI(N, n, K, expected))

plot(1:K, wam.specificities(N, n, K, 1:K), type="l", main="Specificities", xlab = x, ylab = y)
points(expected, wam.specificities(N, n, K, expected))

plot(1:K, wam.jaccard(N, n, K, 1:K), type="l", main="Jaccard", xlab = x, ylab = y)
points(expected, wam.jaccard(N, n, K, expected))

plot(1:K, wam.loglikelihood(N, n, K, 1:K), type="l", main="Loglikelihood", xlab = x, ylab = y)
points(expected, wam.loglikelihood(N, n, K, expected))

plot(1:K, wam.z(N, n, K, 1:K), type="l", main="Z", xlab = x, ylab = y)
points(expected, wam.z(N, n, K, expected))

plot(1:K, wam.t(N, n, K, 1:K), type="l", main="T", xlab = x, ylab = y)
points(expected, wam.t(N, n, K, expected))

plot(1:K, wam.chisq(N, n, K, 1:K), type="l", main="Chi square", xlab = x, ylab = y)
points(expected, wam.chisq(N, n, K, expected))

#plot(1:K, wam.collostruction(N, n, K, 1:K), type="l", main="Collostruction", xlab = x, ylab = y)
#points(expected, wam.collostruction(N, n, K, expected))

par(op)


###################################################
### code chunk number 7: wam_loglikelihood
###################################################
wam.loglikelihood(N, n, K, k);
expected <- round(K * n / N)
wam.loglikelihood(N, n, K, expected);


###################################################
### code chunk number 8: wam_loglikelihood_graph
###################################################
maxk <- min(K,n)
maxk
allk <- 0:maxk

plot(wam.loglikelihood(N, n, K, allk),
     type="l", xlab="k", ylab="Log likelihood",
	  main="Graph of the Log likelihood function",
	  sub="(N=61449, n=7896, K=296)")
points(k, wam.loglikelihood(N, n, K, k), pch="+")
points(expected, wam.loglikelihood(N, n, K, expected), pch="x")
legend("right",legend=c("k","expected"), pch=c("+","x"), cex=0.75)


###################################################
### code chunk number 9: wam_specificities
###################################################
wam.specificities(N, n, K, k, method="base");
wam.specificities(N, n, K, expected, method="base");


###################################################
### code chunk number 10: wam_specificities_graph
###################################################
plot(wam.specificities(N, n, K, allk, method="base"),
     type="l", xlab="k", ylab="specificities",
	  main="Graph of the specificities function",
	  sub="(N=61449, n=7896, K=296)")
points(k, wam.specificities(N, n, K, k, method="base"), pch="+")
points(expected, wam.specificities(N, n, K, expected, method="base"), pch="x")
legend("top",legend=c("k","expected"), pch=c("+","x"), cex=0.75)


###################################################
### code chunk number 11: wam_specificities_presentation3
###################################################
mode <- floor((n+1)*(K+1)/(N+2));
mode

plot(dhyper(allk, K, N-K, n),
type="l", xlab="k (possible subfrequency of peuple)", ylab="Density of probability",
main="Hypergeometric distribution", sub="density fonction")
points(k, dhyper(k, K, N-K, n), pch="+")
points(mode, dhyper(mode, K, N-K, n), pch="x")


###################################################
### code chunk number 12: wam_specificities_presentation5
###################################################
y <- ifelse(allk <= mode, phyper(allk, K, N-K, n),
                          phyper(allk-1, K, N-K, n, lower.tail=FALSE))
plot(allk, y,
	type="l", xlab="k (possible subfrequency of peuple)",
	ylab="Cumulative probability",
	main="Hypergeometric distribution",
	sub="lower tail for k < mode, upper tail for k >= mode")


###################################################
### code chunk number 13: wam_specificities_presentation5b
###################################################
y <- ifelse(allk <= mode, phyper(allk, K, N-K, n),
                       phyper(allk-1, K, N-K, n, lower.tail=FALSE))
y <- ifelse(allk <= mode, -y, y);
plot(allk, y,
	type="l", xlab="k (possible subfrequency of peuple)",
	ylab="Cumulative probability",
	main="Hypergeometric distribution",
	sub="-(lower tail) for k < mode, upper tail for k >= mode")


###################################################
### code chunk number 14: wam_specificities_presentation5c
###################################################
plot(allk, wam.specificities(N, n, K, allk, method="base"),
	type="l", xlab="k (possible subfrequency of peuple)",
	ylab="Specificities",
	main="Specificities word association measure")


###################################################
### code chunk number 15: wam_specificities_presentation6
###################################################
y <- ifelse(allk <= mode, phyper(allk, K, N-K, n, log.p=TRUE),
                       phyper(allk-1, K, N-K, n, lower.tail=FALSE, log.p=TRUE))
y <- ifelse(allk <= mode, -abs(y), abs(y));
plot(allk, y,
	type="l", xlab="k (possible subfrequency of peuple)",
	ylab="log probability of hypergeometric distribution",
	main="Log-Specificities function");
points(k, phyper(k, K, N-K, n, log.p=TRUE), , pch="+")
points(mode, phyper(mode, K, N-K, n, log.p=TRUE), pch="x")


###################################################
### code chunk number 16: wam_specificities_presentation6b
###################################################
phyper(mode, K, N-K, n, log.p=TRUE)


###################################################
### code chunk number 17: wam_specificities_presentation7
###################################################
y <- ifelse(allk <= mode, phyper(allk, K, N-K, n, log.p=TRUE),
                       phyper(allk-1, K, N-K, n, lower.tail=FALSE, log.p=TRUE))
cdmo <- phyper(mode, K, N-K, n, log.p=TRUE);
y <- ifelse(allk <= mode, -abs(cdmo-y), abs(cdmo-y));
plot(allk, y,
	type="l", xlab="k (possible subfrequency of peuple)",
	ylab="log probability of hypergeometric distribution",
	main="Specificities")
points(k, wam.specificities(N, n, K, k, method="log"), pch="+")
points(mode, wam.specificities(N, n, K, mode, method="log"), pch="x")


###################################################
### code chunk number 18: wam_specificities_presentation8
###################################################
plot(allk, wam.specificities(N, n, K, allk, method="log"),
	type="l", xlab="k (possible subfrequency of peuple)",
	ylab="log probability of hypergeometric distribution",
	main="Specificities")
points(k, wam.specificities(N, n, K, k, method="log"), pch="+")
points(mode, wam.specificities(N, n, K, mode, method="log"), pch="x")


###################################################
### code chunk number 19: wam_specificities_presentation9
###################################################
wam.specificities(N, n, K, mode, method="log");


###################################################
### code chunk number 20: wam_z
###################################################
wam.z(N, n, K, k);
wam.z(N, n, K, mode);


###################################################
### code chunk number 21: wam_z_graph
###################################################
plot(wam.z(N, n, K, allk),
     type="l", xlab="k", ylab="Z",
	  main="Z",
	  sub="(N=61449, n=7896, K=296)")
points(k, wam.z(N, n, K, k), pch="+")
points(mode, wam.z(N, n, K, mode), pch="x")
legend("right",legend=c("k","expected"), pch=c("+","x"), cex=0.75)


###################################################
### code chunk number 22: wam_t
###################################################
wam.t(N, n, K, k);
wam.t(N, n, K, mode);


###################################################
### code chunk number 23: wam_t_graph
###################################################
plot(allk, wam.t(N, n, K, allk),
     type="l", xlab="k", ylab="T",
	  main="t",
	  sub="(N=61449, n=7896, K=296)")
points(k, wam.t(N, n, K, k), pch="+")
points(mode, wam.t(N, n, K, mode), pch="x")
legend("right",legend=c("k","expected"), pch=c("+","x"), cex=0.75)


###################################################
### code chunk number 24: wam_chisq
###################################################
wam.chisq(N, n, K, k);
wam.chisq(N, n, K, mode);


###################################################
### code chunk number 25: wam_chisq_graph
###################################################
plot(allk, wam.chisq(N, n, K, allk),
     type="l", xlab="k", ylab="Chi square",
	  main="Chi square",
	  sub="(N=61449, n=7896, K=296)")
points(k, wam.chisq(N, n, K, k), pch="+")
points(mode, wam.chisq(N, n, K, mode), pch="x")
legend("right",legend=c("k","expected"), pch=c("+","x"), cex=0.75)


###################################################
### code chunk number 26: wam_collostruction
###################################################
#wam.collostruction(N, n, K, k);
#wam.collostruction(N, n, K, expected);


