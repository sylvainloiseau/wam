# Dunning 1993 : 74
test.wam.chisq <- function() {
AB <- 110
AnoB <- 2442
noAB <- 111
noAnoB <- 29114
k <- AB
K <- AnoB + k
n <- noAB + k
N <- noAnoB + AB + AnoB + noAB
wam.chisq(N, n, K, k)
expected <- 525.02;
found <- wam.chisq(N, n, K, k, yates.correction=F, p.value = F, two.sided = T)
checkEqualsNumeric(expected, round(found, 2));
}
