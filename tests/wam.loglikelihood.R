# Dunning 1993 : 72
test.wam.loglikelihood <- function() {
AB <- 110
AnoB <- 2442
noAB <- 111
noAnoB <- 29114
k <- AB
K <- AnoB + k
n <- noAB + k
N <- noAnoB + AB + AnoB + noAB
wam.loglikelihood(N, n, K, k)
expected <- 270.72;
found <- wam.loglikelihood(N, n, K, k, p.value = F, two.sided = T)
checkEqualsNumeric(expected, round(found, 2));
}
