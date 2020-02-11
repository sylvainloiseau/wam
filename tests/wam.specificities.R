
# See : Lafon Pierre (1980) Sur la variabilité de la fréquence des formes dans un corpus, 1/1  pp. 127-165
test.wam.specificities.peuple_D4 <- function() {
	data(robespierre, package="wam");
	peuple_D4 <- robespierre[robespierre$types == "peuple" & robespierre$parts == "D4",];
	N <- peuple_D4$N;
	n <- peuple_D4$n;
	K <- peuple_D4$K;
	k <- peuple_D4$k;
	# see Lafon 1980 : 149
	expected <- -66.937*10^-4;
	found <- wam.specificities(N, n, K, k, method="base");	
	checkEqualsNumeric(expected, found)
}

# Lafon 1980 : 140-141
test.wam.specificities.peuple_D9 <- function() {
   N = 61449;
   n = 1084;
   K = 296;
   k = 4;
	dhyper(N, n, K, 11)

}