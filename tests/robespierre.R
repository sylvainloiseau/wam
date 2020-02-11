
# Test the data set robespierre

test.wam.specificities.dataset <- function() {
	data(robespierre, package="wam")
	N <- sum(robespierre$k)
	checkEqualsNumeric(N, robespierre$N[1])
}
