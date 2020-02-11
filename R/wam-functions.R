#' Calculate word association measure according to various fomula
#'
#' Word association measures give an information about the tendency of two
#' words to co-occurr with greater (or lesser) than chance frequency.
#'
#' @param N numeric vector, the total number of occurrences in the corpus
#' @param n numeric vector, The frequency of word 1
#' @param K numeric vector, The frequency of word 2
#' @param k numeric vector, The frequency of the cooccurrence or word 1 and word 2.
#'
#' @export
#' @return a numeric vector of association strength
#'
#' @seealso see \code{\link{wam}} for another, higher level interface.
#'
#' @author  Bernard Desgraupes
#' @author Sylvain Loiseau
#' @name wam-function
wam.jaccard <- function(N, n, K, k) {
  return(k / (K + n - k));
}

#' Calculate word association according to the information theoretic concept of Mutual information: \eqn{MI(x,y) =  P(x,y) / P(x) P(y)}
#' 
#' @param N numeric vector, the total number of occurrences in the corpus
#' @param n numeric vector, The frequency of word 1
#' @param K numeric vector, The frequency of word 2
#' @param k numeric vector, The frequency of the cooccurrence or word 1 and word 2.
#'
#' @return a numeric vector of association strength
#'
#' @export
#' @rdname  wam-function
wam.MI <- function(N, n, K, k) {
  expected <- (K/N) * (n/N);
  return(ifelse(k > expected, log10(k / expected), -log10(k / expected)));
}

#' Absolute frequencies.
#' 
#' @param N numeric vector, the total number of occurrences in the corpus
#' @param n numeric vector, The frequency of word 1
#' @param K numeric vector, The frequency of word 2
#' @param k numeric vector, The frequency of the cooccurrence or word 1 and word 2.
#'
#' @return a numeric vector of association strength
#'
#' @export
#' @rdname wam-function
wam.frequency <- function(N, n, K, k) {
  return(k);
}

#' Compute word association measure according to loglikelihood (Dunning 1993)
#' 
#' @param N numeric vector, the total number of occurrences in the corpus
#' @param n numeric vector, The frequency of word 1
#' @param K numeric vector, The frequency of word 2
#' @param k numeric vector, The frequency of the cooccurrence or word 1 and word 2.
#' @param p.value length 1 logical vector: is p value used as the indicator.
#' @param two.sided length 1 logical vector
#'
#' @return a numeric vector of association strength
#' @references Dunning, T. 1993.  « Accurate methods for the statistics of surprise and coincidence ».  In: \emph{Computational linguistics}.  19/1.  MIT Press, pp.  61-74
#'
#' @export
#' @rdname wam-function
#' @importFrom stats pnorm
#' @importFrom stats pchisq
wam.loglikelihood <- function(N, n, K, k, p.value=FALSE, two.sided=TRUE) {
  #N <- N;
  C1 <- K; # column
  C2 <- N-K;
  R1 <- n; # row
  R2 <- N-n;
  O11 <- k; # observed
  O12 <- n-k;
  O21 <- K-k;
  O22 <- C2 - O12;
  E11 = R1 * C1 / N; # expected
  E12 = R1 * C2 / N;
  E21 = R2 * C1 / N;
  E22 = R2 * C2 / N;

  ll <- 2 * (
      ifelse(O11 > 0, O11 * log(O11 / E11), 0) +
      ifelse(O12 > 0, O12 * log(O12 / E12), 0) +
      ifelse(O21 > 0, O21 * log(O21 / E21), 0) +
      ifelse(O22 > 0, O22 * log(O22 / E22), 0)
      );

  if (! two.sided) {
    if (! p.value) stop("p.value mandatory with 'two.sided=FALSE'");
    z <- ifelse(O11 >= E11, sqrt(ll), -sqrt(ll));
    return(pnorm(z, 0, 1, lower.tail=FALSE));
  }

  if (p.value) {
    ll <- pchisq(ll, 1, lower.tail=FALSE);
  }
  return(ll);
}

#' Calculate word association according to the collostruction method (Stefanowitsch and Gries 2003).
#'
#' @param N numeric vector, the total number of occurrences in the corpus
#' @param n numeric vector, The frequency of word 1
#' @param K numeric vector, The frequency of word 2
#' @param k numeric vector, The frequency of the cooccurrence or word 1 and word 2.
#'
#' @export
#' @return a numeric vector of association strength
#' @rdname wam-function
#' @importFrom stats fisher.test
#' @references Stefanowitsch A. \& Gries St. Th. 2003 "Collostructions: Investigating the interaction of words and constructions", \emph{International Journal of Corpus Linguistics}, 8/2, 209-234. http://www.anglistik.uni-muenchen.de/personen/professoren/schmid/schmid_publ/collostructional-analysis.pdf
wam.collostruction <- function(N, n, K, k) {
  # res <- mapply(function(N, n, K, k) {
  #   c <- contingency(N, n, K, k);
  #   return(fisher.test(c)$p.value)
  # }, N, n, K, k);
  # return(res);
  mo <- floor((n+1)*(K+1)/(N+2));
  #x <- data.frame(N, n, K, k, mo)
  x <- contingency(data.frame(N=N, n=n, K=K, k=k, stringsAsFactors = FALSE))
  res <- vector(mode="double", length=nrow(x))
  
  for (i in 1:nrow(x)) {
    # N <- x[i,"N"]
    # n <- x[i,"n"]
    # K <- x[i,"K"]
    # k <- x[i,"k"]
    # mo <- mo[i]
    crosst <- matrix(as.numeric(x[i,c("C11", "C12", "C21", "C22")]), 2, 2);
    fisher <- fisher.test(crosst);
    res[i] <- fisher$p.value
  }
  return(res)
  #islog <- TRUE;
  #cdk <- ifelse(k <= mo, phyper(k, K, N-K, n, log.p=islog), phyper(k-1, K, N-K, n, log.p=islog, lower.tail=FALSE));
  #cdmo <- phyper(mo, K, N-K, n, log.p=islog);
  #collo <- ifelse(k <= mo, -abs(cdmo-cdk), abs(cdmo-cdk));
  #return(collo)
}

#' Calculate word association according to the specificities method (hypergeometric distribution) (Lafon 1980).
#' 
#' @param N numeric vector, the total number of occurrences in the corpus
#' @param n numeric vector, The frequency of word 1
#' @param K numeric vector, The frequency of word 2
#' @param k numeric vector, The frequency of the cooccurrence or word 1 and word 2.
#' @param method character: one of log, logscale, base, logscale, gap
#'
#' @return a numeric vector of association strength
#' @references Lafon, P. (1980). « Sur la variabilité de la fréquence des formes dans un corpus ». In: \emph{Mots}. 1. pp. 127--165. \url{http://www.persee.fr/web/revues/home/prescript/article/mots_0243-6450_1980_num_1_1_1008}
#' @export
#' @rdname wam-function
#' @importFrom stats phyper
wam.specificities <- function(N, n, K, k, method="log") {
# mode
  mo <- floor((n+1)*(K+1)/(N+2));

# cdf (or its log)
  islog <- (method %in% c("log","logscale"))
    cdk <- ifelse(k <= mo, phyper(k, K, N-K, n, log.p=islog), phyper(k-1, K, N-K, n, log.p=islog, lower.tail=FALSE));
  specif <- double(length(k));

  if (method == "base") {
    specif <- ifelse(k <= mo, -cdk, cdk);
  } else {
# cumulative probability for the mode (or its log)
    cdmo <- phyper(mo, K, N-K, n, log.p=islog);
    if (method == "gap") {
      specif <- ifelse(k <= mo, -abs(cdmo-cdk), abs(cdmo-cdk));
    } else if (method == "log") {
      specif <- ifelse(k <= mo, -abs(cdmo-cdk), abs(cdmo-cdk));
    } else if (method == "scale") {
      specif <- ifelse(k <= mo, -abs(cdmo-cdk)/cdmo, abs(cdmo-cdk)/cdmo);
    } else if (method == "logscale") {
      specif <- ifelse(k <= mo, -abs((cdmo-cdk)/cdk) , abs((cdmo-cdk)/cdk));
    }
  }
  return(specif);
}

#' Word association measure based on the Z score.
#' 
#' @param N numeric vector, the total number of occurrences in the corpus
#' @param n numeric vector, The frequency of word 1
#' @param K numeric vector, The frequency of word 2
#' @param k numeric vector, The frequency of the cooccurrence or word 1 and word 2.
#' @param yates.correction logical: yates correction
#'
#' @return a numeric vector of association strength
#' @references Church, K., Gale, W., Hanks, P. & Hindle, D., 1991.  « Using Statistics in Lexical Analysis ».  In : Zernik, U. (ed.), \emph{Lexical Acquisition}.  Hillsdale, Lawrence Erlbaum Ass., pp. 115--164.
#' @export
#' @rdname wam-function
wam.z <- function(N, n, K, k, yates.correction=FALSE) {
  O11 <- k;
  E11 = (K * n) / N;

  diff <- O11 - E11;
  if (yates.correction) {
    diff <- diff + ifelse(O11 > E11, -0.5, 0.5);
  }
  return(diff / sqrt(E11));
}

#' Calculate word association according to the T test.
#' 
#' \verb{
#'  t-score =  prob(X=chemistry, Y=physics) - ( prob(X=chemistry) prob(Y=physics) )  
#'  ----------------------------------------------------------------------
#'    sqrt((1/T) prob(X=chemistry, Y=physics))
#'}
#'
#' @return a numeric vector of association strength
#' @param N numeric vector, the total number of occurrences in the corpus
#' @param n numeric vector, The frequency of word 1
#' @param K numeric vector, The frequency of word 2
#' @param k numeric vector, The frequency of the cooccurrence or word 1 and word 2.
#' @export
#'
#' @rdname wam-function
wam.t <- function(N, n, K, k) {
  #(%O11% - %E11%) / sqrt(%O11%)
  O11 <- k;
  E11 = n * K / N;

  t <- (O11 - E11) / sqrt(O11);
  #  - pnorm(t, 0, 1, lower.tail=FALSE, log.p=TRUE) / log(10)
  return (t);
}

#' Calculate word association according to the chi square test. (Church, Gale \emph{et al.} 1991)
#'
#' @param N numeric vector, the total number of occurrences in the corpus
#' @param n numeric vector, The frequency of word 1
#' @param K numeric vector, The frequency of word 2
#' @param k numeric vector, The frequency of the cooccurrence or word 1 and word 2.
#' @param yates.correction length 1 numeric vector
#' @param p.value length 1 numeric vector
#' @param two.sided length 1 numeric vector
#'
#' @return a numeric vector of association strength
#' @export
#' @references Church, K., Gale, W., Hanks, P. & Hindle, D., 1991.  «
#' Using Statistics in Lexical Analysis ».  In : Zernik, U. (ed.),
#' \emph{Lexical Acquisition}.  Hillsdale, Lawrence Erlbaum Ass., pp.
#' @rdname wam-function
#' @importFrom stats pchisq
wam.chisq <- function(N, n, K, k, yates.correction=TRUE, p.value=TRUE, two.sided=FALSE) {
  C1 <- K;
  C2 <- N-K;
  R1 <- n;
  R2 <- N-n;
  O11 <- k;
  O12 <- n-k;
  O21 <- K-k;
  O22 <- C2 - O12;
  E11 = R1 * C1 / N;
  E12 = R1 * C2 / N;
  E21 = R2 * C1 / N;
  E22 = R2 * C2 / N;

  if (yates.correction) {
    chi_squared = N * (abs(O11 * O22 - O12 * O21) - N / 2) ** 2 / (R1 * R2 * C1 * C2);
  } else {
    chi_squared = N * (O11 * O22 - O12 * O21) ** 2 / (R1 * R2 * C1 * C2);
  }

  if (! two.sided) {
    if (! p.value) stop("p.value mandatory with 'two.sided=FALSE'");
    z <- ifelse(O11 >= E11, sqrt(chi_squared), -sqrt(chi_squared));
    return(pnorm(z, 0, 1, lower.tail=FALSE));
  }

  if (p.value) {
    return (pchisq(chi_squared, 1, "upper") );
  } else {
    return(chi_squared);
  }

}

#' Calculate word association according to the association ratio (Church and Hanks 1990).
#' 
#' @param N numeric vector, the total number of occurrences in the corpus
#' @param n numeric vector, The frequency of word 1
#' @param K numeric vector, The frequency of word 2
#' @param k numeric vector, The frequency of the cooccurrence or word 1 and word 2.
#'
#' @return a numeric vector of association strength
#'
#' @references Kenneth Ward Church, Patrick Hanks (1990) "Word Association Norms,
#' Mutual Information, and Lexicography" \emph{Computational Linguistics},
#' 16/1, pages 22-29.
#' \url{http://www.aclweb.org/anthology/P89-1010.pdf}
#'
#' @rdname wam-function
#' @export
#' @examples
#' data(ar)
#' attach(ar)
#' wam.ar(N, n, K, k)
wam.ar <- function(N, n, K, k) {
  log2((k/N)/((n/N)*(K/N)))
}

#' @param N numeric vector, the total number of occurrences in the corpus
#' @param n numeric vector, The frequency of word 1
#' @param K numeric vector, The frequency of word 2
#' @param k numeric vector, The frequency of the cooccurrence or word 1 and word 2.
#' @return a numeric vector of association strength
#'
#' @rdname wam-function
wam.g <- function(N, n, K, k) {
  stop("not implemented yet")
}
