#' Utility functions for converting between "contingency" and "marginal" representations, and between contingency table and vectorised formats.
#' 
#' 
#' @description 
#'   The four arguments used in word attraction measure can be given in two forms in the litterature:
#'
#'- either as the four values (C11, C12, C21, C22) of a contingency table:
#'  
#'  \tabular{lll}{
#'    \tab word1   \tab ¬word1   \cr
#'    word2 \tab C11     \tab C12     \cr
#'    ¬word2 \tab C21     \tab C22     \cr
#'  }
#'
#' where :
#' - C11 is the number of occurrences in context A and B (eg. lexem = 'x' and construction = 'be x')
#' - C12 is the number of occurrences in context A and not B (eg. lexem = 'x' and construction != 'be x')
#' - C21 is the number of occurrences not in context A and in B (eg. lexem != 'x' and construction = 'be x')
#' - C22 is the number of occurrences not in context A and not in context B (eg. lexem != 'x' and construction != 'be x')
#'
#'- or using marginal total :
#'  
#'  \tabular{llll}{
#'    \tab word1   \tab ¬word1      \tab Total \cr
#'    word2  \tab k       \tab             \tab  n \cr
#'    ¬word2 \tab         \tab             \tab    \cr
#'    Total  \tab K       \tab             \tab  N \cr
#'  }
#'
#' where :
#' - N The total number of occurrences in the corpus
#' - n The number of occurrence in the subcorpora
#' - K The total frequency of the form in the corpus
#' - k The subfrequency of the form in the subcorpora
#' 
#' These utility functions help converting between these two forms.
#' 
#' @param contingency a data frame containing columns named C11, C12, C21 and C22
#' 
#' @return a data frame with columns named N, n, K, k.
#' @export
#' @name utilities
#' @examples
#' data(robespierre)
#' peuple_D4 <- robespierre[robespierre$types=="peuple" & robespierre$parts == "D4",]
#' peuple_D4
#' res <- contingency(peuple_D4)
#'
#' 
#' data(happen)
#' happen
#' happen.vec <- cont2vec(happen)
#' happen.vec
#' happen.mar <- marginal(happen.vec)
#' happen.mar
#' res <- do.call(wam.collostruction, as.list(happen.mar))
#' res
marginal <- function(contingency) {
  marginal <- data.frame(
    N=sum(contingency$C11, contingency$C12, contingency$C21, contingency$C22),
    n=sum(contingency$C11, contingency$C12),
    K=sum(contingency$C11, contingency$C21),
    k=contingency$C11
  );
  return(marginal);
}

#' Turn a contingency format into a marginal one.
#'
#' @param marginal a data frame with columns named N, n, K, k.
#'
#' @return a data frame with four columns named after the four arguments.
#' @author Sylvain Loiseau
#' @export
#' @rdname utilities
contingency <- function(marginal) {
    contingency <- data.frame(
      C11=marginal$k,
      C12=marginal$K - marginal$k,
      C21=marginal$n - marginal$k,
      C22=(marginal$N - marginal$K) - (marginal$n - marginal$k)
    );
    return(contingency);
}

#' Turn a contingency format into a marginal one.
#'
#' @param cont a 2 * 2 contingency table
#'
#' @return a data frame with four columns named C11, C12, C21, C22
#' @author Sylvain Loiseau
#' @export
#' @rdname utilities
cont2vec <- function(cont) {
  return(data.frame(
    C11=cont[1,1],
    C12=cont[1,2],
    C21=cont[2,1],
    C22=cont[2,2]
  ))
}