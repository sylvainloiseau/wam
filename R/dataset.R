#' A contingency table lexem ~ construction
#' 
#'   A contingency table lexem ~ construction
#'   A 2*2 contingency table containing the frequency of lexem in the slot of a construction :
#'     \tabular{lrr}{
#'       \tab accident\tab !accident\cr
#'       N waiting to happen \tab       14\tab        21\cr
#'       !N waiting to happen\tab     8606\tab  10197659\cr
#'     }
#'
#' @name happen
#'
#' @usage data(happen)
#' @format A 2*2 numeric matrix
#' @docType data
#' @references 
#'   Stefanowitsch A. \& Gries St. Th. 2003 "Collostructions: Investigating the interaction of words and constructions", \emph{International Journal of Corpus Linguistics}, 8/2, 209-234, p. 219.
#' @examples
#' data(happen)
#' happen.vec <- cont2vec(happen)
#' happen.mar <- marginal(happen.vec)
#' attach(happen.mar)
#' wam.collostruction(N, n, K, k)
#' detach(happen.mar)
NULL

#' Data for testing word association measures.
#'
#' The data are presented in Kenneth Ward Church, Patrick Hanks (1990) \emph{Computational Linguistics}, 16/1, pages 22-29.
#'
#' @name ar
#' @docType data
#'
#' @usage data(ar)
#' @format
#'   A data frame with 4 observations on the following 6 variables:
#'     \describe{
#'       \item{\code{N}}{a numeric vector} The corpus size
#'       \item{\code{word1}}{a character vector} the first wordform
#'       \item{\code{n}}{a numeric vector} the discourses sizes
#'       \item{\code{word2}}{a character vector} the second wordform
#'       \item{\code{K}}{a numeric vector} The total frequency of the word under scrutiny
#'       \item{\code{k}}{a numeric vector} The subfrequency of the 6 words in the 10 discourses.
#'     }
#' 
#' @references 
#'   Kenneth Ward Church, Patrick Hanks (1990) \emph{Computational Linguistics}, 16/1, pages 22-29.
#' 
#' @examples
#' data(ar)
#' attach(ar)
#' wam.ar(N, n, K, k)
#' detach(ar)
NULL

#'   A tiny corpus ready for word association measures.
#'   
#'   A tiny corpus containing with frequencies of 6 words in 10 discourses par Robespierre (between november 1793 and july 1794)
#'   
#'   This data frame study 6 words in 10 discourses ; it contains therefore 60 rows.
#'   
#'   The words : \code{de} \code{ennemi} \code{others} \code{patrie} \code{peuple}
#'   \code{republique}.
#'   
#' @name robespierre
#' @docType data
#' @usage data(robespierre)
#' @format
#'   A data frame with 60 observations on the following 6 variables.
#'   \describe{
#'     \item{\code{N}}{a numeric vector} The corpus size
#'     \item{\code{n}}{a numeric vector} the discourses sizes
#'     \item{\code{K}}{a numeric vector} The total frequency of the word under scrutiny
#'     \item{\code{k}}{a numeric vector} The subfrequency of the 6 words in the 10 discourses.
#'     \item{\code{types}}{The words represented by this line}
#'     \item{\code{parts}}{The part concerned with this line}
#'   }
#' @references 
#'   LAFON, Pierre, 1980. « Sur la variabilité de la fréquence des formes dans un corpus ». In : Mots. 1. pp. 127--165.
#'
#' @examples
#' data(robespierre)
#' wam(data=robespierre, measure = "chisq")
NULL
