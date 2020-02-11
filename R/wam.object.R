#' Contains word association measure indicators.
#' 
#' See \code{\link{wam}} for constructing a WordAssociation object.
#'
#' @slot N numeric. 
#' @slot n numeric. 
#' @slot K numeric. 
#' @slot k numeric. 
#' @slot association matrix. 
#' @slot indicator.name character. 
#' @slot types character. 
#' @slot parts character. 
#'
#' See the functions \code{\link{N-big}}, \code{\link{n-small}}, \code{\link{K-big}}, \code{\link{k-small}},
#' \code{\link{types}}, \code{\link{parts}}, \code{\link{indicator.name}} and \code{\link{association}}
#' for retrieving these pieces of information.
#' 
#' @return a \code{\link{WordAssociation-class}} object
#'
#' @exportClass WordAssociation
#'
#' @author Sylvain Loiseau
#' @seealso \code{\link{wam}}
setClass(
    "WordAssociation",
    representation(
      N = "numeric",
      n = "numeric",
      K = "numeric",
      k = "numeric",
      association="matrix",
      indicator.name="character",
      types="character",
      parts="character"
      )
    );

#' Compute association measure and construct a \code{\link{WordAssociation-class}} object.
#'
#' @param data : A data frame with the following columns:
#' N, n, K, k, types, parts: 
#'   \itemize{
#'     \item{N}{number of tokens in the corpus}
#'     
#'     \item{n}{number of tokens in the subcorpus}
#'     
#'     \item{K}{frequency of the type in the corpus}
#'     
#'     \item{k}{frequency of the type in the subcorpus}
#'     
#'     \item{types}{Optional : linguistic types}
#'     
#'     \item{parts}{Name of the subset of the corpus with which
#'       the attraction is measured}
#'     
#'   }
#' @param measure a function implementing statistical attraction measures. These functions must accept 'N', 'n', 'K' and 'k' arguments. See the help page \code{wam-functions}.
#' @param ... argument provided to the function of the "measure" parameter.
#'
#' @return a \code{\link{WordAssociation-class}} object
#' @export
#'
#' @seealso \code{\link{WordAssociation-class}}, \code{wam-functions}
#' @import methods
#' @examples
#' data(robespierre)
#' wam(data=robespierre, measure = c("chisq", "loglikelihood"))
wam <- function(data, measure = "loglikelihood", ...) {
  types <- data$types;
  parts <- data$parts;
  N <- data$N;
  n <- data$n;
  K <- data$K;
  k <- data$k;

  types <- as.character(types);
  if (!is.character(types)) {
    stop("types must be a character vector");
  }
  
  if (!is.character(measure)) {
    stop("'measure' must be a character vector of function name");
  }
  
  if (is.null(parts)) {
    parts <- rep("subpart", length(types));
  }
  parts <- as.character(parts)
  if (!is.character(parts)) {
    stop("parts must be a character vector");
  }
  if (is.null(N) | is.null(n) | is.null(K) | is.null(k)) {
    stop("none of the four arguments N, n, K, k can be null");
  }
  if (any(is.na(N)) | any(is.na(n)) | any(is.na(K)) | any(is.na(k))) {
    stop("no element of the four arguments N, n, K, k can be NA");
  }
  association <- matrix(0, nrow=max(length(N), length(n), length(K), length(k)), ncol=length(measure));
  measure <- c(measure); # otherwise "objet de type 'closure' non indiçable" error with length(measure) = 1
  i <- 1;
  for (m.names in measure) {
    m <- get(paste("wam.", m.names, sep=""));
    if (any(! c("N", "n", "K", "k") == names(formals(m))[1:4] ))  {
      stop(paste( "a function provided in 'measure' do not accept the N, n, K, k arguments"));
    }
    indicateurs <- m(N, n, K, k, ...);
    if (all(is.na(indicateurs))) {
      stop(paste("the measure '", m.names, "' yield only NA."));
    }
    association[,i] <- indicateurs;
    i <- i + 1;
  }
  colnames(association) <- measure;
  return(methods::new("WordAssociation",
             N=N, n=n, K=K, k=k,
             association=association,
             indicator.name=measure,
             types=types,
             parts=parts));
}

#' Number of tokens in its corpus for each forms in a \code{\link{WordAssociation-class}} object.
#'
#' @param obj a \code{\link{WordAssociation-class}} object.
#'
#' @return a numeric vector
#' @docType methods
#' @exportMethod N
#'
#' @examples
#' data(robespierre)
#' x <- wam(data=robespierre, measure = c("chisq", "loglikelihood"))
#' N(x)
#' @name N-big
setGeneric("N", function(obj) {
  return(standardGeneric("N"));
})

#' @name N
#' @aliases N,WordAssociation-method
#' @docType methods
#' @rdname N-big
setMethod("N", "WordAssociation", function(obj) obj@N)

#' Number of tokens in its subcorpus for each forms in a \code{\link{WordAssociation-class}} object.
#'
#' @param obj a \code{\link{WordAssociation-class}} object.
#'
#' @return a numeric vector
#' @exportMethod n
#' @docType methods
#' @name n-small
#' @examples
#' data(robespierre)
#' x <- wam(data=robespierre, measure = c("chisq", "loglikelihood"))
#' n(x)
setGeneric("n", function(obj) {
  return(standardGeneric("n"));
})

#' @name n
#' @aliases n,WordAssociation-method
#' @docType methods
#' @rdname n-small
setMethod("n", "WordAssociation", function(obj) obj@n)

#' Total frequency of forms in a \code{\link{WordAssociation-class}} object
#'
#' @param obj a \code{\link{WordAssociation-class}} object.
#'
#' @return a numeric vector
#' @exportMethod K
#' @docType methods
#' @name K-big
#' @examples
#' data(robespierre)
#' x <- wam(data=robespierre, measure = c("chisq", "loglikelihood"))
#' K(x)
setGeneric("K", function(obj) {
  return(standardGeneric("K"));
})

#' @name K
#' @aliases K,WordAssociation-method
#' @docType methods
#' @rdname K-big
setMethod("K", "WordAssociation", function(obj) obj@K)

#' Subfrequencies of forms in a \code{\link{WordAssociation-class}} object.
#'
#' @param obj a \code{\link{WordAssociation-class}} object.
#'
#' @return a numeric vector
#' @exportMethod k
#' @name k-small
#' @examples
#' data(robespierre)
#' x <- wam(data=robespierre, measure = c("chisq", "loglikelihood"))
#' k(x)
setGeneric("k", function(obj) {
  return(standardGeneric("k"));
})

#' @name k
#' @aliases k,WordAssociation-method
#' @docType methods
#' @rdname k-small
setMethod("k", "WordAssociation", function(obj) obj@k)

#' Get the association strengh for all association computed in this \code{\link{WordAssociation-class}} object
#'
#' @param obj a \code{\link{WordAssociation-class}} object.
#' @return a numeric vector
#' @exportMethod association
#' @examples
#' data(robespierre)
#' x <- wam(data=robespierre, measure = c("chisq", "loglikelihood", "specificities"))
#' indicator.name(x)
setGeneric("association", function(obj) {
  return(standardGeneric("association"));
})

#' @name association
#' @aliases association,WordAssociation-method
#' @docType methods
setMethod("association", "WordAssociation", function(obj) obj@association)

#' The names of the statistical indicators used in a \code{\link{WordAssociation-class}} object
#'
#' @param obj a \code{\link{WordAssociation-class}} object.
#'
#' @return a character vector
#' @exportMethod indicator.name
#' @docType methods
#' @examples
#' data(robespierre)
#' x <- wam(data=robespierre, measure = c("chisq", "loglikelihood", "specificities"))
#' indicator.name(x)
setGeneric("indicator.name", function(obj) {
  return(standardGeneric("indicator.name"));
})

#' @name indicator.name
#' @aliases indicator.name,WordAssociation-method
#' @docType methods
setMethod("indicator.name", "WordAssociation", function(obj) obj@indicator.name)

#' List of the types of a corpus
#'
#' @param obj a \code{\link{WordAssociation-class}} object.
#'
#' @return A character vector containing the list of the linguistic
#' types corresponding to the computed association measure.
#' @exportMethod types
#' @docType methods
#' @examples
#' data(robespierre)
#' x <- wam(data=robespierre, measure = c("chisq", "loglikelihood"))
#' types(x)
setGeneric("types", function(obj) {
  return(standardGeneric("types"));
})

#' @name types
#' @aliases types,WordAssociation-method
#' @docType methods
setMethod("types", "WordAssociation", function(obj) obj@types)

#' The part of the corpus each entry belong to in a \code{\link{WordAssociation-class}} object
#'
#' @param obj a \code{\link{WordAssociation-class}} object.
#'
#' @return a character vector
#' @exportMethod parts
#' @docType methods
#' @examples
#' data(robespierre)
#' x <- wam(data=robespierre, measure = c("chisq", "loglikelihood"))
#' parts(x)
setGeneric("parts", function(obj) {
  return(standardGeneric("parts"));
})

#' @name parts
#' @aliases parts,WordAssociation-method
#' @docType methods
setMethod("parts", "WordAssociation", function(obj) obj@parts)

