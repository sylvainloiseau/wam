
#' Print a summary of a \code{\link{WordAssociation-class}} object
#'
#' @param object \code{\link{WordAssociation-class}}. 
#'
#' @exportMethod summary
#'
#' @examples
#' data(robespierre)
#' res <- wam(data=robespierre, measure = c("chisq", "loglikelihood"))
#' summary(res)
setMethod("summary", signature(object = "WordAssociation"), function(object) {
  cat(paste("Word association measure for ", length(unique(types(object))), " types in ", length(unique(parts(object))), " parts.\n", sep=""));
})

#' Show a \code{\link{WordAssociation-class}} object
#'
#' @param object \code{\link{WordAssociation-class}}. 
#'
#' @examples
#' data(robespierre)
#' res <- wam(data=robespierre, measure = c("chisq", "loglikelihood"))
#' show(res)
setMethod("show", signature(object="WordAssociation"), function(object) {
  print(object);
});

#' Print a \code{\link{WordAssociation-class}} object
#'
#' Pretty-print the word attractions measures in a \code{\link{WordAssociation-class}} object. The lexical types
#' are grouped by subcorpora and, in each subcorpora, print the types sorted according to the requested word association measure.
#' 
#' @param x \code{\link{WordAssociation-class}}
#' @param from rank of the first (sorted) attracted forms printed
#' @param to rank of the last (sorted) attracted forms printed
#' @param threshold print only attracted forms with association strength above the threshold
#' @param types character. Print only the given types.
#' @param parts character. Print only the given subcorpora.
#' @param sort.by character. The name of one of the word association indicator available in the  \code{\link{WordAssociation-class}} object for sorting the lexical types in each subcorpora. By default, the first indicator (indicator.name(x)[1])
#' @param file character. If no empty, print the table in the given file
#' @param append logical. If the \code{file} argument is no empty, select erasing file or adding the table at the end of the file, preserving existing content.
#'
#' @exportMethod print
#'
#' @examples
#' data(robespierre)
#' res <- wam(data=robespierre, measure = c("chisq", "loglikelihood"))
#' print(res)
#' # or:
#' res
setMethod("print", signature(x="WordAssociation"), function(x, from=1, to=100, threshold=NULL,
        types=NULL, parts=NULL,
        sort.by=indicator.name(x)[1], file="", append=FALSE) {

  printable <- .get.printable(x, from, to, threshold, types, parts, sort.by);

  msg <- paste("Printing association measure for", length(printable), "part(s);");
  if (!is.null(threshold)) {
    msg <- paste(msg, "threshold:", threshold, "\n");
  } else {
    msg <- paste(msg, "from:", from, "to:", to, "\n");
  }
  cat(msg, file=file, append=append);

  indicators <- indicator.name(x);

  cat(paste("Corpus size:", N(x)[1], "\n"), file=file, append=append);
  cat(paste("Sorted by:", sort.by, "\n"), file=file, append=append);
  header <- sprintf("%-20s | %8s | %8s ", "word", "sub freq", "tot freq");
  for (i in indicators) {
    header <- paste(header, sprintf("| %15s ", i), sep="");
  }
  l <- nchar(header);
  header_separator_line <- paste(paste(rep("-", l), collapse=""), "\n", sep="");

  cat(header_separator_line, file=file, append=append);
  cat(paste(header, "\n", sep=""), file=file, append=append);
  cat(header_separator_line, file=file, append=append);

  part_separator_line <- paste(paste(rep(".", l), collapse=""), "\n", sep="");
  for (i_part in 1:length(printable)) {
    part <- printable[[i_part]];
    cat(part_separator_line, file=file, append=append);
    cat(paste("Part name:", part$part[1], "\n"), file=file, append=append);
    cat(paste("Part size:", part$n[1], "tokens.", "\n"), file=file, append=append);
    cat(paste("Positive specificities printed:", sum(part[,7] > 0), "\n"), file=file, append=append);
    cat(paste("Negative specificities printed:", sum(part[,7] < 0), "\n"), file=file, append=append);
    if (nrow(part) > 0) {
      for (i_word in 1:nrow(part)) {
        word_name <- part[i_word, "types"];
        word_sub_freq <- part[i_word, "k"];
        word_tot_freq <- part[i_word, "K"];
        line <- sprintf("%-20s | %8.0f | %8.0f ", word_name, word_sub_freq, word_tot_freq)
	for (i in indicators) {
          line <- paste(line, sprintf("| %15.2f ", part[i_word, i]), sep="");
	}
	cat(paste(line, "\n", sep=""), file=file, append=append);
      }
    }
  }
});

.get.printable <- function(x, from, to, threshold, types=NULL, parts=NULL, sort.by) {

  if(!class(x) == "WordAssociation") stop("x must be of class WordAssociation");

  if (is.null(sort.by)) {
      stop(paste("'sort.by' cannot be null"));
  }
  if(!length(sort.by) == 1) {
    stop(paste("cannot sort with more than one key"));
  }

  if (!is.null(types)) {
    if (any(!(types %in% types(x)))) {
		 stop("some types are not availabe")
    }
  }

  if (!is.null(parts)) {
     if (any(!(parts %in% parts(x)))) {
 		 stop("some parts are not availabe")
     }
  }

  if (is.null(threshold)) {
    if (is.null(from) | is.null(to)) {
      stop("either 'threshold' or 'from' and 'to' options must be given");
    }
    if ((!is.numeric(from)) | (!is.numeric(to))) {
      stop("both 'from' and 'to' must be numeric");
    }
    if ((length(from) > 1) | (length(to) > 1)) {
      stop("both 'length(from)' and 'length(to)' must be '1'");
    }
    if (from >= to) {
      stop("'to' must be greater than 'from'");
    }
    if (from < 1) {
      stop("'from' must be greater than 0");
    }
  } else {
    if (!is.numeric(threshold)) {
      stop("'threshold' must be numeric");
    }
    if (length(threshold) > 1) {
      stop("'threshold' must be of length 1");
    }
    if (threshold <= 0) {
      stop("'threshold' must be greater than 0");
    }
  }
  
  df <- data.frame(types=types(x), parts=parts(x), N=N(x), n=n(x), K=K(x), k=k(x));
  df <- cbind(df, association(x));
  if(!sort.by %in% colnames(df)) {
    stop(paste("cannot sort by '", sort.by, "': no column of that names (in: ", paste(colnames(df), collapse=" "), ")", sep=""));
  }

  if (!is.null(types)) {
    df <- df[ df$types %in% types, ];
  }

  if (!is.null(parts)) {
    df <- df[ df$parts %in% parts, ];
    df$parts <- droplevels(df$parts);
  }

  if (!is.null(threshold)) {
    df <- df[ abs(df[, sort.by]) > threshold, ];
  }

  df <- df[order(df[, sort.by], decreasing=TRUE),];

  l <- split(df, df$parts);

  # extract lines 'from' to 'to' in both positive and negative values
  if (!is.null(from)) {
    l <- lapply(l, function(x) {
        n.negative <- sum(x[, sort.by] < 0);
       .matrix.head.tail(x, from, to, n.negative);
    });
  }
  return(l);
}

#' Private function
#'
#' @param x the matrix
#' @param from first index of the row to kept
#' @param to last index of the row to kept
#' @param n.negative number of row with negative value. If > 0, the value from and to are used for collecting rows, symetrically, at the bottom of the matrix.
#'
#' @noRd
.matrix.head.tail <- function(x, from, to, n.negative) {
    n.positive <- nrow(x) - n.negative;
    index <- 0;
    if (from > n.positive) {
        index <- numeric();
    } else {
       index <- from:(min(to, n.positive));
    }
    if (n.negative > 0) {
        if (from > n.negative) {
        } else {
           from.negative <- nrow(x) + 1 - from;
           to.negative <- nrow(x) + 1 - to;
           index.first.negative <- nrow(x) + 1 - n.negative;
           index <- c(index, (max(to.negative, index.first.negative)):from.negative);
        }
    }
    x <- x[index, ];
    return(x);
}

#' @importFrom utils write.csv
writeWordAssociation <- function(x, file, from=1, to=50, threshold=NULL, types=NULL, parts=NULL) {
  if(!class(x) == "specificities") stop("x must be of class specificities");
  if(is.null(file)) stop("file cannot be null");
  if (is.null(types)) types <- attr(x, "types")
  if (is.null(parts)) parts <- attr(x, "parts")
  printable <- .get.printable(x, from, to, threshold, types, parts);
  for (i in 1:length(printable)) {
      write.csv(printable[[i]], file=paste(file, "_", i, ".csv", sep=""));
  }
}

writeAsXML.wma <-
function(x, file, from=1, to=100, threshold=NULL, types=NULL, parts=NULL) {
  if(!class(x) == "specificities") stop("x must be of class specificities");
  if(is.null(file)) stop("file cannot be null");
  if (is.null(types)) types <- attr(x, "types")
  if (is.null(parts)) parts <- attr(x, "parts")
  printable <- .get.printable(x, from, to, threshold, types, parts);
  .saveDataFrameAsXML(as.data.frame(printable), file);
}

.saveDataFrameAsXML <- function(data, filename) {
  r <- nrow(data);
  c <- ncol(data);
  rname <- rownames(data);
  cname <- colnames(data);
  write("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>", file=filename);
  write("<table>", file=filename, append=T);
  if (length(cname) > 0) {
    write("<row role=\"label\">", file=filename, append=T);
    write("<cell/>", file=filename, append=T);
    for(i in cname) {
      write("<cell>", file=filename, append=T);
      write(i, file=filename, append=T);
      write("</cell>", file=filename, append=T);
    }
    write("</row>", file=filename, append=T);
  }
  if (length(r) > 0) {
    for(i in 1:r) {
      write("<row role=\"data\">", file=filename, append=T);
      write("<cell role=\"label\">", file=filename, append=T);
      write(rname[i], file=filename, append=T);
      write("</cell>", file=filename, append=T);
      for(j in 1:c) {
        write("<cell>", file=filename, append=T);
        write(data[i,j], file=filename, append=T);
        write("</cell>", file=filename, append=T);
      }
      write("</row>", file=filename, append=T);
    }
  }
  write("</table>", file=filename, append=T);
}
