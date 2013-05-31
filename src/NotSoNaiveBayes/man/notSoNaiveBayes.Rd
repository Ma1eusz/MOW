\name{notSoNaiveBayes}
\alias{notSoNaiveBayes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Not so naive  Bayesian network classifier
}
\description{
This package implements tree-augmented naive Bayesian network (TAN) classifier.
}
\usage{

## Default S3 method 
naiveBayes(x,y,...)

## S3 method for class 'notSoNaiveBayes'
predict(object, newdata)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A numeric matrix, or a data frame of categorical variables.}
  \item{y}{Class vector}
  \item{object}{An object of class "notSoNaiveBayes"}
  \item{newdata}{A dataframe with new predictors (with possibly fewer columns than the training data). Note that the column names of newdata are matched against the training data ones.}
  \item{...}{Currently not used}


}
}
\details{
Implementation tree-augmented naive Bayesian network (TAN) classifier based on article:


Nir Friedman, Dan Geiger, and Moises Goldszmidt. Bayessian Network Classiers.
Machine Learning, (29):131163, 1997.
}

\references{
\href{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.178.8884&rep=rep1&type=pdf}{Nir Friedman, Dan Geiger, and Moises Goldszmidt. Bayessian Network Classiers.
Machine Learning, (29):131163, 1997.
}
}
\author{
Mateusz Jamiołkowski
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  data(nursery.rad)
  model <- notSoNaiveBayes(nursery.rad[,-9], nursery.rad[,9])
  table(predict(model, nursery.rad[,-9]), nursery.rad[,9])


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~bayes }
\keyword{ ~TAN }
