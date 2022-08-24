\name{DS.trueScore}
\alias{DS.trueScore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the true-score measure for person's D-score
on a set of items with delta scores in itemDeltas,
logistic parameters of the items and the person's D-score
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dS.trueScore(deltas, parameters, Dscore, o )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{deltas}{
  Item delta values
  }
  \item{parameters}{
  logistic item parameters, according RFM model
}
  \item{Dscore}{
  Dscore values
}
  \item{o}{
  Delta scoring options list. Default value is dS.options
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
list
\item{trueScore}{ values}
\item{SE}{standard errors of true score}
}
\references{
Dimitrov, Dimiter. (2017). The Delta-Scoring Method of Tests With Binary Items: A Note on True Score Estimation and Equating. Educational and Psychological Measurement. 78. 001316441772418. 10.1177/0013164417724187. 
}
\author{
Dimitar Atanasov, 2022

\email{datanasov@nbu.bg}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
