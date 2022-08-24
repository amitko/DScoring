\name{DS.PCR}
\alias{DS.PCR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Probability for correct response
based on estimated parameters of the logistic function
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dS.PCR(parameters, Dscore, o)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parameters}{
  logistic parameters, according the corresponding RFM model
}
  \item{Dscore}{
  values on the ability scale
}
  \item{o}{
  Delta scoring options list. Default value is dS.options
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
matrix

rows correspond to the Dscore values

columns rorrespond to the items
}
\references{

Dimitrov, Dimiter. (2019). Modeling of Item Response Functions Under the D -Scoring Method. Educational and Psychological Measurement. 80. 001316441985417. 10.1177/0013164419854176. 
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
