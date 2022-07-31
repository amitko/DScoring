\name{DS.logitDeltaPlot}
\alias{DS.logitDeltaPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot ICC
}
\description{
Plots ICC for the specified items, fitted by dS.logitDeltaFit(), uder the model (1RFM, 2RFM, 3RFM) defined in options.
}
\usage{
dS.logitDeltaPlot(Fit,items , o)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Fit}{
  Fitted models, returned from dS.logitDeltaFit()
}
  \item{items}{
  Items to be plotted. Default value is 1:nrow(Fit$parameters)
}
  \item{o}{
  deltaScoring options, default: dS.options()
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Dimitrov, Dimiter. (2019). Modeling of Item Response Functions Under the D -Scoring Method. Educational and Psychological Measurement. 80. 001316441985417. 10.1177/0013164419854176. 
}
\author{
Dimitar Atanasov, 2022

\email{datanasov@nbu.bg}
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
