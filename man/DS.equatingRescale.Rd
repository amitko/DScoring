\name{DS.equatingRescale}
\alias{DS.equatingRescale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Rescales the item deltas of a test according
 to rescaling constants A and B.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dS.equatingRescale(deltas, constants)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{deltas}{
 new test item deltas
}
  \item{constants}{
  List of constants A and B. Returned by dS.equatingConstants
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
numeric

rescaled deltas
}
\references{
Dimitrov, Dimiter. (2017). The Delta-Scoring Method of Tests With Binary Items: A Note on True Score Estimation and Equating. Educational and Psychological Measurement. 78. 001316441772418. 10.1177/0013164417724187. 

Dimitrov, Dimiter. (2016). An Approach to Scoring and Equating Tests With Binary Items: Piloting With Large-Scale Assessments. Educational and Psychological Measurement. 76. 10.1177/0013164416631100. 
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
