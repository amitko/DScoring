\name{DS.personDscore}
\alias{DS.personDscore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns the so called D-Score for a person
 with a given response vector over a set
 of items with precalculated deltas;
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dS.personDscore(itemData, deltas, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{itemData}{
 0/1 item response
}
  \item{deltas}{
Item delta values
}
  \item{type}{
"relative_to_d" - default value

"relative_to_n"

"total"
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
