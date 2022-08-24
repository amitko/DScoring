\name{Ds.observedLogitDelta}
\alias{Ds.observedLogitDelta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the observed proportions of correct item performance for different vaues on the Dscore ability scale.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ds.observedLogitDelta(itemData, Dscore, o = dS.options())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{itemData}{dichotomous item response 0/1}
  \item{Dscore}{  Persons Dscore Vlues   }
  \item{o}{  Delta scoring options list. Default value is dS.options  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
data.frame

rows represents Dscore values

columns represents items.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
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
