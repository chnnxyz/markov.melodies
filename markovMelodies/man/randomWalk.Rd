\name{randomWalk}
\alias{randomWalk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random Walk Matrix Generation
}
\description{
Generates a random walk matrix for use in markovMelodies.
}
\usage{
randomWalk(pUp,pDn,PSt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pUp}{
  Probability of going a step up.
}
  \item{pDn}{
  Probability of going a step down.
}
  \item{pSt}{
  Probability of staying on the same step.
}
}
\details{
Probabilities will be normalized by the function, so it is not necessary that pUp+pDn+pSt=1.
}
\value{
Square 22x22 float matrix.
}
\references{
https://github.com/chnnxyz/markov.melodies
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{randomMatrix}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
