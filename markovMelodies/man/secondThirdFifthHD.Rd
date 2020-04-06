\name{secondThirdFifthHD}
\alias{secondThirdFifthHD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transition Matrix With Priority on 0, 2, 3 and 5 Intervals (Both Up and Down)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
secondThirdFifthHD(pSec,pThird,pFifth,pStay)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pSec}{Weight for 2 intervals.
%%     ~~Describe \code{x} here~~
}
\item{pThird}{Weight for 3 intervals.}
}
\item{pFifth}{Weight for 5 intervals.}
\item{pStay}{Weight for not changing notes.}
\details{
Adds uniform low probability to all other interval values, it is not necessary that arguments are smaller than 1.
}
\value{
A 22x22 square transition matrix.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
