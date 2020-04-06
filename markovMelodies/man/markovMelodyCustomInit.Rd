\name{markovMelodyCustomInit}
\alias{markovMelodyCustomInit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Markov Chain Melody With Defined Initial Note
}
\description{
Generates a set of notes and durations using Markov Chains. The initial note is selected at random.
}
\usage{
markovMelodyRandomInit(root="c",mode="ionian",sigTop=4,sigBot=4,stopAfterBars=32,noteLengthDist=rep(1,11)/11,transMatrix,startPos,octave)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{root}{The root note for the scale to use, values can be \code{"c"}, \code{"c#"}, \code{"db"}, \code{"d"}, \code{"d#"}, \code{"eb"}, \code{"e"}, \code{"f"}, \code{"f#"}, \code{"gb"}, \code{"g"}, \code{"g#"}, \code{"ab"}, \code{"a"}, \code{"a#"},  \code{"bb"}, \code{"b"}.}
  \item{mode}{The scale mode. Supported modes are \code{"major"}, \code{"minor"},\code{"ionian"}, \code{"dorian"}, \code{"phrygian"}, \code{"lydian"}, \code{"mixolydian"}, \code{"aeolian"}, \code{"locrian"}.}
  \item{sigTop}{Top number of time signature.}
  \item{sigBot}{Bottom number of time signature.}
  \item{stopAfterBars}{Number of bars to be generated.}
  \item{noteLengthDist}{Note length duration distribution. The vector is defined by \code{c(1,3/4,1/2,3/8,1/4,3/16,1/8,3/32,1/16,3/64,1/64)}. Accepts any vector of length 11.}
  \item{transMatrix}{A 22x22 transition matrix. (i.e. sum of each row is equal to 1)}
  \item{startPos}{Starting note, accepts the same values as \code{root}.}
  \item{octave}{Starting octave, accepts \code{"4"}, \code{"5"} and \code{"6"}}
}
}
\details{
Currently it does not support modulation or taking notes from other points of the circle. \code{"major"} and \code{"ionian"} are equivalent, so are \code{"minor"} and \code{"aeolian"}.
}
\value{
Returns a list of all parameters and a data frame object with the state ID, MIDI pitch value and duration. All numbers are ready to implement in a MIDI file as delivered.
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
