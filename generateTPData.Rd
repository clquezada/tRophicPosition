\name{generateTPData}
\alias{generateTPData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
generateTPData(n.baselines = 1, n.obsB = 25, n.obsSC = 25, dNb1 = NULL, std.devB = 0.1, dNsc = NULL, std.devSC = 0.1, n.obsDeltaN = 20, deltaN = 3.4, std.devDeltaN = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n.baselines}{
%%     ~~Describe \code{n.baselines} here~~
}
  \item{n.obsB}{
%%     ~~Describe \code{n.obsB} here~~
}
  \item{n.obsSC}{
%%     ~~Describe \code{n.obsSC} here~~
}
  \item{dNb1}{
%%     ~~Describe \code{dNb1} here~~
}
  \item{std.devB}{
%%     ~~Describe \code{std.devB} here~~
}
  \item{dNsc}{
%%     ~~Describe \code{dNsc} here~~
}
  \item{std.devSC}{
%%     ~~Describe \code{std.devSC} here~~
}
  \item{n.obsDeltaN}{
%%     ~~Describe \code{n.obsDeltaN} here~~
}
  \item{deltaN}{
%%     ~~Describe \code{deltaN} here~~
}
  \item{std.devDeltaN}{
%%     ~~Describe \code{std.devDeltaN} here~~
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
%% ~put references to the literature/web site here ~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (n.baselines = 1, n.obsB = 25, n.obsSC = 25, dNb1 = NULL, 
    std.devB = 0.1, dNsc = NULL, std.devSC = 0.1, n.obsDeltaN = 20, 
    deltaN = 3.4, std.devDeltaN = 0.1) 
{
    if (is.null(dNb1)) {
        dNb1 <- rnorm(n.obsB, runif(1, -5, 5), std.devB)
    }
    else {
        dNb1 <- rnorm(n.obsB, dNb1, std.devB)
    }
    if (is.null(dNsc)) {
        dNsc <- rnorm(n.obsSC, dNb1 + (2 * deltaN), std.devSC)
    }
    else {
        dNsc <- rnorm(n.obsSC, dNsc, std.devSC)
    }
    deltaN <- rnorm(n.obsDeltaN, deltaN, std.devDeltaN)
    if (n.baselines == 2) {
        return(list(dNb1 = dNb1, dNsc = dNsc, deltaN = deltaN))
    }
    else {
        return(list(dNb1 = dNb1, dNsc = dNsc, deltaN = deltaN))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
