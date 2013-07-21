% % \encoding{UTF-8}
% \name{get.incidence}

% \alias{get.incidence}
% \alias{get.incidence,obkData-method}
% \alias{get.incidence.obkData}

% \title{Derive incidence from "obkData" objects}
% \description{
%   \code{get.incidence} is a generic function with a method for
%   \linkS4class{obkData} objects. It can be used to extract incidence
%   curves based on sample data.
% }
% \usage{
% get.incidence(x, \dots)

% \S4method{get.incidence}{obkData}(x, use=c("all", "samples", "individuals",
%        "records"), \dots)
% }
% \arguments{
%   \item{x}{an \linkS4class{obkData} object.}
%   \item{use}{a character string indicating the ...}
%   \item{\dots}{currently not used.}
% }
% \author{
%   Thibaut Jombart \email{t.jombart@imperial.ac.uk}.
% }
% \examples{

% ## LOAD DATA ##
% data(ToyOutbreak)

% ## VARIOUS USE OF GET.INCIDENCE ##
% get.incidence(ToyOutbreak)


% }
% \keyword{classes}
