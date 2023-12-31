% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HelperFunctions.R
\name{PVfactory}
\alias{PVfactory}
\title{PVfactory (R6Class for present values with arbitrary dimensions)}
\description{
PVfactory (R6Class for present values with arbitrary dimensions)

PVfactory (R6Class for present values with arbitrary dimensions)
}
\details{
provides functions to calculate present values for survival, death, dread
disease, invalidity and guaranteed benefits in various dimensions
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PVfactory-new}{\code{PVfactory$new()}}
\item \href{#method-PVfactory-guaranteed}{\code{PVfactory$guaranteed()}}
\item \href{#method-PVfactory-survival}{\code{PVfactory$survival()}}
\item \href{#method-PVfactory-death}{\code{PVfactory$death()}}
\item \href{#method-PVfactory-disease}{\code{PVfactory$disease()}}
\item \href{#method-PVfactory-afterDeath}{\code{PVfactory$afterDeath()}}
\item \href{#method-PVfactory-clone}{\code{PVfactory$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PVfactory-new"></a>}}
\if{latex}{\out{\hypertarget{method-PVfactory-new}{}}}
\subsection{Method \code{new()}}{
Initialize the present value factory with defaults for qx, interest and multiple payments per year
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PVfactory$new(qx, m = 1, mCorrection = list(alpha = 1, beta = 0), v = 1)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{qx}}{the vector of mortality / death probabilities}

\item{\code{m}}{the number of yearly payments}

\item{\code{mCorrection}}{the list of alpha/beta to correct present values for multiple payments per year}

\item{\code{v}}{the discount factor (1 / (1+i))}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PVfactory-guaranteed"></a>}}
\if{latex}{\out{\hypertarget{method-PVfactory-guaranteed}{}}}
\subsection{Method \code{guaranteed()}}{
Present values of guaranteed benefits (paid in advance or arrears, possible multiple times per year)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PVfactory$guaranteed(
  advance = NULL,
  arrears = NULL,
  start = 0,
  ...,
  m = private$m,
  mCorrection = private$mCorrection,
  v = private$v
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{advance}}{the payment cashflows in advance}

\item{\code{arrears}}{the payment cashflows in arrears}

\item{\code{start}}{the time index, where the PV calculation should be based}

\item{\code{...}}{other parameters (currently not used, for future use)}

\item{\code{m}}{the number of yearly payments}

\item{\code{mCorrection}}{the list of alpha/beta to correct present values for multiple payments per year}

\item{\code{v}}{the discount factor (1 / (1+i))}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PVfactory-survival"></a>}}
\if{latex}{\out{\hypertarget{method-PVfactory-survival}{}}}
\subsection{Method \code{survival()}}{
Present values of survival benefits (paid in advance or arrears, possible multiple times per year)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PVfactory$survival(
  advance = NULL,
  arrears = NULL,
  start = 0,
  ...,
  m = private$m,
  mCorrection = private$mCorrection,
  v = private$v
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{advance}}{the payment cashflows in advance}

\item{\code{arrears}}{the payment cashflows in arrears}

\item{\code{start}}{the time index, where the PV calculation should be based}

\item{\code{...}}{other parameters (currently not used, for future use)}

\item{\code{m}}{the number of yearly payments}

\item{\code{mCorrection}}{the list of alpha/beta to correct present values for multiple payments per year}

\item{\code{v}}{the discount factor (1 / (1+i))}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PVfactory-death"></a>}}
\if{latex}{\out{\hypertarget{method-PVfactory-death}{}}}
\subsection{Method \code{death()}}{
Present values of death benefits
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PVfactory$death(benefits, start = 0, ..., v = private$v)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{benefits}}{the vector of death / invalidity / disease benefits}

\item{\code{start}}{the time index, where the PV calculation should be based}

\item{\code{...}}{other parameters (currently not used, for future use)}

\item{\code{v}}{the discount factor (1 / (1+i))}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PVfactory-disease"></a>}}
\if{latex}{\out{\hypertarget{method-PVfactory-disease}{}}}
\subsection{Method \code{disease()}}{
Present values of disease benefits
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PVfactory$disease(benefits, start = 0, ..., v = private$v)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{benefits}}{the vector of death / invalidity / disease benefits}

\item{\code{start}}{the time index, where the PV calculation should be based}

\item{\code{...}}{other parameters (currently not used, for future use)}

\item{\code{v}}{the discount factor (1 / (1+i))}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PVfactory-afterDeath"></a>}}
\if{latex}{\out{\hypertarget{method-PVfactory-afterDeath}{}}}
\subsection{Method \code{afterDeath()}}{
Present values of guaranteed benefits after death (paid in advance or arrears, possible multiple times per year)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PVfactory$afterDeath(
  advance = NULL,
  arrears = NULL,
  start = 0,
  ...,
  m = private$m,
  mCorrection = private$mCorrection,
  v = private$v
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{advance}}{the payment cashflows in advance}

\item{\code{arrears}}{the payment cashflows in arrears}

\item{\code{start}}{the time index, where the PV calculation should be based}

\item{\code{...}}{other parameters (currently not used, for future use)}

\item{\code{m}}{the number of yearly payments}

\item{\code{mCorrection}}{the list of alpha/beta to correct present values for multiple payments per year}

\item{\code{v}}{the discount factor (1 / (1+i))}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PVfactory-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PVfactory-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PVfactory$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
