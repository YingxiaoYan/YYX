% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/change_name.R
\name{change_name}
\alias{change_name}
\title{Do not use $ or \ [ ]to mess things up

[ ]: R:\%20}
\usage{
change_name(
  X,
  oldname_format,
  newname_format,
  remove_rest = F,
  add_head = F,
  add_tail = F,
  capital = "non",
  only_initial = F
)
}
\arguments{
\item{X}{a vector of names}

\item{oldname_format}{a vector,what you want to change.}

\item{newname_format}{a vector, what you want to change to}

\item{remove_rest}{T or F remove the part that is not substituted}

\item{add_head}{logical, if you want to add something infromt of your new name format}

\item{add_tail}{logical, if you want to add something behind of your new name format}

\item{capital}{T or F if every thing should be captial or not}

\item{only_initial}{T or F if the captial only operate on the initial letter}
}
\value{
X_new new vector of names
}
\description{
Do not use $ or \ [ ]to mess things up

[ ]: R:\%20
}
