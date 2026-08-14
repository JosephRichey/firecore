\name{WriteFirefighterCookie}
\alias{WriteFirefighterCookie}
\title{Write or refresh a signed browser cookie}
\usage{WriteFirefighterCookie(session, firefighter_id, config)}
\arguments{
\item{session}{The active Shiny session.}
\item{firefighter_id}{The stable firefighter ID to store in the token.}
\item{config}{Settings returned by \code{NewAuthCookieConfig()}.}
}
\description{Write or refresh a signed browser cookie.}
