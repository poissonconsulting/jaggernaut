\name{jmodel}
\alias{jmodel}
\title{Create JAGS model object}
\usage{
  jmodel(model, monitor = NULL, select = NULL,
    modify_data = NULL, gen_inits = NULL, random = NULL,
    derived = NULL, extract_data = NULL,
    description = NULL)
}
\arguments{
  \item{model}{a character element defining the model in
  the JAGS dialect of the BUGS language}

  \item{monitor}{a character vector of the parameters to
  monitor}

  \item{select}{a character vector of the variables to
  select from the data set being analysed}

  \item{modify_data}{a function to modify the data set
  being analysed (after it has been converted to list
  form)}

  \item{gen_inits}{a function to generate initial values
  for an MCMC chain (it is passed the modified data in list
  form)}

  \item{random}{a named list of parameters representing
  random variables}

  \item{derived}{a character element defining a model in
  the JAGS dialect of the BUGS language that defines
  derived parameters}

  \item{extract_data}{a function to convert the parameter
  estimates into a data set (after they have been converted
  to list form)}

  \item{description}{a named character vector descriping
  each of the parameters in the model where the name
  indicates the parameter to which the description applies}
}
\value{
  a JAGS model object
}
\description{
  Creates a JAGS model object
}
\examples{
model <- jmodel("
 model {
   bLambda ~ dlnorm(0,10^-2)
   for (i in 1:nrow) {
     x[i]~dpois(bLambda)
   }
 }"
)
}

