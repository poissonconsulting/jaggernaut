\name{model}
\alias{model}
\title{Create a JAGS model}
\usage{
  model(code, monitor = NULL, select = NULL,
    modify_data = NULL, gen_inits = NULL,
    derived_code = NULL, random = NULL, description = NULL)
}
\arguments{
  \item{code}{a character element defining the model in the
  JAGS dialect of the BUGS language}

  \item{monitor}{a character vector of the parameters to
  monitor}

  \item{select}{a character vector of the variables to
  select from the data set being analysed (can also specify
  variables to transform and/or centre)}

  \item{modify_data}{a function to modify the data set
  being analysed (after it has been converted to list
  form)}

  \item{gen_inits}{a function to generate initial values
  for an MCMC chain (it is passed the (modified) data in
  list form)}

  \item{derived_code}{a character element defining a model
  in the JAGS dialect of the BUGS language that specifies
  derived parameters}

  \item{random}{a named list of parameters to be treated as
  random effects with the related data as values}

  \item{description}{a named character vector descriping
  each of the parameters in the model where the name
  indicates the parameter to which the description applies}
}
\value{
  a \code{jmodel} object
}
\description{
  Creates a JAGS model (\code{jmodel} object) which defines
  a Bayesian model in the JAGS dialect of the BUGS
  language.  In addition to defining the model a JAGS model
  object can also specify the parameters to monitor, the
  variables to select, a function to manipulate the input
  data, a function to generate all (or some) of the initial
  values and JAGS code to extract derived values from the
  final model among other things.
}
\details{
  The \code{model} function defines a JAGS model that can
  then be passed to the \code{\link{analysis}} function
  together with a data frame to perform a Bayesian
  analysis. The idea is that a JAGS model can be defined
  once and then used to perform analyses on different data
  frames. To facilitate use the only argument that needs to
  be provided is a character element defining the model in
  the JAGS dialect of the the BUGS language. However
  various other arguments can also be set to provide
  additional control.

  The \code{monitor} argument is used to define the
  parameters in the model to monitor - by default all model
  parameters are monitored except those that begin with the
  character d, e or i and are followed by an any upper case
  character, i.e., \code{eCount} would not be monitored
  while \code{bIntercept} and \code{ecount} would.

  If \code{select} is \code{NULL} (the default) all
  variables in the data frame are passed to the analysis.
  If select is defined then only those variables named in
  select are passed to the analysis. As a warning is given
  if a variable named in select is not in the data frame
  this can be useful for ensuring all the required
  variables are present in the data frame.

  In addition if a variable name in select is followed by a
  * then the variables is standardised by substracting its
  mean and then dividing by its standard deviation before
  it is passed to the analysis i.e.,
  \code{select=c("Weight", "Length*")} would result in
  \code{Length} being standardised in the analysis while
  \code{select=c("Weight", "Length")} would not. A
  transformation can also be applied to a variable - for
  example the argument \code{select=c("Weight",
  "log(Length)*")} would result in \code{Length} being
  logged (and renamed \code{LogLength}) and then
  standardised.

  Once the \code{selec}t argument has been applied to the
  the data, dates and factors are converted into integers
  and the data frame is converted into list form for input
  into JAGS. As well as each variable the list also
  contains a named element for each factor that gives the
  number of levels of the factor. For example if the factor
  \code{Type} with three levels is present in the input
  data then an addition element would be created in the
  list of data with the name \code{nType} and value three.
  This is useful for iterating over factor levels in the
  JAGS model code. To facilitate iterating over all the
  data the list also contains an element named \code{nrow}
  that defines the number of rows in the input data.

  In some cases additional manipulations of the data may be
  required such as conversion of variables into matrix or
  array form based on input factor levels. This can be
  achieved by defining a function for the
  \code{modify_data} argument. The function will be passed
  the data in list form and should return an updated list
  of the modified data.

  At this point initial values can be generated for one or
  more of the model parameters using the \code{gen_inits}
  argument which expects a function that takes the list of
  data and returns a list of the initial values - the
  function is called once for each chain and should
  therefore use random generation of initial values if
  different starting values are desired. The remaining
  arguments are used after the analysis has completed.

  The \code{derived_code} argument is used to define a
  model in the JAGS dialect of the BUGS language that
  specifies derived parameters. For further information on
  the use of the \code{derived_code} argument see
  \code{\link{derived}}.

  The \code{random} argument is used specify which
  parameters represent random effects. It takes the form of
  a named list where the parameters are the names of the
  list elements and the values are character vectors of the
  variables in the input data frame that the parameters are
  random with respect to. For further information on the
  use of the \code{random} argument see
  \code{\link{derived}}.

  The \code{description} argument is a named character
  vector that can be used to provide a description of
  parameters or variables in the JAGS model code. Currently
  the \code{description} argument has no functionality.
}
\examples{
# Poisson GLM (Kery & Schaub 2011 p.55-59)
mod <- model("
 model {
   alpha ~ dunif(-20, 20)
   beta1 ~ dunif(-10, 10)
   beta2 ~ dunif(-10, 10)
   beta3 ~ dunif(-10, 10)

   for (i in 1:nrow) {
     log(eCount[i]) <- alpha + beta1 * Year[i]
       + beta2 * Year[i]^2 + beta3 * Year[i]^3
     Count[i] ~ dpois(eCount[i])
   }
 }",
select = c("Count","Year*")
)
}
\references{
  Kery M & Schaub M (2011) Bayesian Population Analysis
  using WinBUGS. Academic Press.
  (\url{http://www.vogelwarte.ch/bpa})
}
\seealso{
  \code{\link{analysis}}, \code{\link{derived}}
}

