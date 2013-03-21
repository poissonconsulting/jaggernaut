\name{jmodel}
\alias{jmodel}
\title{Create a jmodel (JAGS model) object}
\usage{
  jmodel(model, monitor = NULL, select = NULL,
    modify_data = NULL, gen_inits = NULL, random = NULL,
    derived_model = NULL, extract_data = NULL,
    description = NULL)
}
\arguments{
  \item{model}{a character element defining the model in
  the JAGS dialect of the BUGS language}

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
  a jmodel (JAGS models) object
}
\description{
  Creates a JAGS model (jmodel) object which defines a
  Bayesian model in the JAGS dialect of the BUGS language.
  In addition to defining the model a JAGS model object can
  also specify the parameters to monitor, the variables to
  select, a function to manipulate the input data, a
  function to generate all (or some) of the initial values,
  a named list of the random parameters and the variables
  they are random with respect to and some code definition
  in the JAGS dialect to extract derived values from the
  final model among other things.
}
\details{
  The jmodel function defines a JAGS model that can then be
  passed to the janalysis function together with a data
  frame to perform a Bayesian analysis. The idea is that a
  JAGS model can be defined once and then used to perform
  analyses on different data frames. To facilitate use the
  only argument that needs to be defined is a character
  element defining the model in the JAGS dialect of the the
  BUGS language. However various other arguments can also
  be set to provide to provide additional control.

  In particular the monitor argument can be used to define
  the parameters in the model to monitor - by default all
  model parameters are monitored except those that begin
  with a d, e or i and are followed by an upper case
  character, i.e., dIntercept would not be monitored while
  bIntercept and dintercept would.

  Unless select is null (the default) all variables in the
  data frame are passed to the analysis.  If select is
  defined then only those variables named in select are
  passed to the analysis. As a warning is given if a
  variable named in select is not in the data frame this
  can be useful for ensuring all the required variables are
  present in the data frame.  In addition if a variable
  name in select is followed by a * then the variables is
  centred (mean of zero and a standard deviation of one)
  before it is passed to the analysis i.e.,
  select=c("Weight","Length*") would result in Length being
  centred in the analysis while select=c("Weight","Length")
  would not. A transformation can also be applied to a
  variable - for example the argument
  select=c("Weight","log(Length)*") would result in Length
  being logged (and renamed LogLength) and then centred.

  Once the select argument has been applied to the data the
  factors are converted into integers and the data frame is
  converted into list form for input into JAGS. As well as
  each variable the list also contains a named element for
  each factor that gives the number of levels of the
  factor. For example if the factor Type with three levels
  is present in the input data then an addition element
  would be created with the name nType and value three.
  This is useful for iterating over factor levels in the
  model. To facilitate iterating over all the data the list
  also contains an element named nrow that defines the
  number of rows in the input data.

  In some cases additional manipulations of the data may be
  required such as conversion of variables into matrix or
  array form based on input factor levels. This can be
  achieved by defining a function for the modify_data
  argument. The function will be passed the data in list
  form and should return a list of the modified data.

  At this point initial values can be generated for one or
  more of the model parameters using the gen_inits argument
  which expects a function that takes the list of data and
  returns a list of the initial values - the function is
  called once for each chain.

  The remaining arguments are used after the analysis. To
  be continued...
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

