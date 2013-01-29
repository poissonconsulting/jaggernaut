
#' Perform JAGS analysis
#'
#' Performs JAGS analysis
#' 
#' @param model a jmodel object specifying the JAGS model
#' @param data a data.frame specifying the data to analyse
#' @param n.iter the number of iterations in each mcmc chain
#' @param n.chain the number of mcmc chains
#' @param resample the number of times to resample 
#' until convergence and independence are achieved
#' @param convergence the threshold for convergence
#' @param independence the threshold for indepedence
#' @param parallel a boolean indicating whether the analysis should
#' be run on parallel processors
#' @param debug a boolean indicating whether or not the intent is to debug the model
#' @param quiet a boolean indicating whether or not to suppress messages
#' @return a JAGS analysis object
#' @export
#' @examples
#' model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
janalysis <- function (
  model, data, n.iter = 1000, n.chain = 3, resample = 3,
  convergence = 1.1, independence = 0,
  parallel = FALSE, debug = FALSE, quiet = FALSE
)
{  
  if(!is.jmodel(model))
    stop ("model should be class jmodel")
  
  if(!is.data.frame(data))
    stop ("data should be class data.frame")
  
  if(parallel && .Platform$OS.type != "unix") {
    warning("parallel is not currently defined for windows")
    parallel <- FALSE
  }
  
  stopifnot(n.iter >= 100)
  stopifnot(n.chain %in% 2:4)
  stopifnot(resample %in% 0:3)
  stopifnot(convergence >= 1.0)
  stopifnot(independence %in% 0:100)
    
  cat_convergence <- function (object) {
    convergence <- calc_convergence (object, summarise = T, type = 'all') 
    cat (' (con:')
    cat (convergence['convergence'])
    cat (', ind:')
    cat (convergence['independence'])
    cat (')\n')
  }
    
  if (debug) {
    n.iter <- 200
    n.chain <- 2
    quiet <- FALSE
    resample <- 0
    parallel <- F
  }
    
  if (quiet) {
    options(jags.pb = "none")
  } else {
    options(jags.pb = "text")
  }

  if(!is.null(model$monitor)) {
    model$monitor <- sort(unique(c(model$monitor,"deviance")))
  }
  
  data_analysis <- translate_data(model, data) 
       
  if (is.function(model$gen_inits)) {
    inits <- list()
    for (i in 1:n.chain)   
      inits[[i]] <- model$gen_inits(data_analysis)
  } else
    inits <- NULL
              
  n.adapt <- 100
  n.burnin <- as.integer(n.iter /2)
  n.sim <- as.integer(n.iter /2)
  n.thin <- max(1, floor(n.chain * n.sim / 1000))

  ptm <- proc.time()
    
  if (parallel) {
      
    doMC::registerDoMC(cores=n.chain)
    rngs<-parallel.seeds("base::BaseRNG", n.chain)
    
    if (!is.null (inits)) {
      for (i in 1:n.chain)
        inits[[i]] <- c(inits[[i]],rngs[[i]])
    } else
      inits <- rngs
    
      mcmc <- foreach(i = 1:n.chain, .combine = add_chains_gsmcmc) %dopar% { 
        file <- tempfile(fileext=".bug")
        cat(model$model, file=file)
        
        jags_analysis (
          data = data_analysis, file=file, monitor = model$monitor, 
          inits = inits[i], n.chain = 1, 
          n.adapt = n.adapt, n.burnin = n.burnin, n.sim = n.sim, n.thin = n.thin, 
          quiet = quiet
        )
      }
  } else {    
    file <- tempfile(fileext=".bug")
    cat(model$model, file=file)
    
    mcmc <- jags_analysis (
      data = data_analysis, file=file, monitor = model$monitor, 
      inits = inits, n.chain = n.chain, 
      n.adapt = n.adapt, n.burnin = n.burnin, n.sim = n.sim, n.thin = n.thin, 
      quiet = quiet
    )
  }
  if(is.null(model$monitor)) {
    model$monitor <- names(mcmc$mcmc)
    model$monitor <- sort(model$monitor)
  }
  
  object <- list(
    model = model, 
    data = data, 
    inits = inits, 
    mcmc = mcmc,
    iterations = n.iter,
    time = ((proc.time () - ptm)[3]) / (60 * 60),
    convergence = convergence,
    independence = independence
    )
  
  class(object) <- c("janalysis")
  check_convergence (object)
  
  while (!check_convergence (object) && resample > 0) 
  {
    if(!quiet) {
      cat ("Resampling due to convergence failure")
      cat_convergence (object)  
    }
    
    resample <- resample - 1
        
    object <- update(object)
  }
  
  if (check_convergence (object)) {
    if (!quiet) {
      cat ('Analysis converged')
      cat_convergence (object)
    }
    return (object)
  }
  if (quiet) {
    message ("Analysis failed to converge")
  } else {
    cat ('Analysis failed to converge')
    cat_convergence (object)
  }    
  return (object)
}





