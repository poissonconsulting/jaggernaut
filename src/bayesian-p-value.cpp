#include <Rcpp.h>
using namespace Rcpp;

//' @title Bayesian p-value
//'
//' @description
//' Calculates Bayesian equivalent of frequentist p-value
//' from vector of posterior samples.
//'
//' @param x numeric vector of posterior samples
//' @return Bayesian equivalent of p-value.
//' @useDynLib jaggernaut
// [[Rcpp::export]]
double bayesian_p_value (NumericVector x) {
  int n = x.size();
  double p = 0;
  
  for (int i = 0; i < n; ++i) {
    if(x[i] >= 0) {
      ++p;
    }
  }
  p /= n;
  if(p > 0.5) {
   p = 1 - p;
  }
  p *= 2;
  return (p); // just need to round to 4 dps...
}

/*** R
      p<-function (x) {
      x<-sum(as.integer(x>=0))/length(x)
      x<-round(x,4)
      return (min(x,1-x)*2)
    }

x <- rnorm(1000, 0.01, 0.01)
print(p(x))
print(bayesian_p_value(x))

microbenchmark(
  p(x),
  bayesian_p_value(x)
)
*/
