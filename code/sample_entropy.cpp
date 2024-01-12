#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double SampleEntropy(NumericVector data, int m, double r, int N, double sd)
{
  int Cm = 0, Cm1 = 0;
  double err = 0.0, sum = 0.0;
  
  err = sd * r;
  
  for (unsigned int i = 0; i < N - (m + 1) + 1; i++) {
    for (unsigned int j = i + 1; j < N - (m + 1) + 1; j++) {      
      bool eq = true;
      //m - length series
      for (unsigned int k = 0; k < m; k++) {
        if (std::abs(data[i+k] - data[j+k]) > err) {
          eq = false;
          break;
        }
      }
      if (eq) Cm++;
      
      //m+1 - length series
      int k = m;
      if (eq && std::abs(data[i+k] - data[j+k]) <= err)
        Cm1++;
    }
  }
  
  if (Cm > 0 && Cm1 > 0)
    return std::log((double)Cm / (double)Cm1);
  else
    return 0.0; 
  
}


/*** R
ts <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        3, 3, 0, 0, 1, 0, 0, 0, 1, 0, 0, 3, 1, 0, 1, 1, 0, 0, 0, 0, 0, 
        2, 0, 0, 0, 3, 0, 0, 1, 0, 0, 0, 1, 4, 0, 1, 1, 0, 0, 1, 0, 0, 
        0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3, 1, 0, 0, 0, 0, 1, 0, 
        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0)
SampleEntropy(ts, m = 2L, r = 0.2, sd = sd(ts), N = as.integer(length(ts)))
*/