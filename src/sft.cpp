#include <Rcpp.h>
#include "nCm_ratio.h"

using namespace Rcpp;

// [[Rcpp::export]]
double sft(double F, double Fn, double T, double K){

  double p_Ct = nCm_ratio(F - 1, Fn -1, F, Fn);
  double bidis = Rcpp::dbinom(1.0,1.0,1.0);


  return bidis;

  }


