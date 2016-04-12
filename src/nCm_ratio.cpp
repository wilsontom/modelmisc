#include <Rcpp.h>
#include <nCm_ratio.h>
using namespace Rcpp;


// [[Rcpp::export]]
double nCm_ratio (double n1, double m1, double n2, double m2){

  if (m1 > n1){
    return 0;
  }
  if (m2 > n2){
    return 0;
  }

  NumericVector n1a (n1);
  for (int i = 0; i < n1; ++i){
    n1a[i] = i + 1;
  }
  NumericVector n1b = log (n1a);
  double RN1 = 0;
  for (int i = 0; i < n1; ++i){
    RN1 += n1b[i];
  }

  NumericVector m1a (m1);
  for (int i = 0; i < m1; ++i){
    m1a[i] = i + 1;
  }
  NumericVector m1b = log (m1a);
  double RM1 = 0;
  for (int i = 0; i < m1; ++i){
    RM1 += m1b[i];
  }

  NumericVector nma (n1 - m1);
  for (int i = 0; i < n1 - m1; ++i){
    nma[i] = i + 1;
  }

  NumericVector nmb = log (nma);
  double RNM1 = 0;
  for (int i = 0; i < n1 - m1; ++i){
    RNM1 += nmb[i];
  }

  NumericVector n2a (n2);
  for (int i = 0; i < n2; ++i){
    n2a[i] = i + 1;
  }

  NumericVector n2b = log (n2a);
  double RN2 = 0;
  for (int i = 0; i < n2; ++i){
    RN2 += n2b[i];
  }

  NumericVector m2a (m2);
  for (int i = 0; i < m2; ++i){
    m2a[i] = i + 1;
  }

  NumericVector m2b = log (m2a);
  double RM2 = 0;
  for (int i = 0; i < m2; ++i){
    RM2 += m2b[i];
  }

  NumericVector nma2 (n2 - m2);
  for (int i = 0; i < n2 - m2; ++i){
    nma2[i] = i + 1;
  }

  NumericVector nmb2 = log (nma2);
  double RNM2 = 0;
  for (int i = 0; i < n2 - m2; ++i){
    RNM2 += nmb2[i];
  }

  return exp (RN1 - RM1 - RNM1 - RN2 + RM2 + RNM2);

  }

