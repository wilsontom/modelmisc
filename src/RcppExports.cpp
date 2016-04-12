// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// nCm_ratio
double nCm_ratio(double n1, double m1, double n2, double m2);
RcppExport SEXP forestTools_nCm_ratio(SEXP n1SEXP, SEXP m1SEXP, SEXP n2SEXP, SEXP m2SEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< double >::type n1(n1SEXP);
    Rcpp::traits::input_parameter< double >::type m1(m1SEXP);
    Rcpp::traits::input_parameter< double >::type n2(n2SEXP);
    Rcpp::traits::input_parameter< double >::type m2(m2SEXP);
    __result = Rcpp::wrap(nCm_ratio(n1, m1, n2, m2));
    return __result;
END_RCPP
}
// timesTwo
double timesTwo(int x);
RcppExport SEXP forestTools_timesTwo(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    __result = Rcpp::wrap(timesTwo(x));
    return __result;
END_RCPP
}
