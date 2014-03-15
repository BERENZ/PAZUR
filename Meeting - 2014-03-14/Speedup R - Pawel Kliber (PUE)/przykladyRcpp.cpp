#include <RcppArmadillo.h>
#include <Rcpp.h>

// [[Rcpp::depends("RcppArmadillo")]]
using namespace Rcpp;

// [[Rcpp::export]]
int dodaj(int x, int y) {
  int s;
  s=x+y;
  return(s);
}

// [[Rcpp::export]]
void add10(NumericVector v) {
  int i, n;
  n = v.size();   
  for(i=0; i<n; i++)
      v[i]+=10;
}

// [[Rcpp::export]]
NumericVector add10a(NumericVector v) {
  int i, n;
  n = v.size();   
  for(i=0; i<n; i++)
      v[i]+=10;
  return(v);
}

// [[Rcpp::export]]
NumericVector add10b(NumericVector v) {
  int i, n;
  n = v.size();
  NumericVector out(n);
  for(i=0; i<n; i++)
      out[i] = v[i]+10;
  return(out);
}

// [[Rcpp::export]]
NumericVector uniqueCpp(NumericVector v) {
  v = unique(v);
  return(v);
}


// [[Rcpp::export]]
double sumCpp(NumericVector x) {
  double s = 0;
  for(NumericVector::iterator it = x.begin(); it != x.end(); it++) {
    s += *it;
  }
  return s;
}


using namespace arma;


// [[Rcpp::export]]
mat rmnorm(int m, rowvec mu, mat Sigma) {
  int n = Sigma.n_cols;
  mat D = chol(Sigma);
  mat X = randn(m, n);
  mat Y = repmat(mu, m, 1) + X * D;
  return Y;
}


