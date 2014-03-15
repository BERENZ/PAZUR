// author: Pawel Kliber PUE
// p.kliber@ue.poznan.pl
#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
using namespace std;

//Resample from transaction data

// [[Rcpp::export]]
DataFrame resampleRcpp(DataFrame dd, int dt, int set, int day)  {
	int start, end, trans_N, m;
  if (set==1) {
    start=32400;
    end=58200;
  if (day>503)
    end=62400;
  }
 if (set==2) {
    start=30600;
    end=58200;
    if(day>503)
	    end=62400;
 }
 if (set==3) {
   start=28800;
    end=68400;
 }
 IntegerVector tt=dd["t"];
 NumericVector pp=dd["p"];
 trans_N=pp.size();
 m = 1+floor((end-start)/dt);
 
 NumericVector pout(m);
 NumericVector tout(m);
 int t, trans, curr;
 double p;
 
 t = start;
 trans = 0;
 curr = 0;

 p=pout[0]=pp[0];
 tout[0]=t;
 curr++;
 t+=dt;

	while(t<=end) {
		while(trans<trans_N && tt[trans]<=t)
			p = pp[trans++];
    tout[curr] = t;
		pout[curr++] = p;
		t += dt;
	}
  return DataFrame::create(_["t"]= tout, _["p"]= pout);
}
