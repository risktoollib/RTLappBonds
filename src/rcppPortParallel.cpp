#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

//' Bond sensitivities
//' 
//' @param x bond details as matrix
//' @param stepSize sensitivities stepsize
//' @return matrix of sensitivities
//' @export
// [[Rcpp::export("rcppPortParallel")]]
NumericVector rcppPortParallel(NumericMatrix x, double stepSize) {

  for (int i = 0; i < x.nrow(); i++) {
    // pricing and duration
    double m = 2;
    double periods = (x(i,1) * m) + 1;
    double ytm = x(i,3) + x(i,5); // same as coupons for CMTs
    double coupon = x(i,2);

    NumericVector tyears(periods);
    for (int j = 1; j < periods; j++) {tyears[j] = tyears[j-1] + 1/m;}
    NumericVector cf = Rcpp::rep(coupon * 100 / m, periods);
    cf[0] = 0;
    cf[periods-1] = 100 + cf[periods-1];
    NumericVector disc_factor(periods);
    for (int j = 0; j < periods; j++) {
      disc_factor[j] = 1 / pow(1 + ytm / m, tyears[j] * m);
      }
    double pv = 0;
    for (int j = 0; j < periods; j++) {pv += cf[j] * disc_factor[j];}
    double duration = 0;
    for (int j = 0; j < periods; j++) {duration += cf[j] * disc_factor[j] * tyears[j]/ pv;}
    x(i,7) = pv;
    x(i,6) = duration;

    // delta/gamma (expo)
      // down shift
    double ytmDown = ytm - stepSize;
    for (int j = 0; j < periods; j++) {
      disc_factor[j] = 1 / pow(1 + ytmDown / m, tyears[j] * m);
    }
    double pv01Down = 0;
    for (int j = 0; j < periods; j++) {pv01Down += cf[j] * disc_factor[j];}
    x(i,8) = pv01Down;

      // up shift
    double ytmUp = ytm + stepSize;
    for (int j = 0; j < periods; j++) {
      disc_factor[j] = 1 / pow(1 + ytmUp / m, tyears[j] * m);
    }
    double pv01Up = 0;
    for (int j = 0; j < periods; j++) {pv01Up += cf[j] * disc_factor[j];}
    x(i,9) = pv01Up;
    // delta
    x(i,10) = (pv01Up - pv01Down) / 2;
    // gamma
    x(i,11) = 0.5 * ((pv01Up - 2 * pv + pv01Down) / (stepSize * stepSize));
  }

return x;

  //IntegerVector a = Rcpp::as<IntegerVector>(periods);
  //IntegerVector x = Rcpp::seq(0,Rcpp::as<IntegerVector>(periods));
  //NumericVector y = Rcpp::as<NumericVector>(x);

  // for (int i = 0; i < ytm.size(); i++) {
  //   NumericVector tyears = seq(0, T2M[i]);
  //   tyears = tyears * m[i]
  //   NumericVector cf = rep(C[i] * 100 / m[i], T2M[i] * m[i]);
  //   NumericVector t_periods = m[i] * tyears;
  //   cf = ifelse(tyears == T2M[i],C[i] * 100 / m[i] + 100,cf);
  //   NumericVector disc_factor = 1 / pow(1 + ytm[i] / m[i], t_periods);
  //   NumericVector pv = cf * disc_factor;
  //   price[i] = sum(pv);
  // }
}