#' portfolioMTM 
#'
#' @description computes the mark-to-market value of the portfolio
#' @param x portfolio input as tibble
#' @param shockpar parallel ytm shock applied to portfolio
#' @param shockInd individual shocks to ytm
#' @param output mtm of potfolio or individual bonds
#' @return portfolio mark-to-market
#' @export 
#' @author Philippe Cote


portfolioMTM <- function(x, shockpar = 0, shockInd = FALSE, output = "p") {
  if (output == "b1") {
    b1 <- x$Notional[1] / 100 * RTL::bond(ytm = x$YTM[1] + ifelse(shockInd,x$Shock[1]/10000,0) + shockpar, C = x$Coupon[1],T2M = x$Maturity[1],m = 2,output = "price")
    return(b1)
  }
  if (output == "b2") {
    b2 <- x$Notional[2] / 100 * RTL::bond(ytm = x$YTM[2] + ifelse(shockInd,x$Shock[2]/10000,0) + shockpar, C = x$Coupon[2],T2M = x$Maturity[2],m = 2,output = "price")
    return(b2)
  }
  if (output == "b3") {
    b3 <- x$Notional[3] / 100 * RTL::bond(ytm = x$YTM[3] + ifelse(shockInd,x$Shock[3]/10000,0) + shockpar, C = x$Coupon[3],T2M = x$Maturity[3],m = 2,output = "price")
    return(b3)
  }
  if (output == "p") {
    b1 <- x$Notional[1] / 100 * RTL::bond(ytm = x$YTM[1] + ifelse(shockInd,x$Shock[1]/10000,0) + shockpar, C = x$Coupon[1],T2M = x$Maturity[1],m = 2,output = "price")
    b2 <- x$Notional[2] / 100 * RTL::bond(ytm = x$YTM[2] + ifelse(shockInd,x$Shock[2]/10000,0) + shockpar, C = x$Coupon[2],T2M = x$Maturity[2],m = 2,output = "price")
    b3 <- x$Notional[3] / 100 * RTL::bond(ytm = x$YTM[3] + ifelse(shockInd,x$Shock[3]/10000,0) + shockpar, C = x$Coupon[3],T2M = x$Maturity[3],m = 2,output = "price")
    p = b1 + b2 + b3
    return(p)
  }
}
