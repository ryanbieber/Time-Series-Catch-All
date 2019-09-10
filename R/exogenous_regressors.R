#' building our exogenous regressors for our arimax and neural net models term, moq, quarter
#'
#' @param ts_data xts matrix with variable Date in first column
#'
#' @return a matrix of xregs based on h
#' @export
#'
#' @examples xreg <- exogenous_regressors(ts_data) makes term variables, quarter, and MoQ variables
exogenous_regressors <- function(data){
  ts_data <- xts(data, order.by = data$Date)
  ts_data<- t(ts_data)
  ts_data<- tslist(ts_data)
  frame <- as.character(ts_data[[1]])
  quarter <- rep(1:4, times=ceiling(length(frame)/12), each=3)
  quarter <- quarter[1:length(frame)]
  term1 <- rep(12, each = 18)
  term2 <- rep(1, each = 9)
  x <- NROW(frame)-27
  term3 <- rep(3, each = x)
  term <- c(term1, term2, term3)
  rows <- ceiling(NROW(ts_data[[1]])/3)
  moq <- rep(seq(1,3,1), times = rows)
  moq <- moq[1:length(ts_data[[1]])]
  frame <- cbind(as.factor(term), as.factor(quarter), as.factor(moq))
  return(frame)
}
