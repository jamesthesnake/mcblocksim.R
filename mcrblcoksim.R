t1 <- Sys.time()

# Load required packages ------------------------------------------
library(quantmod)

# Build the function ----------------------------------------------
mcblocksim <- function(R, l){
  
  # Read price data and build xts object
  data <- read.csv("yourdirectory/simSample.csv", header = TRUE, stringsAsFactors=F)
  s1.dates <- as.POSIXct(data[,2], format="%d-%m-%y") #beware the formatting may need adjusting for your excel settings
  s1 <- xts(data[,3], s1.dates)
  
  # Declare and assign values to other variables
  n <- length(s1) #length of backtest results
  #l <- 5 #block length for sampling (user-defined)
  n.sim <- n
  #R <- 5000 #number of samples required (user-defined)
  endpt <- n - l + 1
  nn <- ceiling(n.sim/l) #number of blocks
  lens <- c(rep(l, nn - 1), 1 + (n.sim - 1)%%l) #lengths of each block
  st <- matrix(sample.int(endpt, nn * R, replace = TRUE), R) #start points of each block
  i.a <- list(starts = st, lengths = lens) #list of start points and lengths of each block
  
  # Get sample indices - store in "inds" matrix
  block <- NULL
  indsim <- NULL
  inds <- NULL
  for(r in 1:nrow(i.a$starts)){
    for(c in 1:ncol(i.a$starts)){
      block <- seq.int(i.a$starts[r, c], i.a$starts[r, c] + i.a$lengths[c] - 1, length.out = i.a$lengths[c])
      indsim <- append(indsim, block)
    }
    inds <- cbind(inds, indsim)
    indsim <- NULL
  }
  
  # Declare and assign values to other variables
  k <- NULL
  tsbootARR <- NULL
  tsbootxts <- NULL
  tmp <- NULL
  ret <- data.frame(ROC(s1[,1]))
  ret[is.na(ret)] <- 0
  
  # Subset ret dataframe with inds and build "tsbootARR" using apply with cumsum()
  for(k in 1:ncol(inds)){
    tmp <- cbind(tmp, ret[inds[,k],])
  }
  tsbootARR <- apply(tmp, 2, function(x) cumsum(x))
  which(is.na(tsbootARR)) #double-check no NAs in ret as a result ROC() call above
  
  # Build xts object for use in plot.zoo
  tsbootxts <- xts(tsbootARR, s1.dates)
  dev.new()
  p <- plot.zoo(tsbootxts, main=paste(R,"sims of simSample", sep = " "), plot.type = "single", col = "lightgray", ylab = "Cumulative Return Distributions")
  p <- lines(cumsum(ROC(s1)[-1,]), col = "red")
  
  p
  
  return(p)
}
# End of function -------------------------------------------------

# Run function ----------------------------------------------------
mcblocksim(1000, 5)

# Record and print time to run script -----------------------------
t2 <- Sys.time()
difftime(t2,t1)

