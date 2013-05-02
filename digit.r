library("ggplot2")

# Given a row of pixels (representing a square image of a digit) and the label of the digit, display
display.digit <- function(row_pixels, y = NULL, theta = 0)
{
  l <- length(row_pixels)
  n <- sqrt(l);
  if (l %% n != 0)
    stop("Length of input should be a perfect square.")  
  mat <- matrix(as.double(row_pixels), nrow=n, ncol=n)
  mat <- apply(mat,1,rev)
  title <- "Digit:"
  if(!is.null(y)) title <- paste(title, as.character(y))
  if(theta != 0) title <- paste(title, "rotated", as.character(theta))
  p<-ggfluctuation(as.table(mat), type = "colour") +
    labs(title=title) +
    scale_fill_gradient(low = "white", high = "blue")
  print(p)
  Sys.sleep(0.5)
}

smooth.digit <- function(row_pixels, y, theta = 0, pred = NULL, prob = NULL)
{
  l <- length(row_pixels)
  n <- sqrt(l);
  if (l %% n != 0)
    stop("Length of input should be a perfect square.")  
  mat <- matrix(as.double(row_pixels), nrow=n, ncol=n)
  mat <- apply(mat,1,rev)
  title <- paste("Digit:", as.character(y))
  if(theta != 0) title <- paste(title, "rotated", as.character(theta))
  if(!is.null(pred)) title <- paste("True:", as.character(y), "Predicted:", as.character(pred))
  if(!is.null(prob)) title <- paste(title, "with probability", as.character(prob))
  FUN <- function(x,y){
    noChange <- mat[x,y] != 0 | x==1 | x==n | y==1 | y==n
    ifelse(noChange, 
           return(mat[x,y]),
           ifelse(length(which(c(mat[x-1,y],mat[x+1,y],mat[x,y-1],mat[x,y+1])!=0)) >=3,
                  return(mean(c(mat[x-1,y],mat[x+1,y],mat[x,y-1],mat[x,y+1]))), 
                  return(0)))
  }
  smooth <- outer(1:n,1:n,Vectorize(FUN))
  p<-ggfluctuation(as.table(smooth), type = "colour") +
    labs(title=title) +
    scale_fill_gradient(low = "white", high = "blue")
  print(p)
  Sys.sleep(1)
}


# for(i in 300:315){ display.digit(train1[i,],labels1[i])}
# 
# row_pixels <- train1[i,]
# digit <- labels1[i]
# theta <- pi/10

# return a rotated image of the same size as row_pixels
rotate.image <- function(row_pixels, digit = NULL, theta){
  
R <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)), nrow=2, ncol=2)

l <- length(row_pixels)
n <- sqrt(l)
if (l %% n != 0)
  stop("Length of input should be a perfect square.")  
Mat <- matrix(as.double(row_pixels), nrow=n, ncol=n)

newMat <- matrix(0,nrow=n,ncol=n)

half <- round(n/2)

X <- 1:n - half
Y <- 1:n - half

FUN_X <- function(X,Y){
  k<-length(X)
  T <- matrix(0,nrow=2,ncol=k)
  T[1,] <-X
  T[2,] <-Y 
  P <- R %*% T + half
  return(P[1,])}

FUN_Y <- function(X,Y){
  k<-length(X)
  T <- matrix(0,nrow=2,ncol=k)
  T[1,] <-X
  T[2,] <-Y 
  P <- R %*% T + half
  return(P[2,])}

oldX <- outer(X,Y,FUN_X)
oldY <- outer(X,Y,FUN_Y)
oldX <- round(oldX)
oldY <- round(oldY)

oldX <- array(oldX)
oldY <- array(oldY)

l <- n*n
IndexList <- outer(1:n,1:n,
                   Vectorize(function(x,y){
                     rcX <- which(x==oldX,arr.ind=TRUE)
                     rcY <- which(y==oldY,arr.ind=TRUE)
                     ind <- intersect(rcX,rcY)
                     ifelse(length(ind)>0,min(ind),l+1)
                     }))

dummy_pixels <- array(0,l+1)
dummy_pixels[1:l] <- row_pixels[1:l]

new_pixels <- dummy_pixels[IndexList]

#smooth out image
newMat <- matrix(new_pixels,nrow=n,ncol=n)
FUN <- function(x,y){
  noChange <- newMat[x,y] != 0 | x==1 | x==n | y==1 | y==n
  ifelse(noChange, 
         return(newMat[x,y]),
         ifelse(length(which(c(newMat[x-1,y],newMat[x+1,y],newMat[x,y-1],newMat[x,y+1])!=0)) >=3,
                return(mean(c(newMat[x-1,y],newMat[x+1,y],newMat[x,y-1],newMat[x,y+1]))), 
                return(0)))
}
smooth <- outer(1:n,1:n,Vectorize(FUN))
smooth_pixels <- as.vector(smooth)
return(smooth_pixels)

#display.digit(new_pixels,digit,theta)
#smooth.digit(new_pixels,digit,theta)

}

# thetaVec <- c(-pi,-pi/2,-pi/4,-3*pi/16, -pi/8, -pi/16, 0, pi/16, pi/8, 3*pi/16,pi/4,pi/2,pi)
# for(i in 302:303){ 
#   row_pixels <- train1[i,]
#   digit <- labels1[i]
#   for(theta in thetaVec){
#     rotate.image(row_pixels,digit,theta)
# }}