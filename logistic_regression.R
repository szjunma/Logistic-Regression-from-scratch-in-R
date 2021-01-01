library(ggplot2)
library(dplyr)


N <- 200 # number of points per class
D <- 2 # dimensionality, we use 2D data for easy visualization
K <- 2 # number of classes, binary for logistic regression
X <- data.frame() # data matrix (each row = single example, can view as xy coordinates)
y <- data.frame() # class labels

set.seed(56)

for (j in (1:K)){
  # t, m are parameters of parametric equations x1, x2
  t <- seq(0,1,length.out = N) 
  # add randomness 
  m <- rnorm(N, j+0.5, 0.25) 
  Xtemp <- data.frame(x1 = 3*t , x2 = m - t) 
  ytemp <- data.frame(matrix(j-1, N, 1))
  X <- rbind(X, Xtemp)
  y <- rbind(y, ytemp)
}

data <- cbind(X,y)
colnames(data) <- c(colnames(X), 'label')

# create dir images
dir.create(file.path('.', 'images'), showWarnings = FALSE)

# lets visualize the data:
data_plot <- ggplot(data) + geom_point(aes(x=x1, y=x2, color = as.character(label)), size = 2) + 
  scale_colour_discrete(name  ="Label") + 
  ylim(0, 3) + coord_fixed(ratio = 1) +
  ggtitle('Data to be classified') +
  theme_bw(base_size = 12) +
  theme(legend.position=c(0.85, 0.87))

png(file.path('images', 'data_plot.png'))
print(data_plot)
dev.off()

#sigmoid function, inverse of logit
sigmoid <- function(z){1/(1+exp(-z))}

#cost function
cost <- function(theta, X, y){
  m <- length(y) # number of training examples
  h <- sigmoid(X %*% theta)
  J <- (t(-y)%*%log(h)-t(1-y)%*%log(1-h))/m
  J
}

#gradient function
grad <- function(theta, X, y){
  m <- length(y) 
  
  h <- sigmoid(X%*%theta)
  grad <- (t(X)%*%(h - y))/m
  grad
}

logisticReg <- function(X, y){
  #remove NA rows
  X <- na.omit(X)
  y <- na.omit(y)
  #add bias term and convert to matrix
  X <- mutate(X, bias =1)
  #move the bias column to col1
  X <- as.matrix(X[, c(ncol(X), 1:(ncol(X)-1))])
  y <- as.matrix(y)
  #initialize theta
  theta <- matrix(rep(0, ncol(X)), nrow = ncol(X))
  #use the optim function to perform gradient descent
  costOpti <- optim(theta, fn = cost, gr = grad, X=X, y=y)
  #return coefficients
  return(costOpti$par)
}

logisticProb <- function(theta, X){
  X <- na.omit(X)
  #add bias term and convert to matrix
  X <- mutate(X, bias =1)
  X <- as.matrix(X[,c(ncol(X), 1:(ncol(X)-1))])
  return(sigmoid(X%*%theta))
}

logisticPred <- function(prob){
  return(round(prob, 0))
}

# training
theta <- logisticReg(X, y)
prob <- logisticProb(theta, X)
pred <- logisticPred(prob)

# generate a grid for decision boundary, this is the test set
grid <- expand.grid(seq(0, 3, length.out = 100), seq(0, 3, length.out = 100))
# predict the probability
probZ <- logisticProb(theta, grid)
# predict the label
Z <- logisticPred(probZ)
gridPred = cbind(grid, Z)

# decision boundary visualization
p <- ggplot() + geom_point(data = data, aes(x=x1, y=x2, color = as.character(label)), size = 2, show.legend = F) + 
  geom_tile(data = gridPred, aes(x = grid[, 1],y = grid[, 2], fill=as.character(Z)), alpha = 0.3, show.legend = F)+ 
  ylim(0, 3) +
  ggtitle('Decision Boundary for Logistic Regression') +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 12) 

png(file.path('images', 'logistic_regression.png'))
print(p)
dev.off()
