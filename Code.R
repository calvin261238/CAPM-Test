#Import file
capm <- read.table(file="Week5DataLarge.csv", header=T, sep=",")

#Count col (number of companies)
L <- ncol(capm)
I <- L - 2 

#Naming beta, alpha, t-value
Betas <- rep(0,417)
Alphas <- rep(0,417)
t_values <- rep(0,417)
Average <- rep(0,417)

#Excess Return of market and stock 1
Rm <- capm[,2] - capm[,1]
Ri <- capm[,3] - capm[,1]

#Regression
myResult <- lm(Ri~Rm)
mysummary <- summary(myResult)
mycoef <- coef(mysummary)

Betas[1] <- mycoef[2,1]
Alphas[1] <- mycoef[1,1]
t_values[1] <- mycoef[1,3]
Average[1] <- mean(Ri)

#Create a loop
for(i in 1:I){
  Ri <- capm[,2+i] - capm[,1]
  myResult <- lm(Ri~Rm)
  mysummary <- summary(myResult)
  mycoef <- coef(mysummary)
  Betas[i] <- mycoef[2,1]
  Alphas[i] <- mycoef[1,1]
  t_values[i] <- mycoef[1,3]
  Average[i] <- mean(Ri)
}

#Visualize
plot(Betas, Average)
x <- c(1:250)/100
points(x, mean(Rm)*x, col="red", type="l")

#find out which company has high Beta or low Beta

companies <- colnames(capm)
companies2 <- companies[3:L]

data <- data.frame(companies2,Betas) #contain all companies & their betas

myOrder <- order(Betas, decreasing=T)

y <- data[myOrder, ]

Beta.high <- y[1:10, ]
Beta.low <- y[I:(I-9),]
