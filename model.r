library(lavaan)

model <- ' # set time
           i =~ 1*t1 + 1*t2 + 1*t3
           s =~ 0*t1 + 1*t2 + 2*t3 
         '


print(Demo.growth)
fit <- growth(model, data=Demo.growth)
summary(fit)
#fname <- "RUNDMC_datasheet.csv"
#df <- read.csv(fname, sep = ";")
#print(names(df))
