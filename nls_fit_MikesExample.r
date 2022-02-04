#Non-linear least squares fitting in R Mike's Example

#generate some data using a non-linear model y=1-exp(-a*x) and add some noise
x=seq(0,100,length=1000)
err=rnorm(1000,0,0.1)
y=1-exp(-0.1*x)+err

#store this data in a data frame
pseudo=data.frame(cbind(x,y))


#plot the data
plot(x,y)

#use nls - non linear least squares - to fit the non-linear model y=1-exp(-a*x) to the data
res=nls(y~1-exp(-a*x),data=pseudo, start=list(a=0.001))

#print out the sumamry
summary(res)

#use the estimate to generate fitted values
fitted=1-exp(-0.101193*x)

#add fitted line to the plot
o=order(x)
lines(x[o],fitted[o],col="red",lwd=2)
