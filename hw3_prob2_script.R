parameters_3 = data.frame(vmax = 0,k = 0)
for (m in 1:1000){
  xm = (enzyme$x+rnorm(length(enzyme$x),0,0.05))
  fit = nls(enzyme$y ~ (v*xm)/(k + xm), data = enzyme, start = list(v = 1300,k = 10))
  parameters_3[nrow(parameters_3)+1,] <- coef(fit)  
}
parameters_3 <- parameters_3[-1,]
#plot(parameters_3[,1], parameters_3[,2], xlim = c(1300,1400), ylim = c(5,7))
plot(parameters_3[,1], parameters_3[,2], xlim = c(1330,1380), ylim = c(5.5,6.2))
lines((ellipse(fit, which = c('v', 'k'), level = 0.90)), col="blue", type = 'l')
lines((ellipse(fit, which = c('v', 'k'), level = 0.95)), col="red", type = 'l')
lines((ellipse(fit, which = c('v', 'k'), level = 0.99)), col="green", type = 'l')

#another method
parameters_4 = data.frame(v = 0,k = 0)
for (m in 1:1000){
  xm = (enzyme$x*(rnorm(length(enzyme$x),1,0.05)))
  fit = nls(enzyme$y ~ (v*xm)/(k + xm), data = enzyme, start = list(v = 1300,k = 10))
  parameters_4[nrow(parameters_4)+1,] <- coef(fit)  
}
parameters_4 <- parameters_4[-1,]
shapiro.test(parameters_4[,1])
shapiro.test(parameters_4[,2])

plot(k, v, xlim = c(4.5,7), ylim = c(1280,1440))
lines((ellipse(fit, which = c('k', 'v'), level = 0.90)), col="blue") #type = 'l')
lines((ellipse(fit, which = c('k', 'v'), level = 0.95)), col="red") #type = 'l')
lines((ellipse(fit, which = c('k', 'v'), level = 0.99)), col="green") #type = 'l')

#

plot(parameters[,1], parameters[,2], xlim = c(1270,1440), ylim = c(4.5,7.3))
lines((ellipse(fit, which = c('v', 'k'), level = 0.90)), col="blue", type = 'l')
lines((ellipse(fit, which = c('v', 'k'), level = 0.95)), col="red", type = 'l')
lines((ellipse(fit, which = c('v', 'k'), level = 0.99)), col="green", type = 'l')