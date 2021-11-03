parameters <- c(k1 = 0.04,k2 = 3e7,k3 = 1e4)
yinitial <- c(A = 1,B = 0,C = 0)
#times <- log(seq(0,1000000,by = 0.01))
model <- function(t,yinitial,parameters){ 
  with(as.list(c(yinitial,parameters)),{ 
    #import<-input(time)
    dA <- -k1 * A + k3 *B * C
    dB <- k1 * A - k3 * B * C - k2 * B^2 + k2*B*C
    dC <- k2 * B^2      
    list(c(dA,dB,dC))
  }) 
}
times <- 10^seq(-6,6,.1)
out <- ode(y = yinitial, times = times, func = model, parms = parameters)
plot(out[,1],out[,2],xlab ="time", ylab ="concentration(umol)", log = 'xy',main = "concentration B")
#+ lines(out[,1],out[,3])
#+ lines(out[,1],out[,4])
#ggplot(as.data.frame(out), main ="concentration") + scale_x_continuous(trans = "log10")
