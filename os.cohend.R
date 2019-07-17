os.cohend = function(data, mu, plot = 1){
    
    smean = mean(data, na.rm = 1)
    sstd = sd(data, na.rm = 1)
    
    d = (mu-smean) / sstd
    
    if (plot == 1){
        plot(density(data), main = 'one sample cohen D')
        abline(v = mu, col = 'red')
        abline(v = smean, col = 'blue')
        text(mu,0,labels = 'mu', col = 'red')
        text(smean,0,labels = 'M', col = 'blue')
        abline(v = smean - sstd, col = 'blue', lty=2, lwd = 0.7)
        abline(v = smean + sstd, col = 'blue', lty=2, lwd = 0.7)
    }
    
    return(d)
}