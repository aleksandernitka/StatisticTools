ps.cohensd = function(data1, data2, plot = 1){
    
    m1 = mean(data1, na.rm = 1)
    m2 = mean(data2, na.rm = 1)
    sd1 = sd(data1, na.rm = 1)
    sd2 = sd(data2, na.rm = 1)
    
    sdpooled = sqrt( (sd1^2 + sd2^2) /2)
    
    d = (m2 - m1) / sdpooled
    
    if (plot == 1){
        library(ggplot2)
        d1 = data.frame(data1)
        d2 = data.frame(data2)
        names(d1) = c('DV')
        names(d2) = names(d1)
        d1$data = 'x'
        d2$data = 'y'
        vals = rbind(d1, d2)
        ggplot(vals, aes(DV, fill = data)) + geom_density(alpha = 0.2)
    }
    
    return(d)
    
}

carrots <- data.frame(length = rnorm(100000, 6, 2))
cukes <- data.frame(length = rnorm(50000, 7, 2.5))
carrots$veg <- 'carrot'
cukes$veg <- 'cuke'
