tt.os = function(x, mu, n, DV, alpha = .05, type = 'two.sided'){
    
    # This wrapper funnction takes care of:
    # (1) normality check
    # (2) t-test
    # (3) effect size calculation (Cohen's D)
    # (4) power calculation:
    # Arguments:
    # x = DV vector
    # mu = value to run t-test against
    # n = number of sample for each group
    # DV = string name for the DV (eg. "RT", "Dwell Time")
    # alpha = significance criterion
    # type = hypothesis tailing
    
    # TODO:
    # Power interpret
    
    source('os.cohend.R')
    library('pwr')
    
    # Check if data is normally distributed
    x.norm = shapiro.test(x)
    # write 
    if (x.norm$p.value >= .05){
        msg = sprintf("Shapiro-Wilk normality test for the DV of %s was not significant (p = %.3f), so the then the null hypothesis that the data are normally distributed is not rejected. ", 
                      DV, x.norm$p.value)
    } else {
        msg = sprintf("Shapiro-Wilk normality test for the DV of %s was significant (W = %.3f, p = %.3f), so the null hypothesis that the data are normally distributed is rejected. ", 
                      DV, x.norm$statistic[[1]], x.norm$p.value)
    }
    # run t-test
    test = t.test(x,mu = mu)
    # run effect size 
    effect = os.cohend(x,mu, plot = 0)
    # sign for p
    if (round(test$p.value,3) == 0){
        psign = '<'
        p = .001
    } else {
        psign = '='
        p = test$p.value
    }
    # write
    if (test$p.value >= .05){
        msg = paste(msg, sprintf("The %s for the DV of %s (M = %.3f, SD = %.3f) was not significant (p = %.3f, 95%% CI [%.3f, %.3f]), so the alternative hypothesis (true mean is not equal to %i) can be rejected. ", 
                                 test$method, DV, mean(x, na.rm = 1), sd(x, na.rm = 1), test$p.value, test$conf.int[1], test$conf.int[2], mu), sep = '')
    } else {
        msg = paste(msg, sprintf("The %s for the DV of %s (M = %.3f, SD = %.3f) was significant (t(%i) = %.3f, p %s %.3f, 95%% CI [%.3f, %.3f]), so the alternative hypothesis (true mean is not equal to %i) can not be rejected. The effect size (Cohen's) was D = %.3f. ", 
                                 test$method, DV, mean(x, na.rm = 1), sd(x, na.rm = 1), test$parameter[[1]], test$statistic[[1]], psign, p, test$conf.int[1], test$conf.int[2], mu, abs(effect)), sep = '')
    }
    
    # run power analysis
    power = pwr.t.test(n, d = effect, sig.level = alpha, type = 'one.sample', alternative = type)
    # write
    if (test$p.value <= 0.05){
        
        # power type
        if (type == 'two.sided'){
            powertype = 'two-tailed'
        } else {
            powertype == 'one-tailed'
        }
        
        msg = paste(msg, sprintf("The observed power was %.3f (n = %i, alpha = %.2f, d = %.3f, %s).", 
                                 power$power, power$n, power$sig.level, power$d, powertype))
    }
    
    # return written stuff
    return(msg)
}
