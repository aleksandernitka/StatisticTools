tt.os = function(x, mu, N, DV, alpha = .05, type = 'two.sided'){
    
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
    # t-test nonparam is One-Sample Wilcoxon Signed Rank Test - add
    
    source('os.cohend.R')
    source('outlierlbl.R')
    library('pwr')
    library('DescTools') # for win
    
    # Check if data is normally distributed
    x.norm = shapiro.test(x)
    # write 
    if (x.norm$p.value >= .05){
        msg = sprintf("Shapiro-Wilk normality test for the DV of %s was not significant (p = %.3f), so then the null hypothesis that the data are normally distributed is not rejected. ", 
                      DV, x.norm$p.value)
    } else {
        msg = sprintf("Shapiro-Wilk normality test for the DV of %s was significant (W = %.3f, p = %.3f), so the null hypothesis that the data are normally distributed is rejected. ", 
                      DV, x.norm$statistic[[1]], x.norm$p.value)
    }
    
    # Check for outliers
    outl = outlierlbl(x, plot = 0)
    if (outl$outlierN == 0){
        msg = paste(msg, "No outliers were detected. ", sep = '')
    } else {
        # winsorize to remove outliers
        x = Winsorize(x, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),  na.rm = FALSE)
        # re-run normality
        x.norm = shapiro.test(x)
        
        if (x.norm$p.value >= .05){
            msg = paste(msg, "A total of %i outliers were detected, 90% winsorization was applied to the data and Shapiro-Wilk normality test re-run, which was not significant (p = %.3f) so the then the null hypothesis that the data are normally distributed is not rejected ", 
                        outl$outlierN, x.norm$p.value)
        } else {
            msg = paste(msg, "A total of %i outliers were detected, 90% winsorization was applied to the data and Shapiro-Wilk normality test re-run, which was significant (W = %.3f, p = %.3f) so the null hypothesis that the data are normally distributed is rejected. ", 
                        outl$outlierN, x.norm$statistic[[1]], x.norm$p.value)
        }

    }
    
    # run t-test or wilcoxon
    if (x.norm < 0.05){
        test = wilcox.test(x, mu = 0, alternative = "two.sided")
    } else {
        test = t.test(x,mu = mu)
    }
    
    # run effect size 
    cohen = os.cohend(x,mu,N)
    d = cohen$d
    r = cohen$r
    dCIl = cohen$lCI # CI
    dCIu = cohen$uCI # CI

    # run power analysis
    power = pwr.t.test(N, d = d, sig.level = alpha, type = 'one.sample', alternative = type)
    
    # sign for p
    if (round(test$p.value,3) == 0){
        psign = '<'
        p = .001
    } else {
        psign = '='
        p = test$p.value
    }
    
    # power type
    if (type == 'two.sided'){
        powertype = 'two-tailed'
    } else {
        powertype == 'one-tailed'
    }
    
    # write
    if (test$p.value >= .05){
        msg = paste(msg, sprintf("The %s for the DV of %s (M = %.3f, SD = %.3f) was not significant (p = %.3f, 95%% CI [%.3f, %.3f]. The unbiased Hodge’s corrected d = %.3f, CI 95%% [%.3f, %.3f], r = %.3f, suggesting that the mean of %s and mu = %.3f differ by a %.3f of SD of the data. The alternative hypothesis (true mean is not equal to %i) can be rejected. ", 
                                 test$method, DV, mean(x, na.rm = 1), sd(x, na.rm = 1), test$p.value, test$conf.int[1], test$conf.int[2], abs(d), dCIl, dCIu, r, DV, mu, d,mu), sep = '')
    } else {
        msg = paste(msg, sprintf("The %s for the DV of %s (M = %.3f, SD = %.3f) was significant (t(%i) = %.3f, p %s %.3f, 95%% CI [%.3f, %.3f]), so the alternative hypothesis (true mean is not equal to %i) can not be rejected. The effect size (unbiased Hodge’s corrected Cohen's) was d = %.3f, 95%% CI [%.3f, %.3f], r = %.3f, suggesting that the mean of %s and mu = %.3f differ by a %.3f of SD of the data.", 
                                 test$method, DV, mean(x, na.rm = 1), sd(x, na.rm = 1), test$parameter[[1]], test$statistic[[1]], psign, p, test$conf.int[1], test$conf.int[2], mu, abs(d), dCIl, dCIu, r, DV, mu, d), sep = '')
    }
    
    # write on power
    if (test$p.value < .05){
        if (power$power > .8){
            msg = paste(msg, sprintf('The observed power for that effect size was %.3f (N = %i, alpha = %.2f, %s) which is greater than the recommended power of 0.8 and means a %.2f%% probability of encountering a Type-II error. ', 
                                     power$power, power$n, power$sig.level, powertype, (1-power$power)*100), sep = '')
        } else if (power$power == .8){
            msg = paste(msg, sprintf('The observed power for that effect size was %.3f (N = %i, alpha = %.2f, %s) which is equal to the recommended power of 0.8 and means a %.2f%% probability of encountering a Type-II error. ', 
                                     power$power, power$n, power$sig.level, powertype, (1-power$power)*100), sep = '')
        } else {
            msg = paste(msg, sprintf('The observed power for that effect size was %.3f (N = %i, alpha = %.2f, %s) which is below the recommended power of 0.8 and means a %.2f%% probability of encountering a Type-II error. ', 
                                     power$power, power$n, power$sig.level, powertype, (1-power$power)*100), sep = '')
            
        }
    }

    
    # return written stuff
    return(msg)
}
