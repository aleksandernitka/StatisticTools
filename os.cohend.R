os.cohend = function(data, mu, N){
    
    # replicates http://www.real-statistics.com/students-t-distribution/one-sample-t-test/confidence-interval-one-sample-cohens-d/
    
    t = t.test(data, mu = mu)$statistic[[1]]
    d = t/sqrt(N)
    
    # CIs
    # d ± se * z
    # se = sqrt( 1/n + d^2 / 2n )
    
    se = sqrt(1/N + d^2 / (2*N))
    z = qnorm(1-0.05/2)
    
    lCI = d - se * z
    uCI = d + se * z
    
    return(list(d = d, lCI = lCI, uCI = uCI))
}