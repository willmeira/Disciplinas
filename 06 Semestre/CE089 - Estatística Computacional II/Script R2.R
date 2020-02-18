## Obtem m valores da U(0,0.5)
    m <- 10000
    x <- runif(m, min = 0, max = 0.5)
    ## Calcula g(x)
    theta.hat <- exp(-x)
    ## Calcula a média
    (m.theta.hat <- sum(theta.hat)/m*(0.5-0))
    
    hist(theta.hat); abline(v = theta, col = 5)
    
        # Nesse caso, podemos obter a solução analítica e integração numérica no R para comparar as estimativas
    
    ## Solução analítica
    (theta <- 1 - exp(-0.5))
    ## Integração numérica no R
    integrate(function(x) exp(-x), lower = 0, upper = 0.5)

    
    ## Obtem m valores da Exp(0.5)
    m <- 10000
    x <- rexp(m, 0.5)
    ## Calcula g(x)
    theta.hat <- exp(-x)
    ## Calcula a média
    (m.theta.hat <- sum(theta.hat)/m*(0.5-0))
    
    hist(theta.hat); abline(v = theta, col = 2)
    
    x <- rexp(m, 1)   # usando f1
    fg <- g(x) / exp(-x)
    theta.hat[2] <- mean(fg)
    se[2] <- sd(fg)
    
    
    
    # Nesse caso, podemos obter a solução analítica e integração numérica no R para comparar as estimativas
    
    ## Solução analítica
    (theta <- 1 - exp(-0.5))
    ## Integração numérica no R
    integrate(function(x) exp(-x), lower = 0, upper = 0.5)
    