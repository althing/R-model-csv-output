    
    # m1 is a linear model.
    x <- summary(m1)$coefficients
    x <- as.data.frame(x)

    est <- x$Estimate
    err <- x$'Std. Error'

    #returns a vector of coef. ests and st. errors.
    #alternates between the two (so the coef for the first var is first, then its se, then the coef for the second var, etc)
    coefs_and_errors <- c(t(cbind(est,matrix(err, ncol=1, byrow=TRUE))))

    #double var names
    # Recursively repeat vector elements N times each
    #https://stackoverflow.com/questions/15141735/recursively-repeat-vector-elements-n-times-each
    var_names <- rownames(x)
    var_names <- as.data.frame(var_names)
    var_names$blanks <- " "

    var_names_and_blanks <- c(t(cbind(var_names$var_names,matrix(var_names$blanks, ncol=1, byrow=TRUE))))
    
    # repeat each name occurance
    #var_names <- rep(v, each=2)
    #y <- cbind(var_names, coefs_and_errors)
    #df <- as.data.frame(y)    
    #fwrite(df, "./test.csv")

    # extract p values
    pvals <- summary(m1)$coefficients[,4]  
    x <- as.data.frame(pvals)
    x$stars <- " "
    x$blanks <- " "

    x$stars[which(x$pvals < 0.1 & x$pvals >= 0.05)] <- "."
    x$stars[which(x$pvals < 0.05 & x$pvals >= 0.01)] <- "*"
    x$stars[which(x$pvals < 0.01 & x$pvals >= 0.001)] <- "**"
    x$stars[which(x$pvals < 0.001)] <- "***"

    stars <- c(t(cbind(x$stars,matrix(x$blanks, ncol=1, byrow=TRUE))))

    #cbind stars with the var_names and coefs_and_errors above.


    y <- cbind(var_names_and_blanks, coefs_and_errors, stars)
    df <- as.data.frame(y)   
