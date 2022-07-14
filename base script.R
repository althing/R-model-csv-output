    models.list <- list(m1, m2, m3, m4, m5, m6)

    # create master list of unique row names and variable names.
    # all models will use the unique colnames to set their row order.   

        full.model.number <- 1 # specify model that has all variables that appear in the table

        results <- summary(models.list[[full.model.number]])$coefficients
        results <- as.data.frame(results)

        # create unique row names
        var.names <- rownames(results)
        var.names <- as.data.frame(var.names)
        setnames(var.names, "var.names", "coef.names")
        var.names$se.names <- var.names$coef.names
        var.names$coef.names <- paste0(var.names$coef.names, ".c")
        var.names$se.names <- paste0(var.names$se.names, ".se")
        row.names <- c(t(cbind(var.names$coef.names, matrix(var.names$se.names, ncol=1, byrow=TRUE))))
        row.names.complete <- as.data.frame(row.names)

        # create variable names as they'll appear on the table (with blanks for se rows) 
        var.names <- rownames(results)
        var.names <- as.data.frame(var.names)
        var.names$blanks <- " "
        var.names.and.blanks.complete <- c(t(cbind(var.names$var.names, matrix(var.names$blanks, ncol=1, byrow=TRUE))))

        y.complete <- cbind(row.names.complete, var.names.and.blanks.complete)


    for (i in 1:length(models.list)) {

                        #i <- 2

        results <- summary(models.list[[i]])$coefficients
        results <- as.data.frame(results)

        # create unique row names
        var.names <- rownames(results)
        var.names <- as.data.frame(var.names)
        setnames(var.names, "var.names", "coef.names")
        var.names$se.names <- var.names$coef.names
        var.names$coef.names <- paste0(var.names$coef.names, ".c")
        var.names$se.names <- paste0(var.names$se.names, ".se")
        row.names <- c(t(cbind(var.names$coef.names, matrix(var.names$se.names, ncol=1, byrow=TRUE))))
        row.names <- as.data.frame(row.names)


        # create vector of coef estimates and se's.
        # vector alternates between the two (so the coef for the first var is first, then its se, then the coef for the second var, etc).
        est <- results$Estimate
        est <- round(est, 3)
        err <- results$'Std. Error'
        err <- round(err, 3)
        err <- paste0("(", err, ")")
        coefs.and.errors <- c(t(cbind(est,matrix(err, ncol=1, byrow=TRUE)))) 
        #coefs.and.errors <- round(coefs.and.errors, 3)

        # create stars (*) column, indicating significance
        pvals <- results[,4] # extract p values
            # or use  summary(models.list[[i]])$coefficients[,4]
        x <- as.data.frame(pvals)
        x$stars <- " "
        x$blanks <- " "
        x$stars[which(x$pvals < 0.1 & x$pvals >= 0.05)] <- "†"     # try special unicode characters like †.
        x$stars[which(x$pvals < 0.05 & x$pvals >= 0.01)] <- "*"
        x$stars[which(x$pvals < 0.01 & x$pvals >= 0.001)] <- "**"
        x$stars[which(x$pvals < 0.001)] <- "***"
        stars <- c(t(cbind(x$stars,matrix(x$blanks, ncol=1, byrow=TRUE))))

        y <- cbind(row.names, coefs.and.errors, stars)

        #make the coefs.and.errors and stars column names unique for each model.
        colnames(y)[2] <- paste0(colnames(y)[2], i)
        colnames(y)[3] <- paste0(colnames(y)[3], i)

        y.complete <- left_join(y.complete, y, by = "row.names")


    }

    # add model numbers to the appropriate cols.
    vec <- rep("", each = ncol(y.complete)) # make empty vector.
    nums <- seq(3, length(vec), by=2) # make vector of cols the model numbers will be added to.
    for (i in 1:length(nums)) {
        a <- paste0("(", i, ")")
        vec[nums[i]] <- a
    }
    y.complete <- rbind(vec, y.complete)
    
    #add dv name to top of col 2
    vec <- rep("", each = ncol(y.complete)) # make empty vector.
    a <- as.character(summary(models.list[[full.model.number]])$terms[[2]]) #full.model.number is set above.
    vec[2] <- paste0("DV: ", a)
    y.complete <- rbind(vec, y.complete)

    # add R2 row.
    my.list <- list()
    for (j in 1:length(models.list)) {
        my.list[j] <- attr(summ(models.list[[j]]), "rsq")  
    }
    vec <- round(unlist(my.list), 3)
    vec <- as.character(vec)
    blanks <- rep("", each = length(vec))
    vec <- c(t(cbind(vec, matrix(blanks, ncol=1, byrow=TRUE))))
    line.and.var.name <- c("R2", "R2")
    vec <- append(line.and.var.name, vec)
    y.complete <- rbind(y.complete, vec)

    # add Adj R2 row.
    my.list <- list()
    for (j in 1:length(models.list)) {
        my.list[j] <- attr(summ(models.list[[j]]), "arsq")  
    }
    vec <- round(unlist(my.list), 3)
    vec <- as.character(vec)
    blanks <- rep("", each = length(vec))
    vec <- c(t(cbind(vec, matrix(blanks, ncol=1, byrow=TRUE))))
    line.and.var.name <- c("aR2", "Adj R2")
    vec <- append(line.and.var.name, vec)
    y.complete <- rbind(y.complete, vec)

    # add N row
    my.list <- list()
    for (j in 1:length(models.list)) {
        my.list[j] <- attr(summ(models.list[[j]]), "n")  
    }
    vec <- unlist(my.list)
    vec <- as.character(vec)
    blanks <- rep("", each = length(vec))
    vec <- c(t(cbind(vec, matrix(blanks, ncol=1, byrow=TRUE))))
    line.and.var.name <- c("", "N")
    vec <- append(line.and.var.name, vec)
    y.complete <- rbind(y.complete, vec)



    y.complete$row.names <- NULL
        


    View(y.complete)
    fwrite(y.complete, "./table_output.csv")

