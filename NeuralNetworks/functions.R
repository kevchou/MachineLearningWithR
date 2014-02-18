CreateNnSkeleton <- function(p, m, q) {

    ## Creates a skeleton for the layout (weights) of the neural
    ## network. Specify the number of hidden layers and nodes with m.
    ##
    ## Args:
    ##   p: The number of inputs variables
    ##   q: The number of outputs variables
    ##   m: A vector for the number of hidden layers and units.
    ##    - Length of m : number of hidden layers
    ##    - Value of m: number of nodes in that layer
    ##    Eg. m = c(3, 3) would give 2 hidden layers, with 3 hidden nodes each
    ##
    ## Returns:
    ##   A list, with each item as the weights for each hidden layer

    ## Needs at least one hidden layer.
    if (length(m) < 1) {
        cat("m cannot be empty\n")
        return(NULL)
    }


    ## Weights for input layer to first hidden layer
    nn.list <- list(w1 = matrix(0, p, m[1]),
                    w1_0 = rep(0,m[1]))

    ## Weights for transitions between the intermediate hidden layers
    if (length(m) > 1) {

        count <- 2

        while (count <= length(m)) {

            mylist = list()
            mylist[[paste0("w", count)]] <- matrix(0, m[count-1], m[count])
            mylist[[paste0("w", count, "_0")]] <- rep(0, m[count])

            nn.list <- append(nn.list, mylist)

            count <- count + 1
        }
    }

    ## Weights from last hidden layer to output
    output.list <- list()
    output.list[[paste0("w", length(m)+1)]] <- matrix(0, m[length(m)], q)
    output.list[[paste0("w", length(m)+1, "_0")]] <- rep(0, q)
    nn.list <- append(nn.list, output.list)

    ## Return the skeleton
    return(nn.list)
}


TrainNn <- function (y, X, m, eta = 0.0002, iters=10000, ...) {

    ## Trains a Neural Network by optimizing the weights using Gradient Descent
    ##
    ## Args:
    ##   X: input matrix
    ##   y: output matrix
    ##   m: number of hidden layers/hidden modes in each layer
    ##   eta: step size for Gradient descent
    ##   iters: max number of iterations
    ##
    ## Returns:
    ##   A list containing the Error for each iteration, and the final weights.

    E <- rep(NA, iters)                 # Sets up vector to hold residuals

    n <- nrow(X)                        # Number of observations
    p <- ncol(X)                        # Number of inputs
    q <- ncol(y)                        # Number of outputs

    skel <- CreateNnSkeleton(p, m, q)   # NN skeleton

    ## Randomly generate initial weights from N(0, 0.1) distribution
    w <- c(rnorm(length(unlist(skel)), 0, 0.1))
    wl <- relist(w, skel)

    fw <- ForwardPropagate(X, wl, ...) # First Forward Propagation

    for (iter in 1:iters) {

        back <- BackPropagate(y, X, wl, fw, ...) # Back propagation
        grad <- CalculateGradient(X, skel, fw, back)  # Calculate gradient

        ## Optimize weights via Gradient Descent
        w  <- w - eta * unlist(grad)
        wl <- relist(w, skel)

        ## Forward propagation with new weights
        fw <- ForwardPropagate(X, wl, ...)

        E[iter] <- mean((fw$out-y)^2)   # Record error

        ## Progress Indicator. Prints out progress every 10%
        if (iter %% (iters/10) == 0) {
            cat(100*iter/iters, '%...', sep='')
        }
    }

    ## Returns vector of the errors, and the final weights
    list(E=E, W=wl)
}


ForwardPropagate <- function (X, wl, funcs =  c('s', 'i' ,'s', 'i')) {

    ## Forward propagation
    ##
    ## X: input matrix
    ## wl: current weights for each node in the network

    n <- nrow(X)
    p <- ncol(X)

    s1 <- matrix(wl$w1_0, n, length(wl$w1_0), byrow=TRUE) + X %*% wl$w1
    h1 <- if (funcs[1] == 's') tanh(s1) else s1

    s2 <- matrix(wl$w2_0, n, length(wl$w2_0), byrow=TRUE) + h1 %*% wl$w2
    h2 <- if (funcs[2] == 's') tanh(s2) else s2

    s3 <- matrix(wl$w3_0, n, length(wl$w3_0), byrow=TRUE) + h2 %*% wl$w3
    h3 <- if (funcs[3] == 's') tanh(s3) else s3

    s4 <- matrix(wl$w4_0, n, length(wl$w4_0), byrow=TRUE) + h3 %*% wl$w4
    out <-  if (funcs[4] == 's') tanh(s4) else s4

    return(list(s1=s1, s2=s2, s3=s3, h1=h1, h2=h2, h3=h3, out=out))
}


BackPropagate <- function (y, X, wl, fw, funcs=c('s', 'i', 's', 'i')) {

    ## Backpropagation

    dE_ds4 <- (if (funcs[4] == 's') (1-fw$out^2) else 1) * 2 * (fw$out - y)
    dE_ds3 <- (if (funcs[3] == 's') (1-fw$h3^2)  else 1) * dE_ds4 %*% t(wl$w4)
    dE_ds2 <- (if (funcs[2] == 's') (1-fw$h2^2)  else 1) * dE_ds3 %*% t(wl$w3)
    dE_ds1 <- (if (funcs[1] == 's') (1-fw$h1^2)  else 1) * dE_ds2 %*% t(wl$w2)

    return(list(dE_ds4=dE_ds4,
                dE_ds3=dE_ds3,
                dE_ds2=dE_ds2,
                dE_ds1=dE_ds1))
}


CalculateGradient <- function (X, skel, fw, bk) {

    ## Calculates the gradient of the log likelihood w.r.t the weights

    p <- ncol(X)
    gr <- skel

    gr$w4_0 <- colSums(bk$dE_ds4)
    gr$w4   <- t(fw$h3) %*% bk$dE_ds4

    gr$w3_0 <- colSums(bk$dE_ds3)
    gr$w3   <- t(fw$h2) %*% bk$dE_ds3

    gr$w2_0 <- colSums(bk$dE_ds2)
    gr$w2   <- t(fw$h1) %*% bk$dE_ds2

    gr$w1_0 <- colSums(bk$dE_ds1)
    gr$w1   <- t(X) %*% bk$dE_ds1

    return(gr)
}
