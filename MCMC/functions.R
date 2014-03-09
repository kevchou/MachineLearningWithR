library(stringr)
library(ggplot2)
library(reshape2)


## Removes all punctuation, numbers, multiple spaces from a string input
textonly <- function(x) {
    x <- str_replace_all(x, "[[:punct:]]", "")   # Removes all symbols
    x <- str_replace_all(x, "[[:digit:]]", "")   # Removes all numbers
    x2 <- str_replace_all(x, "  ", " ")
    while (x2 != x) {
        x <- x2
        x2 <- str_replace_all(x2, "  ", " ")
    }
    x2
}

## Decrypts or encrypts a string using a specified mapping
## decipher=TRUE: input an ENCRYPTED string, outputs decrypted string using mapping
## decipher=FALSE: input an UNENCRYPTED string, encrypts it using specified mapping
decipher <- function(input, mapping, decipher=TRUE) {
    mapping <- paste(mapping, collapse="")
    letter <- paste(toupper(letters), collapse="")
    input <- toupper(input)
    if (decipher) { output <- chartr(mapping, letter, input) }
    else {output <- chartr(letter, mapping, input)}
    output
}

## Calculates Score using specified mapping
score <- function(string_input, mapping) {
    string_deciphered <- decipher(string_input, mapping)
    temp <- embed(unlist(strsplit(toupper(string_deciphered), '')), 2)
    temp.count <- table( temp[,2], temp[,1])
    temp.melt <- melt(temp.count)

    let <- c(' ', toupper(letters) )
    score= 0
    for (i in let) {
        for (j in let) {
            r.ij <- pair.count$value[which(pair.count$Var1==i & pair.count$Var2==j)] + 1
            fx.ij <- temp.melt$value[which(temp.melt$Var1==i & temp.melt$Var2==j)] + 1
            if (is.na(fx.ij) || length(fx.ij) == 0) { fx.ij = 1 }
            score = score + fx.ij*log(r.ij)
        }
    }
    score
}

## Attempts to decrypt simple substitution cipher using MCMC
## inputs: encrypted string
## p = scaling parameter (tempering)
## iterations = max no. of iterations
## start.map= mapping to start with (random by default)
run.mcmc.decipher <- function(to_decipher, p=1, iterations=2000, start.map = sample(toupper(letters)) ) {
    mapping <- start.map

    current.decipher <- decipher(to_decipher, mapping)
    current.score <- score(to_decipher, mapping)

    max.decipher <- current.decipher
    max.score <- current.score

    i <- 1
    numaccept <- 0
    list <- NULL
    while( i <= iterations) {
                                        # Randomly switch 2 letters in the mapping
        proposal <- sample(1:26, 2)
        prop.mapping <- mapping
        prop.mapping[proposal[1]] = mapping[proposal[2]]
        prop.mapping[proposal[2]] = mapping[proposal[1]]

        prop.score <- score(to_decipher, prop.mapping)

        if( log(runif(1)) < p*(prop.score - current.score) ) {
            mapping = prop.mapping
            current.score <- prop.score
            current.decipher <- decipher(to_decipher, mapping)

            if( current.score > max.score) {
                max.score = current.score
                max.mapping = mapping
            }
            numaccept <- numaccept+1
        }
        cat(i, substring(current.decipher,1,50), current.score, '\n')
        if (i %% 100 ==0 ) {
            list <- rbind(list, c(i, substring(current.decipher, 1, 70), current.score))
        }
        i=i+1
    }
    ret <- list(max.score = max.score, max.mapping = max.mapping, list=list, numaccept= numaccept)
    return(ret)
}

## Splits a string into "period" substrings
text.split <- function(x, period) {
    x <- unlist(strsplit(x," "))  # Splits string into words
    string = matrix(0, nrow=ceiling(length(x)/period) ,ncol=period)

    for ( i in 1:ceiling(length(x)/period)) {
        temp <- NULL
        for (j in (period*(i-1) +1) :(i*period)) {
            if ( is.na(x[j])) { x[j] = ""}
            temp <- c(temp, x[j])
        }
        string[i,] = temp
    }
    out <- list()
    for (i in 1:period) {
        out[[i]] <- paste(string[,i], collapse=" ")
    }
    unlist(out)
}

## Decrypts substitution cipher with different mapping every period
decipher2 <- function(input.string, map.list, period, ...) {
    string.split <- text.split(input.string, period)
    deciphered <- list()
    for (i in 1:period) {
        deciphered[[i]] <- decipher(string.split[i], map.list[[i]], ...)
        deciphered[[i]] <- unlist(strsplit(deciphered[[i]], " "))
    }

    if (period > 1) {
        n <- length(deciphered[[1]])
        for (j in 2:period) {
            if (length(deciphered[[j]]) < n) { deciphered[[j]] <- c(deciphered[[j]], "")}
        }
    }
    recombine.mat <- NULL
    for (i in 1:period) {
        recombine.mat <- cbind(recombine.mat, deciphered[[i]])
    }
    recombined <- paste(as.vector(t(recombine.mat)), collapse=" ")
    textonly(recombined)
}

## Calculates score of each substring, and adds them up
score2 <- function( input, maplist, period) {
    input.split <- text.split(input, period)
    score.list <- NULL
    score.sum <- 0
    for (i in 1:period) {
        score.list[i] <- score(input.split[i], maplist[[i]])
        score.sum <- score.sum + score.list[[i]]
    }
    score.sum
}

## MCMC decipher with periods
run.mcmc.decipher2 <- function(to_decipher, period, p=1, iterations=2000) {
    map.list <- list()
    for (i in 1:period){
        map.list[[i]] <- sample(toupper(letters))
    }

    current.decipher <- decipher2(to_decipher, map.list, period)
    current.score <- score2(to_decipher, map.list, period)

    max.decipher <- current.decipher
    max.score <- current.score

    i <- 0
    numaccept <- 0
    list <- list()
    while( i <= iterations) {
        ## Randomly switch 2 letters in the mapping
        proposal.list <- list()
        prop.mapping.list = map.list
        for (j in 1:period){
            proposal.list[[j]] <- sample(1:26, 2)
            prop.mapping.list[[j]][proposal.list[[j]][1]] = map.list[[j]][proposal.list[[j]][2]]
            prop.mapping.list[[j]][proposal.list[[j]][2]] = map.list[[j]][proposal.list[[j]][1]]
        }
        prop.score <- score2(to_decipher, prop.mapping.list, period)

        if( log(runif(1)) < p*(prop.score - current.score)) {
            map.list = prop.mapping.list
            current.score <- prop.score
            current.decipher <- decipher2(to_decipher, map.list, period)

            if( current.score > max.score) {
                max.score = current.score
                max.mapping.list = prop.mapping.list
            }
            numaccept = numaccept + 1
        }

        if (i %% 100 ==0 ) {
            accuracy.count <- rep(0,period)
            for (n in 1:period) {
                accuracy.count[n] <- sum(map.list[[n]] == starting.map[[n]])
            }
            accuracy <- accuracy.count/26
            list <- rbind(list, c(i, substring(current.decipher, 1, 70), current.score, accuracy))
            colnames(list) <- c('iteration', 'decoded string', 'score', rep('accuarcy', period))
        }
        cat(i, substring(current.decipher,1,50), current.score, accuracy, '\n')
        i=i+1
    }
    ret <- list(max.score = max.score, start.mapping = starting.map, max.mapping.list = max.mapping.list, list=list, numaccept = numaccept)
    return(ret)
}

## MCMC decipher with periods, using random scan updating 1 map at a time
run.mcmc.decipher.random <- function(to_decipher, period, p=1, iterations=2000) {

    map.list <- list()
    for (i in 1:period){
        map.list[[i]] <- sample(toupper(letters))
    }

    current.decipher <- decipher2(to_decipher, map.list, period)
    current.score <- score2(to_decipher, map.list, period)

    max.decipher <- current.decipher
    max.score <- current.score

    i <- 0
    numaccept <- 0
    list <- list()
    while( i <= iterations) {
        ## Randomly switch 2 letters in the mapping
        ## random scan through no. of periods
        prop.mapping.list = map.list
        coord <- floor(runif(1,0,period)) + 1
        proposal.switch <- sample(1:26, 2)
        prop.mapping.list[[coord]][proposal.switch[1]] = map.list[[coord]][proposal.switch[2]]
        prop.mapping.list[[coord]][proposal.switch[2]] = map.list[[coord]][proposal.switch[1]]

        prop.score <- score2(to_decipher, prop.mapping.list, period)

        if( log(runif(1)) < p*(prop.score - current.score)) {
            map.list = prop.mapping.list
            current.score <- prop.score
            current.decipher <- decipher2(to_decipher, map.list, period)

            if( current.score > max.score) {
                max.score = current.score
                max.mapping.list = prop.mapping.list
            }
            numaccept = numaccept + 1
        }

        ## Saves information every 100 iterations
        if (i %% 100 ==0 ) {
            accuracy.count <- rep(0,period)
            for (n in 1:period) {
                accuracy.count[n] <- sum(map.list[[n]] == starting.map[[n]])
            }
            accuracy <- accuracy.count/26
            list <- rbind(list, c(i, substring(current.decipher, 1, 60), current.score, accuracy))
        }

        ## Prints out current iteration info
        cat(i, substring(current.decipher,1,60), current.score, accuracy, '\n')
        i=i+1
    }
    colnames(list) <- c('iteration', 'decoded string', 'score', rep('accuarcy', period))
    ret <- list(max.score = max.score, start.mapping = starting.map, max.mapping.list = max.mapping.list, list=list, numaccept = numaccept)
    return(ret)
}

                                        ## Creates 'period' random mappings
create.map <- function(period) {
    map.list <- list()
    for (i in 1:period) {
        map.list[[i]] <- sample(toupper(letters))
    }
    map.list
}

## Unigram attack. replaces most frequent letter in encrypted text with most frequent
## letter in reference text, etc
unigram <- function(test) {
    let <- unlist(strsplit(training, ''))
    let.count <- table(let)
    let.count <- let.count[-1]
    let.sorted <- sort(let.count, decreasing = TRUE)

    coded.char <- unlist(strsplit(test, ''))
    coded.count <- table(coded.char)
    coded.sorted <- sort(coded.count[-which(names(coded.count) == ' ')], decreasing=TRUE)

    lettersNotInCoded <- toupper(letters)[which(toupper(letters) %in% names(coded.sorted)== FALSE)]
    lettersNotInCoded <- sample(lettersNotInCoded)
    coded.sorted2 <- c(coded.sorted, rep(0, length(lettersNotInCoded)))
    names(coded.sorted2) <- c(names(coded.sorted), lettersNotInCoded)

    mapping = NULL

    for (i in 1:length(letters)) {
        mapping[i] = names(coded.sorted2)[which(names(let.sorted) == toupper(letters)[i])]
    }
    new.test<- decipher(test, mapping)
    ret <- list(deciphered = new.test, mapping = mapping)
    ret
}


## Set up War and Peace for reference text
readfile <- readLines('warandpeace.txt')            # Read in War and Peace text file
training <- toupper(paste(readfile, collapse=' '))  # Collapses it to one line
training <- textonly(training)

## Creates a table of number of times (row letter) is followed by (column letter)
let <- unlist(strsplit(training, ''))
let2 <- embed(let, 2)
mat.count <- table( let2[,2], let2[,1] )
pair.count <- melt(mat.count)

## Divides each number in a row by its row sum. Frequency
mat.trans <- sweep( mat.count, 1, rowSums(mat.count), FUN='/')
