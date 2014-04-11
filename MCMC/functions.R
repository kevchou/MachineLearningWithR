library(stringr)  # For string manipulation
library(ggplot2)  # Plots
library(reshape2) # Reshaping data tables

CleanText <- function(x) {
    ## Removes all punctuation, numbers, multiple spaces from a string input
    ##
    ## Args:
    ##   x: a string
    ##
    ## Returns:
    ##   Cleaned up string with no numbers and symbols

    x <- str_replace_all(x, "[[:punct:]]", "")   # Removes all symbols
    x <- str_replace_all(x, "[[:digit:]]", "")   # Removes all numbers

    ## Removes multiple spaces
    x2 <- str_replace_all(x, "  ", " ")
    while (x2 != x) {
        x <- x2
        x2 <- str_replace_all(x2, "  ", " ")
    }

    return(x2)
}

CipherText <- function(input, mapping, decipher=TRUE) {
    ## Decrypts or encrypts a string using a simple substitution cipher with
    ## inputted mapping
    ##
    ## Args:
    ##   input: the string to decrypt/encrypt
    ##   mapping: mapping to use
    ##   decipher: if TRUE, then dencrypts input string. False: encrypts string
    ##
    ## Returns:
    ##   The encrypted/decrypted string

    mapping <- paste(mapping, collapse="")

    letter <- paste(toupper(letters), collapse="") # Capitalize everything

    input <- toupper(input)

    ## Encrypt or decrypt
    if (decipher) {
        output <- chartr(mapping, letter, input)
    } else {
        output <- chartr(letter, mapping, input)
    }

    return(output)
}


CalcScore <- function(string_input, mapping) {
    ## Calculates current Score with current mapping
    ##
    ## Args:
    ##   string_input: The current string
    ##   mapping: current mapping used to decrypt
    ##
    ## Returns:
    ##   The current score

    string_deciphered <- CipherText(string_input, mapping)

    ## Create table of consecutive letters
    temp <- embed(unlist(strsplit(toupper(string_deciphered), split='')), 2)
    temp.count <- table( temp[,2], temp[,1])
    temp.melt <- melt(temp.count)

    let <- c(' ', toupper(letters) )    # All letters

    score <- 0

    ## Calculates score
    for (i in let) {
        for (j in let) {
            r.ij <- pair.count$value[which(pair.count$Var1==i & pair.count$Var2==j)] + 1
            fx.ij <- temp.melt$value[which(temp.melt$Var1==i & temp.melt$Var2==j)] + 1
            if (is.na(fx.ij) || length(fx.ij) == 0) { fx.ij = 1 }
            score = score + fx.ij*log(r.ij)
        }
    }

    return(score)
}

RunMcmcDecipher <- function(to.decipher,
                            p = 1,
                            iterations = 2000,
                            start.map = sample(toupper(letters))) {
    ## Uses the Metropolis algorithm to decrypt a simple substitution cipher
    ##
    ## Args:
    ##   to_decipher: the text to decrypt
    ##   p: scaling (tempering) parameter
    ##   iterations: number of iterations to run for MCMC
    ##   start.map: letter mapping to start with. Random by default
    ##
    ## Returns:
    ##   a list object containing:
    ##     - The decrypted text (if successful)
    ##     - Mapping used for the decryption
    ##     - table of scores

    current.mapping  <- start.map
    current.decipher <- CipherText(to.decipher, current.mapping)
    current.score    <- CalcScore(to.decipher, current.mapping)

    max.decipher <- current.decipher
    max.score    <- current.score

    ## To keep track of all scores
    all.scores <- rep(NA, iterations)
    text.history <- rep(NA, iterations)
    num.accept <- 0

    i <- 1

    while(i <= iterations) {

        ## Randomly switch 2 letters in the mapping
        proposal <- sample(1:26, 2)
        prop.mapping <- current.mapping
        prop.mapping[proposal[1]] <- current.mapping[proposal[2]]
        prop.mapping[proposal[2]] <- current.mapping[proposal[1]]

        ## Calculate score using new mapping
        prop.score <- CalcScore(to.decipher, prop.mapping)

        if(log(runif(1)) < p*(prop.score - current.score)) {

            ## If new mapping is accepted, update current variables
            current.mapping  <- prop.mapping
            current.score    <- prop.score
            current.decipher <- CipherText(to.decipher, current.mapping)

            ## If new score is higher than previous max score, update.
            if(current.score > max.score) {
                max.score   <- current.score
                max.mapping <- current.mapping
            }

            ## Increment number of accepted
            num.accept <- num.accept + 1
        }

        if (i %% (iterations/100) == 0) {
            cat(i, substring(current.decipher,1,57), current.score, '\n')
        }

        all.scores[i]   <- current.score
        text.history[i] <- substring(current.decipher, 1, 57)

        i <- i + 1
    }

    ret <- list(max.score = max.score,
                max.mapping = max.mapping,
                scores = all.scores,
                text.history = text.history,
                num.accept= num.accept)

    return(ret)
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
    new.test<- CipherText(test, mapping)
    ret <- list(deciphered = new.test, mapping = mapping)
    ret
}
