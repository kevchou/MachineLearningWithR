source('functions.R')

## Set up War and Peace for reference text
readfile <- readLines('warandpeace.txt')            # Read in War and Peace text file
training <- toupper(paste(readfile, collapse=' '))  # Collapses it to one line
training <- CleanText(training)

## Creates a table of number of times (row letter) is followed by (column letter)
let <- unlist(strsplit(training, ''))
let2 <- embed(let, 2)
mat.count <- table( let2[,2], let2[,1] )
pair.count <- melt(mat.count)

## Divides each number in a row by its row sum. Frequency
mat.trans <- sweep( mat.count, 1, rowSums(mat.count), FUN='/')

## Plots
tab <- melt(mat.trans)
names(tab) <- c('cond1', 'prob2', 'prob')
ggplot(tab, aes(cond1,prob2)) +
    geom_tile(aes(fill=prob)) +
    scale_fill_gradient(low="white", high="black", limits=c(0,1)) +
    labs(title="Using \'War and Peace\'", x="Conditional on first letter",
         y="Probability of second letter", prob="Probability")+
    coord_equal()


## Reading Flatline into R
x <- readLines('flatland.txt')          # Loads Flatland into R
x <- toupper(paste(x, collapse=' '))    # Capitalizes everything
x <- CleanText(x)                        # Make sure text is in proper format
x <- substring(x, 2, nchar(x))          # Removes first character


starting.map <- sample(toupper(letters)) # Random letter mapping to start with

encrypted.text <- CipherText(x, starting.map, decipher = FALSE)

decrypted.text1 <- RunMcmcDecipher(encrypted.text, p = 0.1, iterations = 5000)
decrypted.text2 <- RunMcmcDecipher(encrypted.text, p = 1,   iterations = 5000)
decrypted.text3 <- RunMcmcDecipher(encrypted.text, p = 10,  iterations = 5000)


everything <- rbind(
    data.frame(p = "p=0.1", iteration = c(1:5000), score = decrypted.text1$scores),
    data.frame(p = "p=1",   iteration = c(1:5000), score = decrypted.text2$scores),
    data.frame(p = "p=10",  iteration = c(1:5000), score = decrypted.text3$scores))
