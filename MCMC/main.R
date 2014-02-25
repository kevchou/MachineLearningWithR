source('functions.R')

x <- readLines('flatland.txt')          # Loads Flatland into R
x <- toupper(paste(x, collapse=' '))    # Capitalizes everything
x <- textonly(x)                        # Make sure text is in proper format
x <- substring(x, 2, nchar(x))

# SET UP
period = 1                              # Number of different ciphers to use
starting.map <- create.map(period)      # Create a cipher map
y <- decipher2(x, starting.map, period=period, decipher=F) # Encrypts text

## Start MCMC
decrypted.text <- run.mcmc.decipher.random(y, period=period, p=1, iterations=5000)
