require(ggplot2)

source('functions.R')
source('display-image.R')

## Import Data -----------------------------------------------------------------
images = as.matrix(read.table('zip-images.txt'))
labels = read.table('zip-labels.txt')
rownames(images) = labels[,1]

labels[1,1]
displayDigit(images[3,])


################################################################################
##  Using NN for dimensionality reduction
################################################################################
mlp <- TrainNn(y=images, X=images, m=c(10, 2, 10), eta=0.0002, iters=10000)

forward.prop <- ForwardPropagate(images, mlp$W)

errordf <- data.frame(iteration = c(1:length(mlp$E)), error = mlp$E)
ggplot(errordf, aes(x=iteration, y=error)) +
    geom_line()


fpdf <- data.frame(x=forward.prop$h2[,1], y=forward.prop$h2[,2], digit = factor(labels[,1]))
ggplot(fpdf, aes(x=x, y=y, label = digit, colour = digit)) +
    geom_text() +
    theme(legend.position="none")


## Attempt to reconstruct
par(mfrow=c(1,2))
displayDigit(images[7,])
displayDigit(forward.prop$out[7,])
par(mfrow=c(1,1))



################################################################################
##  Neural Network as Classifier
################################################################################

## Creates output matrix. Each row is a Zero vector of length 10, where the
## (i+1)th position is 1 if the label is 'i'
CreateLabelVector <- function(label) {

    label.vector <- rep(0, 10)
    label.vector[label + 1] <- 1

    return(label.vector)
}

labels.mat <- t(apply(labels, 1, FUN = CreateLabelVector))
colnames(labels.mat) <- paste(0:9)

head(labels.mat)



## Divide data into training and test sets
train.idx <- sample(nrow(images), floor(0.80 * nrow(images)))

train.x <- images[train.idx, ]
train.y <- labels.mat[train.idx, ]

test.x <- images[-train.idx, ]
test.y <- labels.mat[-train.idx, ]

## Train NN
nn  <- TrainNn(train.y, train.x, c(15, 15, 15), eta = 0.00002, iters = 20000)

## Error plot
error.df <- data.frame(iterations = c(1:length(nn$E)), error = nn$E)
ggplot(error.df, aes(x=iterations, y=error)) + geom_line()

## Get classified values
estimated <- ForwardPropagate(test.x, nn$W)$out # Forward Propagation with
                                                # estimated weights on test set

GetEstimatedLabel <- function(est) {
    ## Takes estimated classifications and returns digit label
    which.max(est) - 1
}

## Gets estimated labels from the output of the neural network
estimated.digit <- apply(estimated, 1, FUN = GetEstimatedLabel)

## True and predicted labels in same matrix
final.classified.vals <- cbind(True = labels[-train.idx, 1],
                                 Predicted = estimated.digit)

head(final.classified.vals)


## Accuracy rate
sum(final.classified.vals[, 1] == final.classified.vals[, 2]) / nrow(final.classified.vals)
