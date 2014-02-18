displayDigit <- function (im) {

  image(1:14, 1:14, t(apply(matrix(im,14,14,byrow=TRUE), 2, rev)),
        col=gray(seq(0,1,length=100)),
        zlim=c(0,1))

}
