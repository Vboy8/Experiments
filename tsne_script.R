#watch out for the labels parameter in epc
#text(x, labels=as.vector(mydata$"V5"))
#mydata[,ncol(mydata)]


#initially input this into console
mydata <- read.table("iris.data", sep=",")
library(tsne)

#later input this into console
x <- 0
epc <- function(x) {
    x <<- x + 1
    filename <- paste("plot", x, "jpg", sep=".")
    cat("> Plotting TSNE to ", filename, " ")

    # plot to d:\\plot.x.jpg file of 2400x1800 dimension
    jpeg(filename, width=240, height=180)

    plot(x, t='n', main="T-SNE")
    text(x, labels=as.vector(mydata$"V4027"))
    dev.off()
}

# run tsne (maximum iterations:500, callback every 100 epochs, target dimension k=5)
tsne_data <- tsne(mydata[,1:(ncol(mydata)-1)], k=5, epoch_callback=epc, max_iter=500, epoch=100)



"""
Rtsne
"""

# load the Rtsne package
library(Rtsne)

# run Rtsne with default parameters
rtsne_out <- Rtsne(as.matrix(mydata[,1:(ncol(mydata) - 1)]))

# plot the output of Rtsne into d:\\barneshutplot.jpg file of 2400x1800 dimension
jpeg("barneshutplot.jpg", width=2400, height=1800)
plot(rtsne_out$Y, t='n', main="BarnesHutSNE")
text(rtsne_out$Y, labels=as.vector(mydata$"V12601"))


"""
This works the best!!! Don't forget the dev.off function
"""

rtsne_out <- Rtsne(as.matrix(mydata[,(1:(ncol(mydata)-1))]))
print(rtsne_out$Y)
# plot the output of Rtsne into d:\\barneshutplot.jpg file of 2400x1800 dimension
jpeg("barneshutplot.jpg", width=2400, height=1800)
plot(rtsne_out$Y, t='n', main="BarnesHutSNE")
text(rtsne_out$Y, labels=as.vector(mydata$"V12601"))
dev.off()

"""
Experimenting with color
"""
mydata <- read.table("centralNervousSystem_outcome.data", sep=",")
library(Rtsne)

rtsne_out <- Rtsne(as.matrix(mydata[,(1:(ncol(mydata)-1))]), perplexity=20)
jpeg("barneshutplot.jpg", width=720, height=540)
plot(rtsne_out$Y, col=mydata$"V7130", pch=16, cex=2.5)
title(main="BarnesHutSNE")
dev.off()