#Load libraries
require(stats)
library("here")

#Set current directory as working directory
setwd(here())

#Function returns Discrete Fourier Transform sample frequencies.
#The sample frequencies are defined according to the Scipy definiton.
#https://docs.scipy.org/doc/scipy/reference/generated/scipy.fftpack.fftfreq.html
fftfreq <- function(n, d) {
    #If n is even number (even number of signal points)
    if (n %% 2 == 0) {
        init_f <- seq(0, n / 2 - 1, by = 1) / (n * d)
        f <- append(init_f, - (n / 2) / (d * n), after = length(init_f))
        neg_f <- seq(- n / 2 + 1, -1, by = 1) / (n * d)
        f <- append(f, neg_f, length(f))
    }
    #If n is odd number (odd number of signal points)
    else {
        init_f <- seq(0, (n - 1) / 2, by = 1) / (n * d)
        f <- append(init_f, - ((n - 1) / 2) / (d * n), after = length(init_f))
        neg_f <- seq(- (n - 1) / 2 + 1, -1, by = 1) / (n * d)
        f <- append(f, neg_f, length(f))
    }
    return(f)
}

#Domain setup
tmax <- 0.74875 #maximum time value
dt <- 0.00125 #s
t <- seq(0, tmax, by = dt) #also try ts function
n <- length(t) #length of time vector t

#Create signal
y <- sin(50.0 * 2.0 * pi * t) + 0.5 * sin(80.0 * 2.0 * pi * t)

#Create sample frequencies
f <- fftfreq(n, dt)

#Fourier transform
fft_y <- fft(y)
mag <- sqrt(Re(fft_y)^2 + Im(fft_y)^2) * 2 / n

#Print frequency and amplitude values
cat("Frequencies: ", f[1 : (n / 2)], "\n", sep = "\t")
cat("Amplitudes: ", mag[1 : (n / 2)], "\n", sep = "\t")

# Writing frequencies and amplitudes data to a tsv file
frequencies <- as.matrix(f[1 : (n / 2)])
amplitudes <- as.matrix(mag[1 : (n / 2)])
results <- cbind(frequencies, amplitudes)
write.table(results, file = "R_results.txt", row.names = FALSE,
    col.names = FALSE)

#Opening the graphical device
pdf("fft.pdf")

#Plotting
layout(matrix(c(1, 2), 2, 1, byrow = TRUE))
plot(t, y, xlab = "Time t [s]", ylab = "y", type = "l")
#plot(f[seq(1, length(f), by = 1) / 2],
#    mag[seq(1, length(f), by = 1) / 2], xlab = "Frequency, Hz",
#        ylab = "Amplitude", xlim = c(0, 1), ylim = c(0, 1), type = "l")
plot(f[1 : (n / 2)],
    mag[1 : (n / 2)], xlab = "Frequency f [Hz]",
        ylab = "Amplitude A", type = "l")

#Closing the graphical device
dev.off()