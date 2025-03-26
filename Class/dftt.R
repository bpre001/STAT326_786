# Original signal

x <- 0:11 * pi / 4
x <- cos(x)

# Compute DFT
X <- fft(x)

# Create a data frame for plotting
df <- data.frame(
  k = 0:(length(X) - 1),
  Re = Re(X),
  Im = Im(X),
  Magnitude = Mod(X),
  Phase = Arg(X)
)

# Load ggplot2 for visualization
library(ggplot2)
library(gridExtra)

# Plot real and imaginary parts
p1 <- ggplot(df, aes(x = k)) +
  geom_segment(aes(xend = k, y = 0, yend = Re), color = "blue", lwd = 1.2) +
  geom_point(aes(y = Re), color = "blue", size = 3) +
  ggtitle("Real Part of DFT") +
  ylab("Re(X)") +
  xlab("Frequency k") +
  theme_minimal()

p2 <- ggplot(df, aes(x = k)) +
  geom_segment(aes(xend = k, y = 0, yend = Im), color = "red", lwd = 1.2) +
  geom_point(aes(y = Im), color = "red", size = 3) +
  ggtitle("Imaginary Part of DFT") +
  ylab("Im(X)") +
  xlab("Frequency k") +
  theme_minimal()

# Plot magnitude and phase
p3 <- ggplot(df, aes(x = k, y = Magnitude)) +
  geom_col(fill = "purple", width = 0.5) +
  ggtitle("Magnitude Spectrum") +
  ylab("|X(k)|") +
  xlab("Frequency k") +
  theme_minimal()

p4 <- ggplot(df, aes(x = k, y = Phase)) +
  geom_col(fill = "orange", width = 0.5) +
  ggtitle("Phase Spectrum") +
  ylab("arg(X(k)) (radians)") +
  xlab("Frequency k") +
  theme_minimal()

# Arrange plots
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Set signal length
N <- 12
n <- 0:(N-1)

# Create real parts of DFT basis functions for k = 0, 1, 2, 3
basis <- data.frame(
  n = rep(n, times = 4),
  k = factor(rep(0:3, each = N)),
  value = c(
    cos(2*pi*0*n/N),   # DC (flat line)
    cos(2*pi*1*n/N),   # 1 cycle over 4 samples
    cos(2*pi*2*n/N),   # 2 cycles (alternating)
    cos(2*pi*3*n/N)    # negative frequency (mirror of k = 1)
  )
)

# Plot
ggplot(basis, aes(x = n, y = value)) +
  geom_line(group = 1, size = 1.2, color = "blue") +
  geom_point(size = 3, color = "blue") +
  facet_wrap(~k, ncol = 2, labeller = label_bquote(k==.(as.integer(k)))) +
  theme_minimal(base_size = 14) +
  labs(title = "Real Part of DFT Basis Functions (N = 4)",
       x = "Sample (n)",
       y = "Amplitude")
