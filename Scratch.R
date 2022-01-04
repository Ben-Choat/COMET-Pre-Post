###12/10/2021 Ben Choat
# read in and unip files
test1 <- unzip("G:/My Drive/Choat Bhaskar/PhD/InTERFEWS/CWCB/Project/Shiny.App.COMET/ThorntonNorthProps.zip")
                 
#https://gist.github.com/aagarw30/2e1bf51a163f06c0b9106c7f53710b4e 


# shiny playing


if(!require(pacman)) {install.packages("pacman")}
library(pacman)

pacman::p_load(rhandsontable)

val.in <- seq(1, 10, 1)
DF = data.frame(val = val.in, bool = TRUE, big = 11:20, #LETTERS[1:10],
                small = val.in/2,# letters[1:10],
                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = FALSE)

DF$chart = c(sapply(1:5,
                    function(x) jsonlite::toJSON(list(values=rnorm(10),
                                                      options = list(type = "bar")))),
             sapply(1:5,
                    function(x) jsonlite::toJSON(list(values=rnorm(10),
                                                      options = list(type = "line")))))

rhandsontable(DF, rowHeaders = NULL, width = 550, height = 300) %>%
  hot_col("chart", renderer = htmlwidgets::JS("renderSparkline"))


####
#User Interface, ui ----
####
ui <- fillPage(theme = shinytheme("cerulean"), #shinythemes::themeSelector(), #theme = shinytheme("cyborg"),
               navbarPage(title = "Pre - Post - COMET",
                          
               ) #close navbarPage
) #cloes fillPage








####
#server ----
####

server <- function(input, output, session) {
  
  
  #####
  #Execute
  #####
  shinyApp(ui, server)
  




# Some scratch work related to pre- post- COMET app.

if(!require(pacman)) {install.packages("pacman")}
library(pacman)

# ggplot2 plotting
# knitr for printing tables
# pracma for inverse error function and linearly interp
# nleqslv for solving nonlinear system of equations
# plyr for round_any
pacman::p_load(ggplot2, knitr, pracma, nleqslv, plyr)

tmp.fun <- function(scc) {(56.71 * (scc/180)^0.2 * exp(-1.43 * (scc/180)^0.7))}
#plot(sapply(seq(0, 1000, 1), function(x) {tmp.fun(x)}) ~ seq(0, 1000, 1))

tmp.df <- data.frame("x" = seq(0, 1000, 1), 
                     "y" = sapply(seq(0, 1000, 1), function(x) {tmp.fun(x)}))

# function from figure Pindyck 2019, all groups
ggplot2::ggplot(data = tmp.df, 
                aes(x = seq(0, 1000, 1), 
                    y = sapply(seq(0, 1000, 1), function(x) {tmp.fun(x)}))
                ) + 
  geom_line()

#Source functions
wdir <- "I:/My Drive/GradSchool/classes/CIVE622_RiskAnalysisofWaterAndEnvSystems/"

source(paste0(wdir, "R/Risk_Uncertainty.functions.r"))


x.tmp = c(13.4110787172,
         28.5714285714,
         61.2244897959,
         43.7317784256,
         77.5510204081,
         92.7113702623,
         106.705539358,
         123.032069970,
         135.860058309,
         154.518950437,
         170.845481049,
         184.839650145,
         203.498542274,
         215.160349854,
           233.819241982,
           247.813411078,
           265.306122448,
           275.801749271,
           296.793002915,
           314.285714285,
           325.947521865,
           341.107871720,
           359.766763848,
           372.594752186,
           388.921282798,
           408.746355685,
           421.574344023,
           436.734693877,
           456.559766763,
           465.889212827,
           478.717201166,
           498.542274052,
           514.868804664,
           530.029154518,
           542.857142857,
           560.349854227,
           576.676384839,
           590.670553935,
           608.163265306,
           623.323615160,
           640.816326530,
           654.810495626,
           673.469387755,
           686.297376093,
           702.623906705,
           716.618075801,
           736.443148688,
           749.271137026,
           767.930029154,
           779.591836734,
           798.250728862,
           812.244897959,
           826.239067055,
           843.731778425,
           889.212827988,
           907.871720116,
           923.032069970,
           945.189504373,
           967.346938775,
           986.005830903,
           1000)
                     
                     ###(a)	Compu
                       
                       
y.tmp <- c(26.99186991869,
              16.9512195121,
             24.87804878048,
             14.02439024390,
             14.95934959349,
             10.0813008130,
              12.92682926829,
             6.01626016260,
              8.00813008130,
              12.9674796747,
              8.00813008130,
              7.84552845528,
              9.06504065040,
             6.01626016260,
             14.95934959349,
              6.01626016260,
             7.073170731707,
             7.03252032520,
             8.048780487804,
              10.08130081300,
             8.983739837398,
              4.105691056910,
              15.08130081300,
              8.983739837398,
              5.975609756097,
             4.146341463414,
             6.2195121951219,
              10.121951219514,
              3.0894308943085,
             5.040650406504,
             6.016260162601,
              2.1138211382113,
              4.024390243902,
              3.9837398373983,
              3.0081300813008,
              0.1626016260162,
              6.016260162601,
              4.024390243902,
              3.0894308943089,
              4.959349593495,
             1.1382113821138,
              4.065040650406,
              3.04878048780,
              1.0975609756097,
              1.0975609756097,
             2.073170731707,
             2.073170731707,
             1.0975609756097,
              2.154471544715,
              3.170731707317,
              1.2195121951219,
             2.032520325203,
              1.0162601626016,
              0.9756097560975,
              0.0406504065,
              0.0406504065,
              0.0406504065,
              0.0406504065,
             1.056910569105689)

scc.df <- data.frame(x.tmp, y.tmp)
###(a)	Compute the first three moments of the data.

#```{r}
n.tmp.df <- length(tmp.df$y)
m0 <- mean(tmp.df$y)
m1 <- var(tmp.df$y)
m2 <- skewn.coef.fun(tmp.df$y, n.tmp.df, m0, sd(tmp.df$y))

kable(data.frame("Moment" = c("mean", "var", "skewn. coef"), "Value" = round(c(m0, m1, m2), 3)))
#```


###Working data is tmp.df. df[x, y]

###(b)	Compute the first three probability weighted moments (B_r) of the data.

#```{r}
b0.pwm <- PWM.fun(x = tmp.df$y, r = 0)
b1.pwm <- PWM.fun(x = tmp.df$y, r = 1)
b2.pwm <- PWM.fun(x = tmp.df$y, r = 2)

kable(data.frame("r" = c(0, 1, 2), "PWMs" = round(c(b0.pwm, b1.pwm, b2.pwm), 3)))
#```

###(c)	Using the Method of Moments and Probability Weighted Moments (PWM), fit normal, lognormal, gamma, and exponential distributions to the data.

#```{r}
#normal
#MOM
mu.norm.mom <- m0
sd.norm.mom <- sd(tmp.df$y)

#pwm
b0.norm.pwm <- m0
b1.norm.pwm <- sqrt(pi) * (2*b1.pwm - b0.pwm)

nrml.df <- data.frame("Distr" = rep("norm", 4), "Method" = c("MOM", "MOM", "PWM", "PWM"), 
                      "Param" = c("mu", "sd", "mu", "sd"), 
                      "Est" = round(c(mu.norm.mom, sd.norm.mom, b0.norm.pwm, b1.norm.pwm), 3))
#lognormal
#mom
w.lnorm.mom <- sqrt(log(1 + (sd(tmp.df$y)/m0)^2))
th.lnorm.mom <- log(m0) - 0.5*w.lnorm.mom^2

#pwm
w.lnorm.pwm <- 2 * erfinv((2*b1.pwm - b0.pwm)/b0.pwm)
th.lnorm.pwm <- log(b0.pwm) - w.lnorm.pwm^2/2

logn.df <- data.frame("Distr" = rep("logn", 4), "Method" = c("MOM", "MOM", "PWM", "PWM"), 
                      "Param" = c("w", "th", "w", "th"), 
                      "Est" = round(c(w.lnorm.mom, th.lnorm.mom, w.lnorm.pwm, th.lnorm.pwm), 3))
#gamma
#mom
skew.cor <- m2 * (1 + 8.5/n.tmp.df)
B.gamma.mom <- (2/skew.cor)^2
alpha.gamma.mom <- sd(tmp.df$y) * skew.cor/2
x0.gamma.mom <- m0 - alpha.gamma.mom*B.gamma.mom

#pwm
#Solve system of equations using library nleqslv
x.start <- c(9, 4)
fn.eqs <- function(x) {
  #alpha = x[1], beta = x[2]
  y <- numeric(2)
  y[1] <- b0.pwm/x[2] - x[1]
  y[2] <- (sqrt(pi) * (2*b1.pwm - b0.pwm) * gamma(x[2]))/gamma(x[2] + 0.5) - x[1]
  y
}
alpha.gamma.tmp <- nleqslv(x.start, fn.eqs)
alpha.gamma.pwm <- alpha.gamma.tmp$x[1]
B.gamma.pwm <- alpha.gamma.tmp$x[2]
x0.gamma.pwm <- b0.pwm - alpha.gamma.pwm * B.gamma.pwm

gamma.df <- data.frame("Distr" = rep("gamma", 6), 
                       "Method" = c("MOM", "MOM", "MOM", "PWM", "PWM", "PWM"), 
                       "Param" = c("alpha", "B", "x0", "alpha", "B", "x0"), 
                       "Est" = round(c(alpha.gamma.mom, B.gamma.mom, x0.gamma.mom, 
                                       alpha.gamma.pwm, B.gamma.pwm, x0.gamma.pwm), 3))

#exponential
#mom
alpha.exp.mom <- sd(tmp.df$y)
x0.exp.mom <- m0 - sd(tmp.df$y)

#pwm
b0.pwm.exp <- PWM.fun(tmp.df$y, 0, exp.TF = TRUE)
b1.pwm.exp <- PWM.fun(tmp.df$y, 1, exp.TF = TRUE)
alpha.exp.pwm <- 2 * (b0.pwm.exp - 2*b1.pwm.exp)
x0.exp.pwm <- b0.pwm.exp - alpha.exp.pwm

exp.df <- data.frame("Distr" = rep("exp", 4), "Method" = c("MOM", "MOM", "PWM", "PWM"), 
                     "Param" = c("alpha", "x0", "alpha", "x0"), 
                     "Est" = round(c(alpha.exp.mom, x0.exp.mom, alpha.exp.pwm, x0.exp.pwm), 3))

all.est.df <- rbind(nrml.df, logn.df, gamma.df, exp.df)
kable(all.est.df)
#```

###(d)	Using graphical assessment (CDF plots) and the chi-square goodness-of-fit test, comment on the agreement between the data and the fitted distributions. Which distributions is the best fitted distribution? 

#```{r fig.width=7, fig.height=7,warning=FALSE}
#ecdf
ecdf.fun(tmp.df$y, plot.out = TRUE)
ecdf.tmp.df <- cdf.out[!duplicated(cdf.out),]

#normal ecdfs
norm.cdf.mom <- pnorm(ecdf.tmp.df$xs, mean = mu.norm.mom, sd = sd.norm.mom)
norm.cdf.pwm <- pnorm(ecdf.tmp.df$xs, mean = b0.norm.pwm, sd = b1.norm.pwm)
#lognormal cdfs  
logn.cdf.mom <- plnorm(ecdf.tmp.df$xs, meanlog = th.lnorm.mom, sdlog = w.lnorm.mom)
logn.cdf.pwm <- plnorm(ecdf.tmp.df$xs, meanlog = th.lnorm.pwm, sdlog = w.lnorm.pwm)
#gamma cdfs
gamma.cdf.mom <- pgamma((ecdf.tmp.df$xs - x0.gamma.mom), shape = B.gamma.mom, scale = alpha.gamma.mom)
gamma.cdf.pwm <- pgamma((ecdf.tmp.df$xs - x0.gamma.pwm), shape = B.gamma.pwm, scale = alpha.gamma.pwm)
#exp cdfs
exp.cdf.mom <- pexp((ecdf.tmp.df$xs - x0.exp.mom), rate = 1/alpha.exp.mom)
exp.cdf.pwm <- pexp((ecdf.tmp.df$xs - x0.exp.pwm), rate = 1/alpha.exp.pwm)

#plots
par(mfrow = c(2,2))
#normal
plot(ecdf.tmp.df$xs, ecdf.tmp.df$Fx, type = "b",
     lty = "dashed", pch = 1, main = "Normal Distr.", 
     ylab = "F(x)", xlab = "tmp.df$y [mg/L]")
lines(ecdf.tmp.df$xs, norm.cdf.mom, type = "l", 
      lty = "dotdash", col = "blue", lwd = 2)
lines(ecdf.tmp.df$xs, norm.cdf.pwm, type = "l", 
      lty = "dashed", col = "red", lwd = 2)
legend("bottomright", legend=c("eCDF", "MOM", "PWM"), 
       col = c("black", "blue", "red"), pch = c(1, NA, NA), 
       lty = c("solid", "dotdash", "dashed"), lwd = c(1,2,2))  
#lognormal
plot(ecdf.tmp.df$xs, ecdf.tmp.df$Fx, type = "b",
     lty = "dashed", pch = 1, main = "LogNormal Distr.", 
     ylab = "F(x)", xlab = "tmp.df$y [mg/L]")
lines(ecdf.tmp.df$xs, logn.cdf.mom, type = "l", 
      lty = "dotdash", col = "blue", lwd = 2)
lines(ecdf.tmp.df$xs, logn.cdf.pwm, type = "l", 
      lty = "dashed", col = "red", lwd = 2)
legend("bottomright", legend=c("eCDF", "MOM", "PWM"), 
       col = c("black", "blue", "red"), pch = c(1, NA, NA), 
       lty = c("solid", "dotdash", "dashed"), lwd = c(1,2,2)) 
#lognormal
plot(ecdf.tmp.df$xs, ecdf.tmp.df$Fx, type = "b",
     lty = "dashed", pch = 1, main = "Gamma Distr.", 
     ylab = "F(x)", xlab = "tmp.df$y [mg/L]")
lines(ecdf.tmp.df$xs, gamma.cdf.mom, type = "l", 
      lty = "dotdash", col = "blue", lwd = 2)
lines(ecdf.tmp.df$xs, gamma.cdf.pwm, type = "l", 
      lty = "dashed", col = "red", lwd = 2)
legend("bottomright", legend=c("eCDF", "MOM", "PWM"), 
       col = c("black", "blue", "red"), pch = c(1, NA, NA), 
       lty = c("solid", "dotdash", "dashed"), lwd = c(1,2,2)) 
#exponential
plot(ecdf.tmp.df$xs, ecdf.tmp.df$Fx, type = "b",
     lty = "dashed", pch = 1, main = "Exponential Distr.", 
     ylab = "F(x)", xlab = "tmp.df$y [mg/L]")
lines(ecdf.tmp.df$xs, exp.cdf.mom, type = "l", 
      lty = "dotdash", col = "blue", lwd = 2)
lines(ecdf.tmp.df$xs, exp.cdf.pwm, type = "l", 
      lty = "dashed", col = "red", lwd = 2)
legend("bottomright", legend=c("eCDF", "MOM", "PWM"), 
       col = c("black", "blue", "red"), pch = c(1, NA, NA), 
       lty = c("solid", "dotdash", "dashed"), lwd = c(1,2,2)) 

#Chi-squared
#define ecdfs
norm.prob.mom <- c()
norm.prob.pwm <- c()
logn.prob.mom <- c()
logn.prob.pwm <- c()
gamma.prob.mom <- c()
gamma.prob.pwm <- c()
exp.prob.mom <- c()
exp.prob.pwm <- c()
ecdf.prob  <- c()
#Define number of breaks for test
n.breaks <- nbins.fun(nrow(ecdf.tmp.df)) * 2# + 1

for(j in 1:nrow(ecdf.tmp.df)) {
  if(j == 1) {
    V1.norm.mom <- V1.norm.pwm <- V1.logn.mom <- V1.logn.pwm <- 0
    V1.gamma.mom <- V1.gamma.pwm <- V1.exp.mom <- V1.exp.pwm <- 0
    V1.ecdf <- 0
  } else {
    V1.norm.mom <- norm.cdf.mom[j - 1]
    V1.norm.pwm <- norm.cdf.pwm[j - 1]
    V1.logn.mom <- logn.cdf.mom[j - 1]
    V1.logn.pwm <- logn.cdf.pwm[j - 1]
    V1.gamma.mom <- gamma.cdf.mom[j - 1]
    V1.gamma.pwm <- gamma.cdf.pwm[j - 1]
    V1.exp.mom <- exp.cdf.mom[j - 1]
    V1.exp.pwm <- exp.cdf.pwm[j - 1]
    V1.tmp.df.ecdf <- ecdf.tmp.df$Fx[j - 1]
  }
  
  norm.prob.mom <- c(norm.prob.mom, norm.cdf.mom[j] - V1.norm.mom)
  norm.prob.pwm <- c(norm.prob.pwm, norm.cdf.pwm[j] - V1.norm.pwm)
  logn.prob.mom <- c(logn.prob.mom, logn.cdf.mom[j] - V1.logn.mom)
  logn.prob.pwm <- c(logn.prob.pwm, logn.cdf.pwm[j] - V1.logn.pwm)
  gamma.prob.mom <- c(gamma.prob.mom, gamma.cdf.mom[j] - V1.gamma.mom)
  gamma.prob.pwm <- c(gamma.prob.pwm, gamma.cdf.pwm[j] - V1.gamma.pwm)
  exp.prob.mom <- c(exp.prob.mom, exp.cdf.mom[j] - V1.exp.mom)
  exp.prob.pwm <- c(exp.prob.pwm, exp.cdf.pwm[j] - V1.exp.pwm)
  ecdf.prob <- c(ecdf.prob, ecdf.tmp.df$Fx[j] - V1.ecdf)
}
#Define densities
pnorm.mom.vctr <- as.vector(table(cut(norm.prob.mom, seq(0, max(norm.prob.mom), length.out = n.breaks))))
pnorm.pwm.vctr <- as.vector(table(cut(norm.prob.pwm, seq(0, max(norm.prob.pwm), length.out = n.breaks))))
plogn.mom.vctr <- as.vector(table(cut(logn.prob.mom, seq(0, max(logn.prob.mom), length.out = n.breaks))))
plogn.pwm.vctr <- as.vector(table(cut(logn.prob.pwm, seq(0, max(logn.prob.pwm), length.out = n.breaks))))
pgamma.mom.vctr <- as.vector(table(cut(gamma.prob.mom, seq(0, max(gamma.prob.mom), length.out = n.breaks))))
pgamma.pwm.vctr <- as.vector(table(cut(gamma.prob.pwm, seq(0, max(gamma.prob.pwm), length.out = n.breaks))))
pexp.mom.vctr <- as.vector(table(cut(exp.prob.mom, seq(0, max(exp.prob.mom), length.out = n.breaks))))
pexp.pwm.vctr <- as.vector(table(cut(exp.prob.pwm, seq(0, max(exp.prob.pwm), length.out = n.breaks))))
pecdf.vctr <- as.vector(table(cut(ecdf.prob, seq(0, max(ecdf.prob), length.out = n.breaks))))

norm.mom.x2p <- chisq.test(pecdf.vctr, pnorm.mom.vctr)$p.value
norm.pwm.x2p <- chisq.test(pecdf.vctr, pnorm.pwm.vctr)$p.value
logn.mom.x2p <- chisq.test(pecdf.vctr, plogn.mom.vctr)$p.value
logn.pwm.x2p <- chisq.test(pecdf.vctr, plogn.pwm.vctr)$p.value
gamma.mom.x2p <- chisq.test(pecdf.vctr, pgamma.mom.vctr)$p.value
gamma.pwm.x2p <- chisq.test(pecdf.vctr, pgamma.pwm.vctr)$p.value
exp.mom.x2p <- chisq.test(pecdf.vctr, pexp.mom.vctr)$p.value
exp.pwm.x2p <- chisq.test(pecdf.vctr, pexp.pwm.vctr)$p.value

kable(data.frame("Distribution" = c(rep("norm", 2), rep("logn", 2), rep("gamma", 2), rep("exp", 2)),
                 "FitMethod" = rep(c("MOM", "PWM"), 4), 
                 "p" = round(c(norm.mom.x2p, norm.pwm.x2p, logn.mom.x2p, logn.pwm.x2p,
                               gamma.mom.x2p, gamma.pwm.x2p, exp.mom.x2p, exp.pwm.x2p), 4)))
#```





