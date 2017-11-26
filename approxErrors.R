writeDatf <- function(){
        p0 <- function(x){1}
        p2 <- function(x){1-x^2/2}
        p4 <- function(x){1-x^2/2+x^4/24}
        p6 <- function(x){1-x^2/2+x^4/24-x^6/720}
        p8 <- function(x){1-x^2/2+x^4/24-x^6/720+x^8/factorial(8)}
        
        
        xs <- seq(from=-2*pi, to=2*pi, length.out=120)
        
        p0e <- round(abs(cos(xs) - p0(xs)), digits=12)
        p2e <- round(abs(cos(xs) - p2(xs)), digits=12)
        p4e <- round(abs(cos(xs) - p4(xs)), digits=12)
        p6e <- round(abs(cos(xs) - p6(xs)), digits=12)
        p8e <- round(abs(cos(xs) - p8(xs)), digits=12)
        
        datf <- data.frame(round(xs,digits=2), p0e, p2e, p4e, p6e, p8e)
        
        names(datf) <- c("x", "p0_error", "p2_error","p4_error", "p6_error", "p8_error")
        
        write.table(datf, "./datf.csv", sep=",")
}