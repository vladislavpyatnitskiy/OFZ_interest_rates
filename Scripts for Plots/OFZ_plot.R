lapply(c("rvest", "timeSeries"), require, character.only = T) # Libs

cbr_data_plot <- function(s,e){
  
  L <- sprintf(
    paste("https://www.cbr.ru/eng/hd_base/zcyc_params/", "?UniDbQuery.Posted=",
          "True&UniDbQuery.From=%s&UniDbQuery.To=%s", sep = ""), s, e)
  
  B <- read_html(L) %>% html_nodes('table') %>% html_nodes('td') %>%
    html_text()
  
  v <- cbind.data.frame(
    B[seq(from = 1, to = length(B), by = 13)],
    B[seq(from = 2, to = length(B), by = 13)],
    B[seq(from = 3, to = length(B), by = 13)],
    B[seq(from = 4, to = length(B), by = 13)],
    B[seq(from = 5, to = length(B), by = 13)],
    B[seq(from = 6, to = length(B), by = 13)],
    B[seq(from = 7, to = length(B), by = 13)],
    B[seq(from = 8, to = length(B), by = 13)],
    B[seq(from = 9, to = length(B), by = 13)],
    B[seq(from = 10, to = length(B), by = 13)],
    B[seq(from = 11, to = length(B), by = 13)],
    B[seq(from = 12, to = length(B), by = 13)],
    B[seq(from = 13, to = length(B), by = 13)]
    )
  
  colnames(v) <- c(
    "Date","3M","6M","9M","1Y","2Y","3Y","5Y","7Y","10Y","15Y","20Y","30Y")
  
  v$Date <- as.Date(v$Date, format = "%d.%m.%Y")
  
  v <- v[order(v$Date, decreasing = F), ]
  
  rownames(v) <- v[,1]
  
  v <- v[,-1]
  
  for (n in 1:ncol(v)){ v[,n] <- as.numeric(v[,n]) }
  
  v <- as.timeSeries(v)
  
  plot(
    v[,1],
    ylim = c(min(v), max(v)),
    lty = 1,
    type = "l",
    lwd = 2,
    las = 1,
    xlab = "Trading Days",
    ylab = "Returns (%)",
    main = "Performance of Russian Government Bonds"
  )
  
  axis(side = 4, las = 2) # Right Y-Axis Values
  
  grid(nx = 1, ny = NULL, lty = 3, col = "grey") # Horizontal lines
  
  for (n in 2:(ncol(v))){ lines(v[,n], col = n, lwd = 2) } # Plot indices
  
  legend(
    x = "bottom",
    inset = c(0, -.37),
    legend = colnames(v),
    xpd = T,
    col = seq(ncol(v)),
    lwd = 2,
    cex = .5,
    bty = "n",
    horiz = F,
    ncol = 6,
    seg.len = 0.8,
    y.intersp = 0.25
  )
  
  on.exit(par(par(no.readonly = T))) # Show legend with names
  
  par(mar = c(10, 6, 6, 6)) # Define borders of the plot
}
cbr_data_plot("25.09.2024", "25.09.2025")
