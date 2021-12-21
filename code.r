---
title: "457 project R code"
output:
  pdf_document: default
  html_notebook: default
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Cmd+Shift+Enter*. 

```{r}
library(astsa)
library(forecast)
#par(mfrow = c(2,1))
rain = read.csv(file = "~/Desktop/half.csv")
rain_time = ts(rain$total_rainfall, start = c(2001,01), frequency = 12)
rain_time
plot.ts(rain_time, main = "Rainfall in Singapore",ylab="rainfall (mm)")
rain_decompose = decompose(rain_time)
plot(rain_decompose)
```



```{r}
acf(rain_time, lag.max = 50)
pacf(rain_time, lag.max = 50)
```


```{r}
tsplot(sqrt(rain_time), main="square root transformation of rainfall",ylab = "rainfall (mm)")
tsplot(log(rain_time), main = "log transformation of rainfall",ylab = "rainfall (mm)")
```

```{r}
tsplot(diff(sqrt(rain_time),12),main = "Seasonal difference of the rainfall", 
       ylab = "rainfall (mm)")
```

```{r}
library(tseries)
adf.test(rain_time)
## test stationary after removing seasonality.
adf.test(diff(sqrt(rain_time),12))
```

```{r}
library(knitr)
aic_table <- function(data,P,Q,s,a){
    table <- matrix(NA,(P+1),(Q+1))
    for(p in 0:P){
        for(q in 0:Q){
            table[p+1,q+1] <- sarima(data, p,0,q,s,1,a,12, model = FALSE, details = FALSE)$AIC
        }
    }
    dimnames(table) <- list(paste("AR", 0:P, sep = ""),paste("MA",0:Q, sep = ""))
    table
}

table1 = aic_table(sqrt(rain_time),3,3,0,1)
require(knitr) 
kable(table1,digits=4, caption = "SARIMA(p,0,q) ×(0,1,1)12  model")
```




```{r}
acf(diff(sqrt(rain_time),12), lag.max = 70)
pacf(diff(sqrt(rain_time),12), lag.max = 70)
```

```{r}
## another sarima model for time series.
sarima(sqrt(rain_time), p=1,d=0,q=1,P=0,D=1,Q=1,S=12)
```


```{r}
## the best sarima model for time series.
sarima(sqrt(rain_time), p=0,d=0,q=2,P=0,D=1,Q=1,S=12)
```


\textbf{Forecasting for time series.}

```{r}
rainfall_mm = rain_time
PRED = sarima.for(sqrt(rainfall_mm),10,0,0,2,0,1,1,12)$pred
SE = sarima.for(sqrt(rainfall_mm),10,0,0,2,0,1,1,12, plot = FALSE)$se

rainfallPRED = PRED^2
rainfallL = (PRED-2*SE)^2
rainfallU = (PRED+2*SE)^2

plot(rainfall_mm)
points(rainfall_mm)
points(rainfallPRED, col = "red")
lines(rainfallPRED, col = "red")
lines(rainfallL, col = "blue")
lines(rainfallU, col = "blue")
```



Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Cmd+Option+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Cmd+Shift+K* to preview the HTML file). 

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.

