# garch-vix

### Summary

Modelling S&amp;P500 volatility with GARCH, GJR-GARCH, Beta-t-EGARCH, 2-Beta-t-EGARCH and comparison with the CBOE Volatility Index (VIX). We generate predictions using historical data and compare how well the different models did in comparison with each other. Mincer-Zarnowitz regression and Diebold-Mariano tests are used for evaluation. There were no sigificant difference between the four GARCH models using Mincer-Zarnowitz regressions (none were unbiased and efficient). Beta-t-EGARCH performs better for some loss functions, but none of the models significantly outperform the VIX using the Diebold-Mariano test. 

## Introduction

Since the global Covid19 pandemic broke out in the first quarter of 2020, it has changed many aspects of daily life. It poses major challenges for the healthcare system. Therefore, governments around the world are taking various measures to control the pandemic, the number of new infections and the number of deaths. Despite past experience with pandemics, this situation still poses many uncertainties, which is also reflected in the financial markets. A good example is the Cboe Volatility Index (VIX). It is a real-time market index that reflects the expectation of the market for forward-looking volatility for 30 days. It provides a measure of market risk and the feelings of investors, derived from the price inputs of the S&P 500 index options. According to Cboe (2020), a local maximum was reached on March 16, 2020 with a VIX of 82.69. This number indicates the annualized and expected standard deviation of the S&P500 return for the next month. This means that based on the VIX, the expected standard deviation of the S&P500 return for the next month is 82.69 / Sqrt(12) = 23.87. Normally, the standard deviation of the S&P500 return for the whole year is around 20%. When comparing the values, the relevance of the Covid-19 pandemic becomes apparent, as in March 2020 the expected standard deviation for one month was higher than for an average year. In most cases, the S&P500 index decreases when the VIX jumps up. 


