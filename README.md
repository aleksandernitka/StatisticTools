# StatisticTools
Some helpful functions for the PhD data analysis.

## Outlier labeling - `outlierlbl.R`
Simple function which takes in a vector of DVs and performs: 
(1) normality check using Shapiro-Wilk method (*W*) and 
(2) outlier labelling using method proposed by Hoaglin, Iglewicz and Tukey (1986), default outlier demarcation criterion of 2.2 as argued in Hoaglin and Iglewicz (1987). 

*David C. Hoaglin, Boris Iglewicz & John W. Tukey (1986) Performance of Some Resistant Rules for Outlier Labeling, Journal of the American Statistical Association, 81:396, 991-999, DOI: 10.1080/01621459.1986.10478363*
*Hoaglin, D., & Iglewicz, B. (1987) Fine-Tuning Some Resistant Rules for Outlier Labeling. Journal of the American Statistical Association, 82(400), 1147-1149. doi:10.2307/2289392*

