# finding parameter set with TotalHIV closest to mean

TotalHIV <- read_output("TotalHIV")
mean(TotalHIV$`1985`)
# 259 was closest
mean(TotalHIV$`1986`)
# 100 was closest
mean(TotalHIV$`1987`)
# 997 was closest
mean(TotalHIV$`1988`)
# 777
mean(TotalHIV$`1989`)
# 407
mean(TotalHIV$`1990`)
# 675
mean(TotalHIV$`1991`)
# 26
mean(TotalHIV$`1992`)
# 463
mean(TotalHIV$`1993`)
# 452
mean(TotalHIV$`1994`)
# 288
mean(TotalHIV$`1995`)
# 943
mean(TotalHIV$`1996`)
# 242 or 713
mean(TotalHIV$`1997`)
# 184
mean(TotalHIV$`1998`)
# 773
mean(TotalHIV$`1999`)
# 152
mean(TotalHIV$`2000`)
# 846
mean(TotalHIV$`2001`)
# 75, 167, 185, 190, 225, 264, 265, 282, 319, 421, 422, 564, 622, 678, 723, 967
mean(TotalHIV$`2002`)
# 792
mean(TotalHIV$`2003`)
# 49
mean(TotalHIV$`2004`)
# 590
mean(TotalHIV$`2005`)
# 138
mean(TotalHIV$`2006`)
# 450
mean(TotalHIV$`2007`)
# 96 or 245
mean(TotalHIV$`2008`)
# 458
mean(TotalHIV$`2009`)
# 537
mean(TotalHIV$`2010`)
# 152
mean(TotalHIV$`2011`)
# 97, 113, 885
mean(TotalHIV$`2012`)
# 452, 588
mean(TotalHIV$`2013`)
# 283, 301, 708
mean(TotalHIV$`2014`)
# 899
mean(TotalHIV$`2015`)
# 362
mean(TotalHIV$`2016`)
# 705
mean(TotalHIV$`2017`)
# 338
mean(TotalHIV$`2018`)
# 681
mean(TotalHIV$`2019`)
# 12, 271, 760
mean(TotalHIV$`2020`)
# 726
mean(TotalHIV$`2021`)
# 911
mean(TotalHIV$`2022`)
# 303
mean(TotalHIV$`2023`)
# 303, 537
mean(TotalHIV$`2024`)
# 207
mean(TotalHIV$`2025`)
# 604
mean(TotalHIV$`2026`)
# 463
mean(TotalHIV$`2027`)
# 71
mean(TotalHIV$`2028`)
# 675
mean(TotalHIV$`2029`)
# 642
mean(TotalHIV$`2030`)
# 46, 101, 147

## Only a few parameter sets give a the closest estimate for more than one year
## 675 (1990 & 2028) 
## 463 (1992 & 2026) 
## 303 (2022 & 2023) 
## 537 (2009 & 2023) 
## 452 (1993 & 2012) 
## 152 (1999 & 2010)

##  Next examining LogLikelihoods
min(LogLxWeight$V1) # -85012.1
which.min(LogLxWeight$V1) # 3709


