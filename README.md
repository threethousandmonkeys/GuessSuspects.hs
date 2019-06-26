# GuessSuspects.hs
A program wrote in Haskell tries to guess the lineup of two different people you specify. 

Each person is a three letter abbreviation of a person's height, hair colour and sex, in that order. 

Height is one of S or T; hair colour is one of B, R, or D; and sex is one of M or F.



```
Your guess 1:  SBM SRM
My answer:  (0,1,2,1)
Your guess 2:  SBF TRM
My answer:  (0,2,2,2)
Your guess 3:  SRF TBM
My answer:  (2,0,0,0)
You got it in 3 guesses!
```

