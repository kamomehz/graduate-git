


# Data source

1. sspGdp.xlsx, sspPop.xlsx, urbanShare.xlsx

- [SSP Database](https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=about)


2. coordinate.csv

- Pick this city list from wiki (megacities and capital cities)
- Then get the coordinate from google map API

3.	code.csv

- Transfer country Alpha-3 to the full name

# My programs

1. pick.R

- Calculate urban area in 2000, 2005, 2010 from Landscan dataset
- Areas over 1000 people/gird are regarded as urban. (one grid is around 1km^2)

2. re_csvmaker.ipynb

- Data preparing
- Create dataframes can be used in further works (step3,4)


3. re_regression.ipynb

- Build a model

4. re_predict.ipynb

- Predict with the model  

5. re_redistribution.ipynb

- According to the model output, adjust SLEUTH out 2050 in folder ./sleuthOut



このようにコードを載せられる
```
grayScale = ( (0.3 * R) + (0.59 * G) + (0.11 * B) )
```


崩壊3rd やり込み中（ログイン800日突破）
![1](/yae.jpg)


