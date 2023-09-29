#### **Undergraduate Thesis**

Firstly, I import multiple variables from the Brazilian Central Bank [website](https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries).

Then, from this single dataframe, I create 12 new dataframes where variables are accumulated. For instance, *"df6"* has the variables accumulated in 6 months.

Finally, for each dataframe I compute rolling and expanding window forecasts for the Brazilian yearly inflation. The models used were Random Forest, LASSO, Ridge, elastic net, adaLASSO and Random Walk. I compare these models with the Focus survey from the Brazilian Central Bank.

------------------------------------------------------------------------


**1.** `generate_data`

Folder with the R files that create the 12 dataframes. Run the file **accumulating_functions** before running **import_data**. 

The file **import_data** calls the excel file **variaveis_descricao** that specifies which "accumulating formula" should be applied to each variable according to their "nature".For instance, the way of accumulating variables that are indexes is different from the way of accumulating variables that are percentage changes. 

------------------------------------------------------------------------


**2.** `data`

Folder where the 12 dataframes are stored.

------------------------------------------------------------------------


**3.** `forecast`

Files that compute forecasts and errors. The workspace is available in case you want to look at the outputs without having to run the files.

------------------------------------------------------------------------


**4.** `generate_plot`

In the thesis, the only plot that I use is generated by the **rolling_error_plot** file. The file **prev_plot** creates lineplots of the predictions for each model.

------------------------------------------------------------------------

**5.** `latex`

Contains the *.tex* code I used in my thesis. Notice that all the tables were generated in the **forecast_rolling** and **forecast_expanding** files. I used the stargazer package to obtain the tables written in *.tex*. After that, I did some minor aesthetic adjustments when I was using Overleaf.    





