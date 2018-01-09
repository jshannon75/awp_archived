# Athens Wellbeing Project
This repo has data and code related to the [Athens Wellbeing Project](http://www.athenswellbeingproject.org) (AWP). A web portal to access this data is in development. The Data folder contains both secondary and primary survey data related to AWP. Specifically, these files are included:

*[Atlas Social Atlas Data_complete.xlsx](https://github.com/jshannon75/awp/raw/master/Data/Athens%20Social%20Atlas%20Data_complete.xlsx): An Excel file with numerous variables at the elementary school level.
*[awp_codebook.xls](https://github.com/jshannon75/awp/raw/master/Data/awp_codebook.xls): An Excel file with all variable names from the quadrant level survey data and the questions they link to
*[awp_estAll.csv](https://github.com/jshannon75/awp/raw/master/Data/awp_estAll.csv): A CSV file with quadrant level population estimates for responses to each of the survey questions.
*[awp_pctAll.csv](https://github.com/jshannon75/awp/raw/master/Data/awp_pctAll.csv): A CSV file with quadrant level rates of response to each survey question. These percentages are provided in raw form, so .37 would be 37% of the quadrant population.
*[awp_estPct.csv](https://github.com/jshannon75/awp/raw/master/Data/awp_estPct.csv): A CSV file with quadrant level both population estimates and response rates. This file just combines the awp_estAll and awp_pctAll datasets.

Three R based script files are also included in the data files, which provide the commands we used to process household responses to the AWP survey conducted in Fall 2016. Household responses are not available online, but contact Jerry Shannon (jshannon@uga.edu) if you have questions about these data.   

Most of the datasets above are in "long" format, meaning that individual variables are listed down a single column. For example, in the image below, we see responses to the question on gender. 

![Data file](/Other/pct_data.png)

Under the variable column, the codebook indicates that childcare1 refers to those who strongly disagree that their childcare is affordable, while childcare_afford3 refers to those who strongly agree that it is. For households in the BHL middle school zone, 252 people (or 22.6%) think that their childcare is not affordable, while 439 (or 39.4%) believe that it is. 

The included standard errors allow you to calculate the uncertainty for each estimate. In general, you can multiply the standard error by 1.96 to get a margin of error for each estimate. In the example above, the margin of error for the BHL's estimate for the childcare1 variable would be 0.64 * 1.96 = .125, or 12.5%. So we think that 22.6% of people think childcare is unaffordable in that zone, but it could be as high as 35.1% or as low as 10.1%. 

Due to the relatively small sample size in some areas, data are presented in quadrants--based on middle school attendance zones. Those zones are outlined on the map below.

![Map](/Other/quadrant_map.png | width=50%)