# Work Schedule
### 1. **Main workflow**
Caculate for whole dataset.
'Whole' means fully use dataset data. Especially unlike the **roster**, **predict_onestep** and **crossvalidate**.
#### Relative Files
pyBKT\source-py\pyBKT\fit\EM_fit.py  
pyBKT\source-py\pyBKT\fit\M_step.py  
pyBKT\source-py\pyBKT\models\Model.py
some **util** files used by Model.py
#### Risk
1. GPT not work well in translate python to R
   1. multiprocessing handle different
   1. Data format changes (numpy to R matrix)
   1. object-oriented difference (python class Vs R class)
   1. Mutiple files interaction (import ways difference)
   1. input and output interface difference (file IO)
2. Unkown dependency (not found until now)

## Remained works
1. **Roster**
   The model has been extended into the Roster to accomodate and simulate the learning environment for a cohort of students learning any combination of individual skills. The Roster feature has the efficient ability to track individuals' progress through the mastery and correctness probabilities outputted by BKT by storing only the current latent and observable state of the student. The following shows an example of Roster being used in practise.
2. **predict_onestep**
   Caculate for only one timestamp step.
3. **crossvalidate**
   Tow kins of crossvalidate ways. Gives error state as output.
4. **data_helper**
   convert unformat data into format ones. Mainly handle the type errors.
5. **c++BKT**
   BKT in c++ and R

## Code Format
1. S4 to S3 class style
2. code format  (https://r-pkgs.org/code.html#code-style) styler::style_pkg()
   1. function api arguments format
   2. names format
   3. syntax (linter installed)

## Document
1. roxygen2 document format (https://style.tidyverse.org/documentation.html)
