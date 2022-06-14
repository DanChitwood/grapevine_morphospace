# A predicted developmental and evolutionary morphospace for grapevine leaves

This repository contains data and code for a revised manuscript, "A predicted developmental and evolutionary morphospace for grapevine leaves"

-  The new dataset is titled 'NY_and_CA_data.txt'. The code to reproduced the analyses and figures in the revised manuscript is in a Jupyter notebook titled `grapevine_morphospace.ipynb`
-  The previously used dataset is also included in this repository, titled 'just_NY_data.csv'. The difference between the previous dataset and the current is that the previous only contains New York germplasm data, wherease the new dataset includes both this data as well as California population data
-  The previous dataset is included to reproduce results from Reviewer #1 during the review process. The code analyzing the previous data addressing the points raised by Reviewer #1 is titled 'reviewer1_response.R'. This R code reproduces results from the originally submitted manuscript, the reviewer's results, as well as new results in the revised manuscript by reproducing Principal Component Analysis (PCA) results from Python scikit-learn using R's prcomp function.
