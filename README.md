# Public Opinion in Arizona

This repo includes code to clean, merge, analyze, recode, and visualize  public opinion data for the Arizona Voter Project.

## Data Cleaning, Formatting, Estimation
### $\texttt{R/}$

$\texttt{recode.R}$: Various functions used in the analysis
$\texttt{orderedLogit.R}$: The analysis consists of estimating a series of ordered logit models. The sript creates an "ordered logit"
function, then I loop through a series of questions to estimate the models. The output is a series of tables, which I export to the $\texttt{data/}$ folder. 
These data are used in the visualizations.



### $\texttt{clean/}$

* buildDataAVP22.R: Cleans the Western Data 
* buildDataAVP22.R: Cleans the AVP Wave 1 Panel. 
* buildDataAVP24.R: Cleans the AVP Wave 2 Panel. 
Each of these files exports a cleaned data set to the $\texttt{data/}$ folder.
* combineDat.R: Combines the three datasets in a variety of ways. Each file is represented as a long and wide form. I created a single file, with common questions across datasets. I also created a panel dataset, by joining respondents from wave 1 to wave 2.

### $\texttt{data/}$

* avp_w1_wide.csv. Wave 1 of the AVP survey in *wide* form, conducted by YouGov.
* avp_w1_long.csv. Wave 1 of the AVP survey in *long* form, conducted by YouGov.
* avp_w2_wide.csv. Wave 2 of the AVP survey in *wide* form, conducted by YouGov.
* avp_w2_long.csv. Wave 2 of the AVP survey in *long* form, conducted by YouGov.
* datWestern2020_long.csv. The Western Data in *long* form.
* datWestern2020_wide.csv. The Western Data in *wide* form, conducted by YouGov.
* avpLong.csv. Truncated three files in *long* form.
* avpWide.csv. Truncated three files in *long* form.
* avp_panel.csv. The two wave panel data from Arizona in *wide* form, conducted by YouGov.
* avp_w1w2_long.csv. The two wave panel data from Arizona in *long* form, conducted by YouGov.

## Data Pipeline
The raw files are in an external folder. The data cleaning process involves a unique file for each survey, which exports a 
minimally cleaned up version of the data. These data are then combined, and further refined in combinedDat.R. Finally,
The ordered_logit file is run to export predictions to the relveant shiny folder.

# Visualization

### $\texttt{shiny/}$

[shiny/contestation](https://viz.datascience.arizona.edu/avp_democracy/). Shiny app visualizing electoral contestion with the Wave I AVP data.
[shiny/immigration](https://viz.datascience.arizona.edu/avp_democracy/). Shiny app visualizing electoral contestion with the Wave I AVP data.
[shiny/guns](https://viz.datascience.arizona.edu/avp_democracy/). Shiny app visualizing electoral contestion with the Wave I AVP data.
[shiny/abortion](https://viz.datascience.arizona.edu/avp_democracy/). Shiny app visualizing electoral contestion with the Wave I AVP data.

## Data Information

All three datasets were collected by YouGov, though all three have different sampling frames.


### Data from the Western States Survey

The Western States Survey is a collaborative, multi-university effort to understand politics across the Western United Stateds. The sampling frame consists of 
3,577 respondents interviewed by YouGov who were  matched down to a sample of 3000 to produce the final dataset. An oversample of 600
Latinos were interviewed for a total of 3600. Respondents were matched to a sampling frame on gender, age, race, and education. The
frame was constructed by stratified sampling from the full 2018 American Community Survey (ACS) 1-year sample with selection within strata by weighted
sampling with replacement.

The matched cases were weighted to the sampling frame using propensity scores. The matched cases and the frame were combined and a logistic
regression was estimated for inclusion in the frame. The propensity score function included age, gender, race/ethnicity, years of education,
and region. The propensity scores were grouped into deciles of the estimated propensity score in the frame and post-stratified according to
these deciles. The weights were then post-stratified on 2016 Presidential vote choice, and a four-way stratification of gender, age (4-categories), race (4-
categories), and education (4-categories), to produce the final weight.

We focus on the $n=1,000$ respondents who were interviewed from Arizona, shortly before and after the 2020 presidential election.


### Wave I of the Arizona Voter Project
The purpose of the Wave I survey was to understand the electorate following the 2022 Midterm Elections in Arizona. The sampling frame involves new respondents who were interviewed from December 14, 2022 - December 28, 2022. In the Wave II survey, YouGov 865 respondents. These were matched down to a sample of 803
to produce the final dataset. The 1,000 respondents were matched to a sampling frame on gender, age, race, and
education.  The sampling frame is a politically representative sample frame of Arizona registered voters, based upon the American Community Survey (ACS)
public use microdata file, public voter file records, the 2020 Current Population Survey (CPS) Voting and Registration supplements, the 2020 National Election Pool (NEP) exit poll, and the 2020 CES surveys, including demographics and 2020
presidential vote. The sample includes sample-weights to account for sampling and non-response.


### Wave II of the Arizona Voter Project
The purpose of the Wave II survey was to assess stability and change in public opinion. The sampling frame involves a mix of recontacted respondents from the Wave I survey and new respondents.
The original study was conducted  December 14, 2022 - December 28, 2022. In the Wave II survey, YouGov 865 respondents. These were matched down to a sample of 803
to produce the final dataset. Of these 803, we carried over 403 from a recontact of respondents in the original study. The new respondents were matched to a sampling frame on gender, age, race, and
education.  The sampling frame is a politically representative sample frame of Arizona registered voters, based upon the American Community Survey (ACS)
public use microdata file, public voter file records, the 2020 Current Population Survey (CPS) Voting and Registration supplements, the 2020 National Election Pool (NEP) exit poll, and the 2020 CES surveys, including demographics and 2020
presidential vote. The sample includes sample-weights to account for sampling and non-response.

