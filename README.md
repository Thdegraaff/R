# Social-Interaction-and-Crime

Gives R code and example output for the paper **Social Interaction and Crime**. Note that:
- for privacy reasons the underlying data is not allowed to be published; please contact me for further information if needed;
- this only provides the code for the second stage estimations and subsequent analyses; first stage estimation is a straightforward (but time-consuming) logit analysis with alternative specific constants.
- one output file is given for the baseline case which should clarify the type of output and the working of the code

The files provided here are:
- `EstimationCrime.R`: basic the control file as it reads in data and give specifications for OLS, IV and quantile regressions. Note again that the underlying data cannot be made publicly available;
- `IteractionBayser.R`: core code file that calculates the intruments for each neighborhood and for each interation. 
- `makefig.R`: Creates neighborhood-specific logit plots (sigmoids);
- `CharacteristicsEq.R`: provides various characteristics of the equilibria;
- `FindEquilibria.R`: finds all equilibria (1 or 3) for each neighborhood and iteration. 
