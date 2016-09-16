# Social-Interaction-and-Crime

This `GitHub` site provides the R code, some example output and a statement about reproducability for the paper **Social Interactions and Crime Revisited: An Investigation Using Individual Offender Data in Dutch Neighborhood** written by Wim Bernasco, Thomas de Graaff, Jan Rouwendal and Wouter Steenbeek to be published in the *Review of Economics and Statistics.

Note that:
- for privacy reasons the underlying data is not allowed to be published; please contact me for further information if needed;
- this only provides the code for the second stage estimations and subsequent analyses; first stage estimation is a straightforward (but time-consuming) logit analysis with alternative specific constants.
- one output file is given for the baseline case which should clarify the type of output and the working of the code.

## Code files provided

The files provided here are:
- `EstimationCrimeBayer.R`: basic the control file as it reads in data and give specifications for OLS, IV and quantile regressions. This script solves for that crime rate for which espilon is zero
- `EstimationCrimeDirectIV.R`: basic the control file as it reads in data and give specifications for OLS, IV and quantile regressions. This script doesn't solve for the crime rate but fixes it as a pre-specified value (typically 0)
- `IteractionBayer.R`: core code file that calculates the intruments for each neighborhood and for each interation solved for crime rate that specifies epsilon = 0
- `IteractionDirectIV.R`: core code file that calculates the intruments for each neighborhood and for each interation with crime rate fixed
- `makefig.R`: Creates neighborhood-specific logit plots (sigmoids);
- `CharacteristicsEq.R`: provides various characteristics of the equilibria;
- `FindEquilibria.R`: finds all equilibria (1 or 3) for each neighborhood and iteration;
- `OutputExample.Rmd`: `R` markdown file combining code and output;
- `OutputExample.html`: Example code and output in `html`;
- `OutputExample.pfd`: Example code and output in `pfd`.
- `ReadGeneralData.R`: Code to read in the dataset (very specific for this set-up)

## About reproducability & replication

Our paper **Social Interactions and Crime Revisited: An Investigation Using Individual Offender Data in Dutch Neighborhood** describes research that utilizes police records of all citizens aged 12 years and older of The Netherlands who have been charged with one or more crimes (felonies as well as misdemeanors). These data contain details (i.e. gender, year of birth, ethnic origin, neighborhood of residence and numbers, years and types of crimes the subjects have ever been charged with). This paper has been exempted from the data requirement of the *Journal of Economics and Statistics* on the basis of the following statement.

>These are proprietary data that have been obtained from The Netherlands National Police (and with consent of The Netherlands' Ministry of Justice) under an arrangement that strictly precludes their posting. 

>We fully respect and endorse the philosophy and derived data and computer code availability policy of the *Review of Economics and Statistics*, and would in fact welcome and encourage replication of our work. Therefore, we commit ourselves to making the raw (i.e. micro-level, individual) data available to anyone making an application. We declare that the applicant(s) will be given on-site access to all crime data, all supplemental non-proprietary data (GIS files and neighborhood level census data), and all code (`Stata` for the logit estimations, and `R` for the instrumental variables estimations and quantile regressions). As the replications or extensions must be performed on-site at the Netherlands Institute for the Study of Crime and Law Enforcement (NSCR), we will also provide an office and sufficiently powerful computing resources. If necessary, the applicant(s) must provide funding for travel and accommodation. The applicant(s) will be required to sign an agreement stating that they (1) will use the data only for scientific purposes, (2) will not make the data accessible to third parties, and (3) will not publish results that will disclose the identity of the subjects in the data. 

For further information and for requests to be granted access to the data, please contact:

Wim Bernasco 
Netherlands Institute for the Study of Crime and Law Enforcement (NSCR)
Postal address: P.O. Box 71304, 1008 BH Amsterdam, The Netherlands
Visiting address: De Boelelaan 1077a, (entrance Buitenveldertselaan), 1081 HV Amsterdam, The Netherlands
Phone: +31 (0)20 598 5239
Fax: +31 (0)20 598 3975
Email: [wbernasco@nscr.nl](wbernasco@nscr.nl)
