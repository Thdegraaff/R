*****************************************************************
* Social Interactions and Crime Revisited: An Investigation Using 
* Individual Offender Data in Dutch Neighborhoods
*****************************************************************
* Wim Bernasco, Thomas de Graaff, Jan Rouwendal & Wouter Steenbeek
*****************************************************************


*****************************************************************
* First stage of model estimation (Youth version)
* Stata v 11.2
*****************************************************************

*****************************************************************
* set matsize and memory
*****************************************************************
clear
clear mata
clear matrix
set matsize 11000
set mem 8g
set more off

global BASE         "G:/MSC/PROJECTS/SocialInteractions/"
global LOGFOLDER    "${BASE}RESTAT_BUILD/"
global WORKFOLDER   "${BASE}RESTAT_BUILD/WorkData/"
global DATAFOLDER   "${BASE}Data/Original/"
global PC4DATA      "${BASE}RESTAT_BUILD/SourceData/PC4/"

*****************************************************************
* Define minimum number of residents in order for (PC4) 
*    neighborhoods to be included in the analysis
*****************************************************************
global POPMINIMUM = 100

*****************************************************************
* Read (proprietary) datafile and produce automatic codebook
*   Note this a a proprietary file and permission was given
*   by RESTAT Editors to not include it with the computer code
***************************************************************** 
use "${DATAFOLDER}NL_OFFENDERS_2006_PC4.dta", clear
noi codebook
  
************************************************************************
* Selection from population: keep youth only (ages 10-19)
************************************************************************
keep if (age20cat == 2 | age20cat == 3)

************************************************************************
* Create dummy Female from Male
************************************************************************  
gen female = 1-male
label var female "Female (dummy)"

************************************************************************
* Recode age 
************************************************************************
gen cage20cat = age20cat-2
label var cage20cat "Age per 1/1/2007 (cat) (10-14=0, 15-19=1)"

************************************************************************
* Merge in the number and name of the municipality to which PC4 belongs
************************************************************************
rename pc4 pc4nr
merge m:1 pc4nr using  ${PC4DATA}nlp4pr07.dta, keepusing(gemnr gemnaam) keep(match master)
rename pc4nr pc4

************************************************************************
* Create dummies indicating any crime in 2006
************************************************************************
quietly { 
  generate byte anycrime = total > 0
  generate byte anyviolent = (gw_sex + gw_ovg + ov_sex + vm_gw + opium + verkeer + overig + vern_oo) > 0
  generate byte anyproperty = vm_ovg  > 0
  label var anycrime "person committed 1+ crimes in 2006"
  label var anyviolent  "person committed 1+ violent (overt) crimes in 2006"
  label var anyproperty "person committed 1+ (non-violent) property crimes in 2006"
}
************************************************************************
* How many people committed any crime in 2006
************************************************************************
tabulate anycrime [fw=frequency]
tabulate anyviolent [fw=frequency]
tabulate anyproperty [fw=frequency]

************************************************************************
* Calculate how many OTHER youth in (PC4) area committed ANY crime, thus
*   1. sum weighted individuals within pc4 in 2006
*   2. substract 1 if the individual was a criminal in 2006
************************************************************************

egen totalpop = total(frequency), by(pc4)
label var totalpop "PC4 total population"

egen pc4anycrime   = total(anycrime     * frequency), by(pc4)
egen pc4anyviolent = total(anyviolent   * frequency), by(pc4)
egen pc4anyproperty = total(anyproperty * frequency), by(pc4)

label var pc4anycrime    "# persons in person's PC4 that have anycrime (incl self)"
label var pc4anyviolent  "# persons in person's PC4 that have anyviolent (incl self)"
label var pc4anyproperty "# persons in person's PC4 that have anyproperty (incl self)"

generate fieldcrime    = pc4anycrime    - anycrime
generate fieldviolent  = pc4anyviolent  - anyviolent
generate fieldproperty = pc4anyproperty - anyproperty

label var fieldcrime    "# other persons in person's PC4 that have anycrime (excl self)"
label var fieldviolent  "# other persons in person's PC4 that have anyviolent (excl self)"
label var fieldproperty "# other persons in person's PC4 that have anyproperty (excl self)"

************************************************************************  
* percentage of criminal others (e.g. divided by population - 1 )
************************************************************************  
generate pfieldcrime    = 100 * fieldcrime    / (totalpop-1)
generate pfieldviolent  = 100 * fieldviolent  / (totalpop-1)
generate pfieldproperty = 100 * fieldproperty / (totalpop-1)
  
************************************************************************  
* the previous definition is replaced by an aggregate definition
************************************************************************  
replace pfieldcrime    = 100 * pc4anycrime / totalpop
replace pfieldviolent  = 100 * pc4anyviolent  / totalpop
replace pfieldproperty = 100 * pc4anyproperty / totalpop
label var pfieldcrime    "% persons in person's PC4 that have anycrime (incl self)"
label var pfieldviolent  "% persons in person's PC4 that have anyviolent (incl self)"
label var pfieldproperty "% persons in person's PC4 that have anyproperty (incl self)"
 
generate anyfieldcrime    = fieldcrime > 0
generate anyfieldviolent  = fieldviolent > 0
generate anyfieldproperty = fieldproperty > 0

label var anyfieldcrime    "are there (other) criminals in pc4 of person" 
label var anyfieldviolent  "are there (other) criminals in pc4 of person" 
label var anyfieldproperty "are there (other) criminals in pc4 of person" 


************************************************************************  
* Frequencies violent and non-violent offenders
************************************************************************  
noi tab anyviolent [fw=frequency] if anycrime
noi tab anyproperty [fw=frequency] if anycrime
noi tab anyproperty anyviolent [fw=frequency] if anycrime, cell

  
************************************************************************  
* Save the file required for logit
************************************************************************  
preserve 
keep pc4 pfieldcrime pfieldviolent pfieldproperty pc4anycrime pc4anyviolent pc4anyproperty totalpop
foreach V of varlist pfieldcrime pfieldviolent pfieldproperty pc4anycrime pc4anyviolent pc4anyproperty totalpop {
  bys pc4: assert `V' == `V'[1]
}
************************************************************************  
* Keep 1 record per PC4
************************************************************************  
bys pc4: keep if _n==1
sort pc4
save "${WORKFOLDER}logit_pc4anycrime2006_Youth.dta", replace
restore

*************************************************************************
* Call R to run "SpatialLagConstructionViolentProperty2006_Youth.R"
*************************************************************************



************************************************************************  
* Selection criteria:
*   1) population must be above ${POPMINIMUM}
*   2) exclude postal code 2331 (Stevenshof, Leiden) which is reference 
************************************************************************    
keep if totalpop > ${POPMINIMUM} | pc4==2331

* Step 1. 
*noi logit anycrime            [fw=frequency], nolog
*noi logit anycrime female [fw=frequency], nolog
*noi logit anycrime female cage20cat [fw=frequency], nolog
*beware: cage20cat2 removed in analysis of juveniles:
capture noi logit anycrime female foreign cage20cat [fw=frequency], nolog


*noi logit anyviolent               [fw=frequency], nolog
*noi logit anyviolent female          [fw=frequency], nolog
*noi logit anyviolent female cage20cat [fw=frequency], nolog
*beware: cage20cat2 removed in analysis of juveniles:
capture noi logit anyviolent female foreign cage20cat [fw=frequency], nolog

*noi logit anyproperty               [fw=frequency], nolog
*noi logit anyproperty female          [fw=frequency], nolog
*noi logit anyproperty female cage20cat [fw=frequency], nolog
*beware: cage20cat2 removed in analysis of juveniles:
capture noi logit anyproperty female foreign cage20cat [fw=frequency], nolog

* Step 2. 

* include number of others who are criminals in pc4
 capture noi {
 logit anycrime fieldcrime                               [fw=frequency], nolog
 *beware: cage20cat2 removed in analysis of juveniles: 
 logit anycrime fieldcrime female foreign cage20cat       [fw=frequency], nolog

 logit anyviolent fieldviolent                           [fw=frequency], nolog
 *beware: cage20cat2 removed in analysis of juveniles: 
 logit anyviolent fieldviolent female foreign cage20cat    [fw=frequency], nolog

 logit anyproperty fieldproperty                         [fw=frequency], nolog
 *beware: cage20cat2 removed in analysis of juveniles: 
 logit anyproperty fieldproperty female foreign cage20cat  [fw=frequency], nolog
 }
 
* now with proportion others who are criminals in pc4
 capture noi {
 logit anycrime pfieldcrime                               [fw=frequency], nolog
 *beware: cage20cat2 removed in analysis of juveniles:  
 logit anycrime pfieldcrime female foreign cage20cat       [fw=frequency], nolog

 logit anyviolent pfieldviolent                           [fw=frequency], nolog
 *beware: cage20cat2 removed in analysis of juveniles:  
 logit anyviolent pfieldviolent female foreign cage20cat   [fw=frequency], nolog

 logit anyproperty pfieldproperty                         [fw=frequency], nolog
 *beware: cage20cat2 removed in analysis of juveniles:  
 logit anyproperty pfieldproperty female foreign cage20cat [fw=frequency], nolog
 }

********************************************************
* To prepare for step 3., create dummies (e.g. bu1011 for pc4==1011)
********************************************************
foreach j of numlist 1/9 {
  local low = `j' * 1000
  local high = `low' + 999
  foreach i of numlist `low'/`high' {
    qui count if pc4==`i'
    if r(N) {
      qui generate bu`i' = pc4==`i'
    } 
  }
  noi display "`j'"
}

drop gw_sex ov_sex gw_ovg vm_ovg vm_gw verkeer opium overig

********************************************************
* Postal code 2331 is referentie neighborhood
********************************************************
drop bu2331

* Step 3. 
set more off

foreach DEPVAR of varlist anycrime anyviolent anyproperty  { 
  preserve
  
  *capture noi logit `DEPVAR' female foreign cage20cat bu* [fw=frequency],  
  *estimates save "${WORKFOLDER}logit_NL${POPMINIMUM}_alpha_`DEPVAR'2006_Youth", replace
  
  estimates use ${WORKFOLDER}logit_NL${POPMINIMUM}_alpha_`DEPVAR'2006_Youth.ster
  

  ********************************************************
  * Generate prediction corrected for pc4's
  ********************************************************
  generate hat = 1 / (1 + exp(-(_b[female]   * female +     ///
                                _b[foreign]  * foreign +    ///
                                _b[cage20cat] * cage20cat +  ///  
								_b[_cons] )))
  
  generate hat_nocons = 1 / (1 + exp(-(_b[female]   * female +     ///
                                _b[foreign]  * foreign +    ///
                                _b[cage20cat] * cage20cat   ///   
								)))
  
  gen bfemale = _b[female]
  gen bforeign = _b[foreign]
  gen bcage20cat = _b[cage20cat]
  gen b_cons = _b[_cons]
  
  drop bu*
  save "${WORKFOLDER}hat_`DEPVAR'2006_Youth.dta", replace
  ********************************************************
  * Sum predictions over pc4's
  ********************************************************
  collapse (sum) sumhat=hat, by(pc4)
  save "${WORKFOLDER}sumhat_`DEPVAR'2006_Youth", replace								

  *************************************************
  * Store estimates pc4 parameters (alpha) to file
  *************************************************
  * Store estimates in row vector
  matrix define B = e(b)
  matrix VCE = get(VCE)
  mat D = vecdiag(VCE)
  mat drop VCE
  * Copy names
  global COLLAB : colnames B
  * Transpose into column vector
  matrix define B = B'
  matrix define D = D'
  matrix define M = B,D
  * Transfer vector to variable
  drop _all
  svmat double M, names(B)
  rename B1 alpha
  rename B2 varalpha
  * Calculate size of name-vector COLLAB
  global COLLABSIZE : word count ${COLLAB}
  noi display as text "Nr of PC4s is : ${COLLABSIZE}"
  * Create variable from name-vector
  capture drop pc4
  generate str12 pc4=""

  local j = 1
  while (`j' <= ${COLLABSIZE'}) {
    local VARNAME : word `j' of ${COLLAB}
    noi disp "`j ' : `VARNAME'"
    qui replace pc4 = "`VARNAME'" if _n==`j'  
    local j = `j' + 1
  }

  * Make explicit that o.buxxxx is a non-estimated parameter
  replace alpha    = . if alpha ==0   & substr(pc4, 1,4)=="o.bu"
  replace varalpha = . if varalpha==0 & substr(pc4, 1,4)=="o.bu"
  generate sealpha = sqrt(varalpha)
  drop varalpha 

  * Correct names of o.buxxxx PC4's
  replace pc4 = subinstr(pc4, "o.bu", "bu", 1)
  
  * keep only the alphas
  replace pc4 = "bu2331" if pc4 == "_cons"
  keep if substr(pc4, 1,2)=="bu" 

  replace pc4 = substr(pc4, 3, .)
  destring pc4, replace
  sort pc4
  order pc4
  save "${WORKFOLDER}logit_NL${POPMINIMUM}_alpha_`DEPVAR'2006_Youth", replace
  restore
}

  
  
capture drop _all
use "${WORKFOLDER}logit_NL${POPMINIMUM}_alpha_anyviolent2006_Youth"
rename alpha    alpha_violent
rename sealpha sealpha_violent
merge 1:1 pc4 using "${WORKFOLDER}logit_NL${POPMINIMUM}_alpha_anyproperty2006_Youth"

assert _merge == 3
drop _merge
rename alpha    alpha_property
rename sealpha sealpha_property
merge 1:1 pc4 using "${WORKFOLDER}logit_NL${POPMINIMUM}_alpha_anycrime2006_Youth"

assert _merge == 3
drop _merge
rename alpha    alpha_crime
rename sealpha sealpha_crime

replace alpha_property = 0 if pc4==2331 
replace alpha_violent = 0 if pc4==2331
replace alpha_crime = 0 if pc4==2331
save "${WORKFOLDER}logit_NL${POPMINIMUM}_all_alpha2006_Youth", replace 

*****************************************************************************
*  postal code
*  # involved in crime, violent crime, property crime
*  % involved in crime, violent crime, property crime
*  pc4-alpha voor crime, violent crime, property crime
*  s.e. of pc4-alpha voor crime, violent crime, property crime
*  # involved in crime, violent crime, property crime (1st order spatial lag)
*  % involved in crime, violent crime, property crime (1st order spatial lag)
*  # involved in crime, violent crime, property crime (2nd order spatial lag)
*  % involved in crime, violent crime, property crime (2nd order spatial lag)
*****************************************************************************

use "${WORKFOLDER}LaggedPC4delinquents2006_Youth", replace
drop if pc4==.
sort pc4
save "${WORKFOLDER}LaggedPC4delinquents2006_Youth", replace

use "${WORKFOLDER}logit_NL${POPMINIMUM}_all_alpha2006_Youth", replace 
noi mmerge pc4 using "${WORKFOLDER}logit_pc4anycrime2006_Youth"

keep if  _merge == 3
drop _merge

noi merge 1:1 pc4 using "${WORKFOLDER}LaggedPC4delinquents2006_Youth"

keep if _merge == 3
gen Lagpfieldcrime1    = 100 * Laggedtotal1    / Laggedpop1
gen Lagpfieldviolent1  = 100 * Laggedviolent1  / Laggedpop1
gen Lagpfieldproperty1 = 100 * Laggedproperty1 / Laggedpop1

gen Lagpfieldcrime2    = 100 * Laggedtotal2    / Laggedpop2
gen Lagpfieldviolent2  = 100 * Laggedviolent2  / Laggedpop2
gen Lagpfieldproperty2 = 100 * Laggedproperty2 / Laggedpop2

keep pc4 pc4anycrime pc4anyviolent pc4anyproperty  ///
          pfieldcrime pfieldviolent pfieldproperty  ///
		  alpha_crime sealpha_crime ///
          alpha_violent sealpha_violent ///
		  alpha_property sealpha_property    ///
		  Laggedtotal1 Laggedviolent1 Laggedproperty1  ///
		  Lagpfieldcrime1 Lagpfieldviolent1 Lagpfieldproperty1  ///
		  Laggedtotal2 Laggedviolent2 Laggedproperty2  ///
		  Lagpfieldcrime2 Lagpfieldviolent2 Lagpfieldproperty2

order pc4 pc4anycrime pc4anyviolent pc4anyproperty  ///
          pfieldcrime pfieldviolent pfieldproperty  ///
		  alpha_crime sealpha_crime ///
          alpha_violent sealpha_violent ///
		  alpha_property sealpha_property    ///
		  Laggedtotal1 Laggedviolent1 Laggedproperty1  ///
		  Lagpfieldcrime1 Lagpfieldviolent1 Lagpfieldproperty1  ///
		  Laggedtotal2 Laggedviolent2 Laggedproperty2  ///
		  Lagpfieldcrime2 Lagpfieldviolent2 Lagpfieldproperty2

label var pc4                     "postal code"
label var pc4anycrime             "# involved in crime"
label var pc4anyviolent           "# involved in violent crime"
label var pc4anyproperty          "# involved in property crime"
label var pfieldcrime             "% involved in crime"
label var pfieldviolent           "% involved in violent crime"
label var pfieldproperty          "% involved in property crime"
label var alpha_crime             "pc4-alpha all crime"
label var sealpha_crime           "s.e. of alpha_crime"
label var alpha_violent           "pc4-alpha violent crime"  
label var sealpha_violent         "s.e. of alpha_violent"
label var alpha_property          "pc4-alpha property crime"  
label var sealpha_property        "s.e. of alpha_property"
label var Laggedtotal1            "# involved in crime (1st order spatial lag)"
label var Laggedviolent1          "# involved in violent crime (1st order spatial lag)"
label var Laggedproperty1         "# involved in property crime (1st order spatial lag)"
label var Lagpfieldcrime1         "% involved in crime (1st order spatial lag)"
label var Lagpfieldviolent1       "% involved in violent crime (1st order spatial lag)"
label var Lagpfieldproperty1      "% involved in property crime (1st order spatial lag)"
label var Laggedtotal2            "# involved in crime (2nd order spatial lag)"
label var Laggedviolent2          "# involved in violent crime (2nd order spatial lag)"
label var Laggedproperty2         "# involved in property crime (2nd order spatial lag)"
label var Lagpfieldcrime2         "% involved in crime (2nd order spatial lag)"
label var Lagpfieldviolent2       "% involved in violent crime (2nd order spatial lag)"
label var Lagpfieldproperty2      "% involved in property crime (2nd order spatial lag)"


save "${WORKFOLDER}final_forThomas_2006_Youth.dta", replace

