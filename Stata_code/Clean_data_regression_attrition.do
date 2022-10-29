* All the scripts are a combined effort with Flavio Argentieri

clear all

cap cd ""

use "Data\Original_Data\student_test_data.dta", clear

use "Data\Original_Data\teacher_pres_data.dta", clear

collapse yrstaught visitno, by(schoolid)

tempfile tomerge
save `tomerge'

use "Data\Original_Data\student_test_data.dta", clear

merge m:1 schoolid using `tomerge'
drop if _merge==2
drop _merge

*Beginning Duflo

* CREATE VARIOUS VARIABLES;
gen etpteacher_tracking_lowstream=etpteacher*lowstream
gen sbm_tracking_lowstream=sbm*tracking*lowstream
foreach name in bottomhalf tophalf etpteacher  {
	gen `name'_tracking=`name'*tracking
}

gen girl_tracking=girl*tracking

foreach name in bottom second third top{
	gen `name'quarter_tracking=`name'quarter*tracking
}

gen percentilesq=percentile*percentile
gen percentilecub=percentile^3

* CLEAN AGE VARIABLE;
replace agetest=r2_age-1 if agetest==.

* STANDARDIZE TEST SCORES;

sum totalscore if tracking==0
gen meancomp=r(mean) 
gen sdcomp=r(sd) 
gen stdR_totalscore=(totalscore-meancomp)/sdcomp 
drop meancomp sdcomp

* Regressions
reg stdR_totalscore tracking, cluster(schoolid) level(90)
reg stdR_totalscore tracking girl percentile agetest etpteacher, cluster(schoolid)
reg stdR_totalscore tracking bottomhalf bottomhalf_tracking girl percentile agetest etpteacher, cluster(schoolid)
reg stdR_totalscore tracking bottomquarter bottomquarter_tracking secondquarter secondquarter_tracking topquarter topquarter_tracking girl percentile agetest etpteacher, cluster(schoolid)

*Ending Duflo

dummies zone
drop zone

foreach var in agetest yrstaught visitno {
	sum `var'
	local sd = r(sd)
	local mean = r(mean)
	gen `var'_corr = (`var'-`mean')/`sd'
}

*Our regression
reg stdR_totalscore tracking girl agetest_corr etpteacher zone* sbm visitno_corr yrstaught_corr, cluster(schoolid) nocons level(90)

foreach var of varlist stdR_totalscore tracking girl agetest_corr etpteacher zone* sbm visitno_corr yrstaught_corr {
    drop if `var' == .
}

egen group =group(schoolid)

sort schoolid pupilid
order pupilid schoolid group tracking girl agetest_corr etpteacher sbm visitno_corr yrstaught_corr zone* stdR_totalscore

* Imputing percentile

reg percentile girl agetest_corr etpteacher zone* sbm visitno_corr yrstaught_corr, cluster(schoolid) nocons
predict percentile_imputed, xb

gen 		dropped = 0
replace 	dropped = 1 if percentile == .

replace percentile = percentile_imputed if percentile == .

* Generating quartiles of baseline variables

foreach var of varlist percentile agetest_corr yrstaught_corr visitno_corr {		// There are no missing values for these vars
	pctile 		quartile_`var'_support = `var', nq(4)
	*local 		trial1 = quartile_`var'_support[1]					// This part of code is for checking
	*local 		trial2 = quartile_`var'_support[2]
	*local 		trial3 = quartile_`var'_support[3]
	*local 		trial4 = quartile_`var'_support[4]
	*local 		trial "`trial1' `trial2' `trial3' `trial4'"
	*dis 		"`trial'"
	gen			quartile_`var' = .
	replace 	quartile_`var' = 1 if `var' <= quartile_`var'_support[1]
	replace 	quartile_`var' = 2 if `var' > quartile_`var'_support[1] & `var' <= quartile_`var'_support[2]
	replace 	quartile_`var' = 3 if `var' > quartile_`var'_support[2] & `var' <= quartile_`var'_support[3]
	replace 	quartile_`var' = 4 if `var' > quartile_`var'_support[3]
	drop 		quartile_`var'_support
}


order pupilid schoolid group percentile quartile_percentile quartile_agetest_corr quartile_yrstaught_corr quartile_visitno_corr tracking girl agetest_corr etpteacher sbm visitno_corr yrstaught_corr zone* stdR_totalscore
*br pupilid schoolid group percentile quartile_percentile quartile_agetest_corr quartile_yrstaught_corr quartile_visitno_corr tracking girl agetest_corr etpteacher sbm visitno_corr yrstaught_corr zone* stdR_totalscore

*Attrition

sort girl quartile_percentile quartile_agetest_corr
egen cell = group(girl quartile_percentile quartile_agetest_corr)

tab cell if dropped==1

* Cells 1-16 have 0 for girls 17-32 1 for girls
tab cell girl
tab cell quartile_percentile
tab cell quartile_agetest_corr

label define cells 1 "011" 2 "012" 3 "013" 4 "014" 5 "021" 6 "022" 7 "023" 8 "024" 9 "031" 10 "032" 11 "033" 12 "034" 13 "041" 14 "042" 15 "043" 16 "044" 17 "111" 18 "112" 19 "113" 20 "114" 21 "121" 22 "122" 23 "123" 24 "124" 25 "131" 26 "132" 27 "133" 28 "134" 29 "141" 30 "142" 31 "143" 32 "144"

label values cell cells

levelsof cell, local(levels)
local lab : value label cell
*dis "`lab'"

gen cell_cleaned = ""

foreach value of local levels {
    local valuelabel `: label `lab' `value''
	*dis `valuelabel'
	replace cell_cleaned = "`valuelabel'" if cell==`value'
}

tab cell cell_cleaned

* Variability analysis

destring cell_cleaned, replace
levelsof cell_cleaned, local(levels_cell)
dis "`levels_cell'"

* Generating weights for total sample

local Tot = _N
bys cell_cleaned : gen cell_weight_complete = _N/`Tot'

gen cell_variance_complete = .
local total_weights_reg = 0
foreach cell in `levels_cell' {
	qui sum tracking if cell_cleaned == `cell'
	local var_`cell' = (r(sd))^2
	*dis "`var_`cell''"
	replace cell_variance_complete = `var_`cell'' if cell_cleaned == `cell'
	qui sum cell_weight_complete if cell_cleaned == `cell'
	local cell_weight_complete = r(mean)
	*dis `cell_weight_complete'
	local total_weights_reg = `total_weights_reg' + `var_`cell'' * `cell_weight_complete'	
}

gen cell_weight_reg_complete = (cell_variance_complete * cell_weight_complete)/`total_weights_reg'
*dis `total_weights_reg'

gen cell_difference_complete = .
foreach cell in `levels_cell' {
	reg stdR_totalscore tracking if cell_cleaned == `cell', cluster(schoolid)
	replace cell_difference_complete = _b[tracking] if cell_cleaned == `cell'
}

reg cell_difference_complete cell_weight_reg_complete, cluster(schoolid)
/*
-----------------------------------------------------------------------------------------
                         |               Robust
cell_difference_complete |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------------------+----------------------------------------------------------------
cell_weight_reg_complete |  -.7381231   .1812817    -4.07   0.000    -1.097048   -.3791979
                   _cons |   .1737142   .0068873    25.22   0.000     .1600777    .1873506
------------------------------------------------------------------------------------------


*/
tempfile	torestore
save		`torestore'

dummies cell_cleaned
*drop cell_cleaned

*toreplicate
reg stdR_totalscore tracking cell_cleaned*, cluster(schoolid) nocons
/*

--------------------------------------------------------------------------------
               |               Robust
stdR_totalsc~e | Coefficient  std. err.      t    P>|t|     [95% conf. interval]
---------------+----------------------------------------------------------------
      tracking |   .1493218   .0768352     1.94   0.054    -.0028064    .3014501

*/
collapse cell_weight_reg_complete cell_difference_complete, by(cell_cleaned)
br

* Generating weights for dropped sample

use `torestore', clear

keep if dropped==0

local Tot = _N
bys cell_cleaned : gen cell_weight_dropped = _N/`Tot'

gen cell_variance_dropped = .
local total_weights_reg = 0
foreach cell in `levels_cell' {
	qui sum tracking if cell_cleaned == `cell'
	local var_`cell' = (r(sd))^2
	*dis "`var_`cell''"
	replace cell_variance_dropped = `var_`cell'' if cell_cleaned == `cell'
	qui sum cell_weight_dropped if cell_cleaned == `cell'
	local cell_weight_dropped = r(mean)
	local total_weights_reg = `total_weights_reg' + `var_`cell'' * `cell_weight_dropped'	
}

gen cell_weight_reg_dropped = (cell_variance_dropped * cell_weight_dropped)/`total_weights_reg'
*dis `total_weights_reg'

gen cell_difference_dropped = .
foreach cell in `levels_cell' {
	reg stdR_totalscore tracking if cell_cleaned == `cell', cluster(schoolid)
	replace cell_difference_dropped = _b[tracking] if cell_cleaned == `cell'
}

reg cell_difference_dropped cell_weight_reg_dropped, cluster(schoolid)
/*

------------------------------------------------------------------------------
             |               Robust
stdR_total~e | Coefficient  std. err.      t    P>|t|     [95% conf. interval]
-------------+----------------------------------------------------------------
    tracking |   .3177235   .1632687     1.95   0.055    -.0064957    .6419428
       _cons |    .711922   .1049589     6.78   0.000     .5034943    .9203496
------------------------------------------------------------------------------


*/

dummies cell_cleaned
drop cell_cleaned

*toreplicate
reg stdR_totalscore tracking cell_cleaned*, cluster(schoolid) nocons
/*
--------------------------------------------------------------------------------
               |               Robust
stdR_totalsc~e | Coefficient  std. err.      t    P>|t|     [95% conf. interval]
---------------+----------------------------------------------------------------
      tracking |   .1680489    .077514     2.17   0.032     .0144343    .3216634

*/
collapse cell_weight_reg_dropped cell_difference_dropped, by(cell_cleaned)
br

***************

use `torestore', clear

merge m:1 cell_cleaned using "Output\mean_and_median_effects"
drop _merge

reg cell_mean_effect cell_variance_complete, cluster(schoolid)
reg cell_mean_effect cell_variance_complete if dropped==1, cluster(schoolid)
reg cell_mean_effect cell_variance_complete if dropped==0, cluster(schoolid)

reg cell_median_effect cell_variability, cluster(schoolid)
reg cell_median_effect cell_variability if dropped==1, cluster(schoolid)
reg cell_median_effect cell_variability if dropped==0, cluster(schoolid)

* Individual effects

sort pupilid schoolid
gen n=_n

tempfile 	tomerge
save		`tomerge'

use "Output\individual_effects", clear
gen n=_n

merge 1:1 n using `tomerge'
drop _merge n

sum mean_individual_effect median_individual_effect
sum mean_individual_effect median_individual_effect if dropped==0
sum mean_individual_effect median_individual_effect if dropped==1


/*Exporting dropped sample
keep if dropped==0
drop group
egen group = group(schoolid)
*/
sort schoolid pupilid
order pupilid schoolid group percentile quartile_percentile quartile_agetest_corr quartile_yrstaught_corr quartile_visitno_corr tracking girl agetest_corr etpteacher sbm visitno_corr yrstaught_corr zone* stdR_totalscore
br pupilid schoolid group percentile quartile_percentile quartile_agetest_corr quartile_yrstaught_corr quartile_visitno_corr tracking girl agetest_corr etpteacher sbm visitno_corr yrstaught_corr zone* stdR_totalscore dropped cell_cleaned

* Attriters vs non-attriters


local outcomes "percentile girl agetest_corr etpteacher zone1 zone2 zone3 zone4 zone5 zone6 zone7 zone8 zone9 sbm visitno_corr yrstaught_corr"

scalar drop _all
foreach var of varlist `outcomes' {
	reg `var' dropped, cluster(schoolid)
	local t_`var' = _b[dropped]/_se[dropped]
	local p_`var' = 2*ttail(e(df_r),abs(`t_`var''))
	if `p_`var''<=0.1 {
		scalar `var' = `t_`var''
	}
}
scalar list

/*

yrstaught_corr = -2.0006675
     zone8 = -3.6726158
     zone7 = -2.6092988
     zone6 = -3.2655847
     zone5 =  1.7162466
     zone1 = -3.7326004

*/

scalar drop _all
local unbalanced_vars "yrstaught_corr zone1 zone5 zone6 zone7 zone8"
foreach var of varlist `unbalanced_vars' {
	reg stdR_totalscore `var', cluster(schoolid)
	local t_`var' = _b[`var']/_se[`var']
	local p_`var' = 2*ttail(e(df_r),abs(`t_`var''))
	if `p_`var''<=0.1 {
		scalar `var' = `t_`var''
	}
}
scalar list

/*/

     zone7 = -2.6233676
     zone5 = -4.3288108
     zone1 =  4.3466317

*/

scalar drop _all

foreach var of local outcomes_mothers {
	reg `var'2020 ib0.treatment_status `var'2019 `controls_mothers_ANCOVA', vce(cluster village_id)
	local t_`var'_1 = _b[1.treatment_status]/_se[1.treatment_status]
	local p_`var'_1 = 2*ttail(e(df_r),abs(`t_`var'_1'))
	if `p_`var'_1'<=0.05 {
		scalar `var'_1 = 1
	}
	local t_`var'_2 = _b[2.treatment_status]/_se[2.treatment_status]
	local p_`var'_2 = 2*ttail(e(df_r),abs(`t_`var'_2'))
	if `p_`var'_2'<=0.05 {
		scalar `var'_2 = 2
	}
}
scalar list

local i=1
foreach var of varlist `outcomes' {
	local mycommand`i' "qui reg `var' dropped, vce(cluster schoolid)"
	local ++i
}
dis `i'-1
*15 variables

local options "command1(string)"
forvalues i=2/15 {
	local options "`options' command`i'(string)"
	}
local options "`options' estimate1(string)"
dis "`options'"


program multiple_estimates, eclass
syntax, command1(string) command2(string) command3(string) command4(string) command5(string) command6(string) command7(string) command8(string) command9(string) command10(string) command11(string) command12(string) command13(string) command14(string) command15(string) estimate1(string)
	`command1'
	local attrition_1 = `estimate1'
	local attrition_list "`attrition_1'"
	local attrition_sum = `attrition_1'
	forvalues j=2/15 {
		
		`command`j''
		local attrition_`j'=`estimate1'
		local attrition_list "`attrition_`j'',`attrition_list'"
		local attrition_sum = `attrition_sum'+`attrition_`j''
		
	}
	
	local maximum_attrition = max(`attrition_list')
	local mean_attrition = `attrition_sum'/15
	forvalues z=1/15 {
		estadd scalar attrition_`z'=`attrition_`z''
	}
	estadd scalar maximum_attrition= `maximum_attrition'
	estadd scalar mean_attrition = `mean_attrition'
	end	


*****	T-STATISTICS	*****

*Mean absolute t-statistics

*ritest dropped e(mean_attrition), r(100) seed(0): qui multiple_estimates, command1(`mycommand1') command2(`mycommand2') command3(`mycommand3') command4(`mycommand4') command5(`mycommand5') command6(`mycommand6') command7(`mycommand7') command8(`mycommand8') command9(`mycommand9') command10(`mycommand10') command11(`mycommand11') command12(`mycommand12') command13(`mycommand13') command14(`mycommand14') command15(`mycommand15') estimate1(abs(_b[dropped]/_se[dropped]))

/*

------------------------------------------------------------------------------
T            |     T(obs)       c       n   p=c/n   SE(p) [95% Conf. Interval]
-------------+----------------------------------------------------------------
       _pm_1 |   1.329277       0     100  0.0000  0.0000         0   .0362167
------------------------------------------------------------------------------

*/


*Maximum absolute t-statistics

*ritest dropped e(maximum_attrition), r(100) seed(0): qui multiple_estimates, command1(`mycommand1') command2(`mycommand2') command3(`mycommand3') command4(`mycommand4') command5(`mycommand5') command6(`mycommand6') command7(`mycommand7') command8(`mycommand8') command9(`mycommand9') command10(`mycommand10') command11(`mycommand11') command12(`mycommand12') command13(`mycommand13') command14(`mycommand14') command15(`mycommand15') estimate1(abs(_b[dropped]/_se[dropped]))

/*

------------------------------------------------------------------------------
T            |     T(obs)       c       n   p=c/n   SE(p) [95% Conf. Interval]
-------------+----------------------------------------------------------------
       _pm_1 |     3.7326       0     100  0.0000  0.0000         0   .0362167
------------------------------------------------------------------------------

*/


*****	COEFFICIENTS	*****

*Mean difference in mean between treatment and control

*ritest dropped e(mean_attrition), r(100) seed(0): qui multiple_estimates, command1(`mycommand1') command2(`mycommand2') command3(`mycommand3') command4(`mycommand4') command5(`mycommand5') command6(`mycommand6') command7(`mycommand7') command8(`mycommand8') command9(`mycommand9') command10(`mycommand10') command11(`mycommand11') command12(`mycommand12') command13(`mycommand13') command14(`mycommand14') command15(`mycommand15') estimate1(abs(_b[dropped]))

/*

------------------------------------------------------------------------------
T            |     T(obs)       c       n   p=c/n   SE(p) [95% Conf. Interval]
-------------+----------------------------------------------------------------
       _pm_1 |   .1234443       0     100  0.0000  0.0000         0   .0362167
------------------------------------------------------------------------------

*/


*School level variables

collapse zone* yrstaught_corr visitno_corr sbm, by(schoolid)

sort schoolid
order schoolid sbm visitno_corr yrstaught_corr zone*
*br

/* Old (but keep it, may be useful)
/*


foreach var of varlist totalscore_corr girl agetest_corr etpteacher std_mark realpercentile_corr {
	qui bysort schoolid : egen `var'_by_school = mean(`var')
	replace `var' = `var'_by_school if `var' ==.
}
foreach var of varlist std_mark realpercentile_corr {
	sum `var' if tracking == 0
	local `var'_mean = r(mean)
	replace `var' = ``var'_mean' if `var' == . & tracking == 0

}
*/

reg stdR_totalscore tracking bottomquarter bottomquarter_tracking secondquarter secondquarter_tracking topquarter topquarter_tracking girl percentile agetest etpteacher zone* sbm yrstaught visitno, cluster(schoolid) nocons




*reg totalscore_corr tracking girl agetest_corr etpteacher std_mark realpercentile_corr zone* sbm yrstaught_corr visitno_corr, cluster(schoolid) nocons

egen group =group(schoolid)

*br pupilid schoolid group totalscore_corr tracking girl agetest_corr etpteacher std_mark realpercentile_corr zone* sbm yrstaught_corr visitno_corr

pctile quartile = std_mark, nq(4)
tab quartile
drop quartile
/*
  -.6762019
   .0226116
   .6521596

*/

/*
  -.7710784
  -.0373848
   .7556142 

*/
gen quartile = .
replace quartile = 1 if std_mark < -.7710784 & std_mark!=.
replace quartile = 2 if std_mark > -.7710784 & std_mark < -.0373848 & std_mark!=.
replace quartile = 3 if std_mark > -.0373848 & std_mark < .7556142 & std_mark!=.
replace quartile = 4 if std_mark > .7556142 & std_mark!=.

reg totalscore_corr tracking##ib3.quartile etpteacher agetest_corr girl std_mark, cluster(schoolid)

forvalues i = 1/4 {
	sum std_mark if quartile == `i'
}
*/

*ritest tracking _b[tracking], r(100) seed(0): reg stdR_totalscore tracking girl agetest_corr etpteacher zone* sbm visitno_corr yrstaught_corr, cluster(schoolid) nocons