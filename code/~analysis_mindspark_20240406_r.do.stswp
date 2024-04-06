/*
Author: Yazen Kashlan
Date: Apr 6, 2024
Objectives: Checking robustness of original results from Table 2
Note: first run globals in original script "analysis_mindspark_20180723.do"

*/


use "${el_clean}ms_batches", clear

** Generate count primary school-level batch members
gen pri_tot = grade<6 
gen pri_out = grade<6 & rctnonrct == "non-RCT"
byso group_id : egen pri_tot_ct = total(pri_tot) // inside RCT and 
byso group_id : egen pri_out_ct = total(pri_out) // outside

** Generate count low starting score members
sum mathstartinglevel // sample-wide median
gen low_math = mathstartinglevel<r(med) 
byso group_id : egen low_math_ct = total(low_math) 
byso group_id : egen batch_med = median(mathstartinglevel) 
egen overall_med = median(mathstartinglevel)
gen batch_below_med = batch_med < overall_med

** Center-by-batch identifier = strata as per paper. Not sure about actual match with strata var 
rename group_id strata 
tab strata

** Get strata-level controls
collapse (mean) mu_grade=grade mu_math=mathstartinglevel mu_lang=langstartinglevel ///
				pri_tot_ct pri_out_ct low_math_ct batch_below_med , by (strata)

label var mu_grade "Batch avg. grade"
label var mu_math  "Batch avg. entering math"
label var pri_out_ct "Batch untreated members in primary school"
label var low_math_ct "Batch members with below median math"

isid strata
tempfile batches
save 	`batches'

** combine datasets
use "${el_clean}ms_blel_jpal_wide", clear 
merge m:1 strata using `batches', keep(3) // ignore batches not in analysis file

** regressions
local stratafe i.strata
local dropstata "*strata*"

eststo clear
eststo: reg m_theta_mle2 i.treat m_theta_mle1 `stratafe', robust // original result
eststo: reg m_theta_mle2 treat##c.mu_grade m_theta_mle1 `stratafe', robust 
eststo: reg m_theta_mle2 treat##c.pri_out_ct m_theta_mle1 `stratafe', robust 
eststo: reg m_theta_mle2 treat##c.mu_math m_theta_mle1 `stratafe', robust 
eststo: reg m_theta_mle2 treat##c.low_math_ct m_theta_mle1 `stratafe', robust 
eststo: reg m_theta_mle2 treat##i.batch_below_med m_theta_mle1 `stratafe', robust 

** output
esttab _all using "${tables}/table5x.htm", ///
	cells(b(star fmt(2)) se(par fmt(2))) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) ///
	replace ///
	label varwidth(20) ///
	nomtitle ///
	drop(`dropstata' m_theta_mle1 0.treat*) ///
	stats(N, 					///
		  fmt(%9.0f %15s) 			/// 
		  labels("Observations")) 	///
	title("FS") ///
	addnotes("`note_local'")
	
	
	
	