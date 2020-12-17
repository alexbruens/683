* 683
* Alex Bruens

import excel using "/Users/alex/Desktop/Fall2020/683/683finaldata/ucdp-term-conf-2015.xlsx", firstrow

drop if Type2 != 3
destring GWNoLoc, replace

drop AJ
drop if Outcome ==.

gen startyear = year(StartDate)

gen endyear = substr(EpEndDate, 6, 4)
destring endyear, replace
replace EpEndDate = "30dec2010" in 141
destring endyear, replace

gen agreementyear = substr(PeAgDate, 6, 4)
destring agreementyear, replace

* v2cseeorgs-v2pepwrgen_ord v2cseeorgs_1lead-v2pepwrgen_ord_5lead

use "/Users/alex/Desktop/Fall2020/683/683finaldata/intrastateconflictterm_outcomes.dta"
rename GWNoLoc gwno
rename endyear year

merge m:1 gwno year using "/Users/alex/Desktop/Fall2020/683/683finaldata/vdem_csovars_yearlags.dta", keepusing(v2cseeorgs-v2pepwrgen_ord v2cseeorgs_1lead-v2pepwrgen_ord_5lead)
drop if _merge!=3

save "/Users/alex/Desktop/Fall2020/683/683finaldata/finaldata_endyear.dta"

use "/Users/alex/Desktop/Fall2020/683/683finaldata/intrastateconflictterm_outcomes.dta"
rename GWNoLoc gwno
rename agreementyear year

merge m:1 gwno year using "/Users/alex/Desktop/Fall2020/683/683finaldata/vdem_csovars_yearlags.dta", keepusing(v2cseeorgs-v2pepwrgen_ord v2cseeorgs_1lead-v2pepwrgen_ord_5lead)
drop if _merge!=3
save "/Users/alex/Desktop/Fall2020/683/683finaldata/finaldata_agreementyear.dta"


* PTS data, unused
use "/Users/alex/Downloads/PTS-2020.dta"
rename Country country
rename Year year
drop gwno 
do "/Users/alex/Desktop/Fall2020/683/683finaldata/gwno_codes.do"
duplicates tag gwno year, gen(dup)
tab dup
tab country if dup ==1

drop dup
drop gwno
rename COW_Code_N gwno

merge 1:1 gwno year using "/Users/alex/Desktop/Fall2020/683/683finaldata/vdem_csovars_yearlags.dta"

foreach var of varlist PTS_A-PTS_S {
    bysort gwno (year): gen `var'_1lead= `var'[_n+1]
    }

save "/Users/alex/Desktop/Fall2020/683/683finaldata/vdemwithPTSlead.dta"

use "/Users/alex/Desktop/Fall2020/683/683finaldata/finaldata_endyear.dta"
drop _merge
merge m:1 gwno year using "/Users/alex/Desktop/Fall2020/683/683finaldata/vdemwithPTSlead.dta", keepusing(PTS_A-PTS_S PTS_A_1lead-PTS_S_1lead)
drop if _merge !=3
save "/Users/alex/Desktop/Fall2020/683/683finaldata/finaldata_endyear_PTS.dta"
