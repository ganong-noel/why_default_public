args scratch_dir crism_dir corelogic_dir

local scratch_dir = subinstr("`scratch_dir'", "scratch_dir:", "", .)
local crism_dir = subinstr("`crism_dir'", "crism_data:", "", .)

foreach month of numlist 305/455 {
use LoanId PaymentStatus UPB Investorid InterestRate using `crism_dir'mcdash/LoanMonth_`month'.dta
rename Investorid InvestorId
save `scratch_dir'stata_sample/defaulters/default_`month'.dta, replace
}
