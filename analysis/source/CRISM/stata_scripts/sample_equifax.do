args scratch_dir crism_dir corelogic_dir

local scratch_dir = subinstr("`scratch_dir'", "scratch_dir:", "", .)
local crism_dir = subinstr("`crism_dir'", "crism_data:", "", .)

foreach month of numlist 306/458{
use loan_id period cnid efx_period zipcode li56 li57 li58 li59 li60 li62 li63 li67 li68 li93 li104 li112 li215 li216 li332 li333 li334 li335 li336 li337 if mod(cnid,100)==11 using `crism_dir'/equifax/36262_mcdash_primary_`month'_F.dta
outsheet using `scratch_dir'stata_sample/main_`month'.csv, comma replace

// Create main sample by taking a 10% sub-sample of customers based on their Equifax ID using mod(cnid,100)==11:
use loan_id cnid efx_period conf bcn50 zipcode new_piggy li66 li69 li94 li107 li109 li110 li111 li113 li114 li115 li116 li322 li323 if mod(cnid,100)==11 using `crism_dir'equifax/update/36262_mcdash_primary_`month'_F_extravar.dta
outsheet using `scratch_dir'stata_sample/update_`month'.csv, comma replace
}
