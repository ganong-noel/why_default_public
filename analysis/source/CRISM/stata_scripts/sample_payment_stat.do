args scratch_dir crism_dir corelogic_dir

local scratch_dir = subinstr("`scratch_dir'", "scratch_dir:", "", .)
local crism_dir = subinstr("`crism_dir'", "crism_data:", "", .)

foreach month of numlist 306/458 {
use cnid li56 li59 li58 li102 li60 li112 li104 li103 li333 li218 li221 li239 li275 if mod(cnid,10)==1 using `crism_dir'equifax/36262_mcdash_primary_`month'_F.dta
save `scratch_dir'stata_sample/del_`month'.dta, replace
}
