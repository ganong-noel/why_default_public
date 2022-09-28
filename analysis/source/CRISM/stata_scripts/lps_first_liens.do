args scratch_dir crism_dir corelogic_dir

local scratch_dir = subinstr("`scratch_dir'", "scratch_dir:", "", .)
local crism_dir = subinstr("`crism_dir'", "crism_data:", "", .)

use LoanId State ClosingMonth OccupancyId PIFMonth MortgageTypeId if MortgageTypeId == 1 | MortgageTypeId == 4 using `crism_dir'mcdash/Loan.dta
export delimited `scratch_dir'all_loans.csv

