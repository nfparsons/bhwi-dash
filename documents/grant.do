clear
import excel "C:\Users\viralu\Desktop\workforce grant\grant.xlsx", sheet("Sheet1") firstrow

ta Status
ta Race
ta GenderIdentity
ta SexualOrientation
ta fundingfor

ta program
ta Program
ta managersapproval
ta RepresentedStatus
ta AmountApproved
tabstat AmountApproved, stats (n min max median p25 p75)
tab Program RepresentedStatus, col chi
tab RepresentedStatus Program, col chi
tab  fundingfor Program, col chi


keep if Status=="Approved"

ta Status
ta Race
ta GenderIdentity
ta SexualOrientation
ta fundingfor

ta program
ta Program
ta managersapproval
ta RepresentedStatus

tabstat AmountApproved, stats (n min max median p25 p75)
tab Program RepresentedStatus, col chi
tab  fundingfor Program, col chi


keep if Program=="Quality Management"

ta Race
ta GenderIdentity
ta SexualOrientation
ta fundingfor
ta Program
ta managersapproval
ta RepresentedStatus











Data Analysis for Q4 FY2024

clear
import excel "C:\Users\viralu\Desktop\workforce grant\grant2.xlsx", sheet("grant1") firstrow

ta Status
ta Race
ta GenderIdentity
ta SexualOrientation
ta fundingfor
ta Fundingfor
ta program
ta Program
ta managersapproval
ta RepresentedStatus

tabstat AmountApproved, stats (n min max median p25 p75)
tab Program RepresentedStatus, col chi
tab  Fundingfor Program, col chi


keep if Status=="Approved"

ta Status
ta Race
ta GenderIdentity
ta SexualOrientation
ta fundingfor
ta Fundingfor
ta program
ta Program
ta managersapproval
ta RepresentedStatus

tabstat AmountApproved, stats (n min max median p25 p75)
tab Program RepresentedStatus, col chi
tab  Fundingfor Program, col chi



keep if Program=="Quality Management"

ta Race
ta GenderIdentity
ta SexualOrientation
ta Fundingfor
ta Program
ta managersapproval
ta RepresentedStatus



