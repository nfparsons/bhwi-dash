clear
import excel "C:\Users\viralu\Desktop\workforce grant\funds.xlsx", sheet("Sheet1") firstrow

ta RepresentedStatus
ta Approvedfor
tabstat AmountApproved, stats (n min max median p25 p75)


ta PostSurveySent


tab  RepresentedStatus Approvedfor, col chi