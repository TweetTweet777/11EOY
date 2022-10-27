'CLEARS SCREEN
CLS

'SET ALL ARRAYS TO START AT 1 INSTEAD OF 0
OPTION BASE 1

'DIMENTION VARIABLES AND ARRAYS
DIM F.NAME(20) AS STRING
DIM YEARS(20) AS INTEGER
DIM USAGE(20) AS INTEGER
DIM SEWAGE(20) AS STRING
DIM RUBBISH(20) AS STRING
DIM WATER.CHG(1 TO 4) AS INTEGER
DIM DISC.CHG(20) AS INTEGER
DIM TOTAL.CHG(20) AS INTEGER
DIM ACC.CHG AS INTEGER
DIM FLAG(20) AS SINGLE

'ASSIGNS CONSTANTS
WATERCHG(1) = 0.1
WATERCHG(2) = 0.12
WATERCHG(3) = 0.15
WATERCHG(4) = 0
SEWAGECHG% = 15
RUBBISHCHG% = 25
DISC% = 0.1

GOSUB READ.INPUT

GOSUB VALIDATION

GOSUB PROCESSING

GOSUB SORTING.DESC

IF PRINT.OPTION$ = "Y" THEN
    GOSUB HARD.PRINT
ELSE
    GOSUB SOFT.PRINT
END IF

END

'DATA
DATA "Y"
DATA


READ.INPUT:
READ READ.OPTION$
FOR COLUMN% = 1 TO UBOUND(F.NAME)

    READ F.NAME(COLUMN%), YEARS(COLUMN%), USAGE(COLUMN%), SEWAGE(COLUMN%), RUBBISH(COLUMN%)

NEXT COLUMN%

RETURN

VALIDATION:
FOR COLUMN% = 1 TO UBOUND(F.NAME)
    IF YEARS(COLUMN%) < 0 OR USAGE(COLUMN%) < 0 OR SEWAGE(COLUMN%) <> "Y" OR SEWAGE(COLUMN%) <> "N" OR RUBBISH(COLUMN%) <> "Y" OR RUBBISH(COLUMN%) <> "N" OR PRINT.OPTION$ <> "Y" OR PRINT.OPTION$ <> "N" THEN
        FLAG(COLUMN%) = 1
    ELSE
        FLAG(COLUMN%) = 0
    END IF

NEXT COLUMN%

RETURN

PROCESSING:
FOR COLUMN% = 1 TO UBOUND(F.NAME)

    IF FLAG(COLUMN%) = 0 THEN
        SELECT CASE USAGE(COLUMN%)
            CASE 0 TO 4000
                PERL% = 1
            CASE 4001 TO 10000
                PERL% = 2
            CASE IS > 10000
                PERL% = 3
            CASE ELSE
                FLAG(COLUMN%) = 5
                PERL% = 4

        END SELECT
        TOTAL.CHG(COLUMN%) = USAGE(COLUMN%) * WATER.CHG(PERL%)

        IF SEWAGE(COLUMN%) = "Y" THEN
            TOTAL.CHG(COLUMN%) = TOTAL.CHG(COLUMN%) + SEWAGECHG%
        END IF
        IF RUBBISH(COLUMN%) = "Y" THEN
            TOTAL.CHG(COLUMN%) = TOTAL.CHG(COLUMN%) + RUBBISHCHG%
        END IF

        IF YEARS(COLUMN%) >= 10 THEN
            DISC.CHG(COLUMN%) = TOTAL.CHG(COLUMN%) * DISC%
            TOTAL.CHG(COLUMN%) = TOTAL.CHG(COLUMN%) * (1 - DISC%)
        END IF
        ACC.CHG% = ACC.CHG% + TOTAL.CHG(COLUMN%)
    END IF

NEXT COLUMN%

RETURN

SORTING.DESC:

FOR COUNT% = 1 TO UBOUND(F.NAME)
    FOR COUNTER% = 1 TO UBOUND(F.NAME)
        IF USAGE(COUNTER%) > USAGE(COUNT%) THEN
            GOSUB SWAPPER
        END IF
    NEXT COUNTER%
NEXT COUNT%

RETURN

SWAPPER:
SWAP F.NAME(COUNT%), F.NAME(COUNTER%)
SWAP YEARS(COUNT%), YEARS(COUNTER%)
SWAP USAGE(COUNT%), USAGE(COUNTER%)
SWAP SEWAGE(COUNT%), SEWAGE(COUNTER%)
SWAP RUBBISH(COUNT%), RUBBISH(COUNTER%)
SWAP DISC.CHG(COUNT%), DISC.CHG(COUNTER%)
SWAP TOTAL.CHG(COUNT%), TOTAL.CHG(COUNTER%)
RETURN

SOFT.PRINT:

PRINT TAB(26); "Paradise City Water Department"
PRINT TAB(25); "Quarterly Customer Billing Report"
PRINT TAB(25); "---------------------------------"
PRINT
PRINT TAB(3); "Customer"; TAB(15); "Account"; TAB(28); "Water"; TAB(38); "Sewage"; TAB(49); "Rubbish"; TAB(60); "Discount"; TAB(73); "Total"
PRINT TAB(5); "Name"; TAB(17); "Age"; TAB(28); "Usage"; TAB(38); "Service"; TAB(49); "Service"; TAB(61); "Amount"; TAB(74); "Due"
PRINT TAB(3); "--------"; TAB(15); "-------"; TAB(28); "-----"; TAB(38); "-------"; TAB(49); "-------"; TAB(60); "--------"; TAB(73); "-----"
FOR COLUMN% = 1 TO UBOUND(F.NAME)
    PRINT TAB(3); F.NAME(COLUMN%);
    IF FLAG(COLUMN%) > 0 THEN
        PRINT "RECORD CONTAINS INVALID DATA!"
    ELSE
        PRINT USING "## #####,.##L & & $$#####,.## $$#####,.##"; TAB(15); YEARS(COLUMN%); TAB(28); USAGE(COLUMN%); TAB(38); SEWAGE(COLUMN%); TAB(49); RUBBISH(COLUMN%); TAB(60); DISC.CHG(COLUMN%); TAB(73); TOTAL.CHG(COLUMN%)

    END IF
NEXT COLUMN%
PRINT STRING$(80, 45)
PRINT TAB(3); "Total Accumulated Charges = ";
PRINT USING "$$#####,.##"; ACC.PAY

PRINT TAB(3); "Customer With Highest Consumption:"
PRINT USING "& #####,.##L"; TAB(3); F.NAME(1); TAB(15); USAGE(COLUMN%)



RETURN

HARD.PRINT:
LPRINT TAB(26); "Paradise City Water Department"
LPRINT TAB(25); "Quarterly Customer Billing Report"
LPRINT TAB(25); "---------------------------------"
LPRINT
LPRINT TAB(3); "Customer"; TAB(15); "Account"; TAB(28); "Water"; TAB(38); "Sewage"; TAB(49); "Rubbish"; TAB(60); "Discount"; TAB(73); "Total"
LPRINT TAB(5); "Name"; TAB(17); "Age"; TAB(28); "Usage"; TAB(38); "Service"; TAB(49); "Service"; TAB(61); "Amount"; TAB(74); "Due"
LPRINT TAB(3); "--------"; TAB(15); "-------"; TAB(28); "-----"; TAB(38); "-------"; TAB(49); "-------"; TAB(60); "--------"; TAB(73); "-----"
FOR COLUMN% = 1 TO UBOUND(F.NAME)
    LPRINT TAB(3); F.NAME(COLUMN%);
    IF FLAG(COLUMN%) > 0 THEN
        LPRINT "RECORD CONTAINS INVALID DATA!"
    ELSE
        LPRINT USING "## #####,.##L & & $$#####,.## $$#####,.##"; TAB(15); YEARS(COLUMN%); TAB(28); USAGE(COLUMN%); TAB(38); SEWAGE(COLUMN%); TAB(49); RUBBISH(COLUMN%); TAB(60); DISC.CHG(COLUMN%); TAB(73); TOTAL.CHG(COLUMN%)

    END IF
NEXT COLUMN%
LPRINT STRING$(80, 45)
LPRINT TAB(3); "Total Accumulated Charges = ";
LPRINT USING "$$#####,.##"; ACC.PAY

LPRINT TAB(3); "Customer With Highest Consumption:"
LPRINT USING "& #####,.##L"; TAB(3); F.NAME(1); TAB(15); USAGE(COLUMN%)

RETURN
