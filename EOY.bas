'FILENAME: EOY.bas
'PROGRAMMER: Ly, Max
'DATE: 27/06/2022
'VERSION: Final

'PURPOSE:
'The Purpose of this program is to compute and print a quarterly (3 monthly) customer billing report for water, sewage, and sanitation for the city of Paradise. 
'The user will also be prompted if they want a hard copy or a soft copy.

'INPUT:
'The program will dimension variables based on: Whether or not they have used sewage or rubbish collection,
'how many years they have been with the company and how much water they have consumed,
'their name, discount charge, total charge, and error flags/data validation.
'The program will read the Full name, Amount of years they have been with this company, 
'amount of liters their household has used over a quarter, 
'whether or not city sewage is used and whether or not city rubbish collection is provided.

'PROCESSING:
'Constants of the loyalty discount, the cost of water per liter dependent on water consumption (0L - 4000L, 4001L - 10000L, > 10000L), 
'cost per quarter for sewage, and the cost per quarter for rubbish collection are stored.
'The program will check for any invalid data, and if there is any, 
'it will skip the processing and will not add anything to the accumulated cost for all customers, saving the resource cost of the program. 
'The program will calculate the cost of water per liter based on the amount of water that a household consumes.
'Then, It will calculate the total cost by calculating the cost of water consumption and adding on sewage and/or rubbish collection cost if applicable.
'It will then calculate a discount based on the discount amount and the gross charge if the customer has been with the company for 10 or more years. 
'It will then add the net charge of the customer to an accumulated charge for all customers. 
'After that, the program will sort the customers and their data applicable into descending order based on the customers' water usage (liters). 
'It will then search for the customer with the highest water consumption and store it in a separate variable to the main array. 


'OUTPUT:
'The output for the program will include a main report title, a subtitle, 
'and column headings for each item (customer name, years that they have been with the company,
'liters of water used, whether or not sewage service is used, whether or not rubbish service is used, 
'discount amount for loyal customers and the total amount due per customer per quarter). 
'Column headings will need to be printed on 2 rows and must be fully descriptive. 
'The accumulated total for all charges for all the customers for the quarter is to be printed once only at the bottom of the report. 
'The customer name and the liters used for the customer who used the most liters of water during the quarter is to be printed below the accumulated total.
'It will either print this in a hard copy or a soft copy based on what the user has entered prior.


'------------------------------------------------------------------

'CLEARS SCREEN
CLS

'SET ALL ARRAYS TO START AT 1 INSTEAD OF 0
OPTION BASE 1

'CHANGES ARRAY SIZE OF ALL NON-CONSTANT ARRAYS
ARRAY.SIZE% = 10

'DIMENTION VARIABLES AND ARRAYS
DIM F.NAME(ARRAY.SIZE%) AS STRING
DIM YEARS(ARRAY.SIZE%) AS INTEGER
DIM USAGE(ARRAY.SIZE%) AS SINGLE
DIM SEWAGE(ARRAY.SIZE%) AS INTEGER
DIM RUBBISH(ARRAY.SIZE%) AS INTEGER
DIM WATER.CHG(4) AS SINGLE
DIM DISC.CHG(ARRAY.SIZE%) AS SINGLE
DIM TOTAL.CHG(ARRAY.SIZE%) AS SINGLE
DIM FLAG(ARRAY.SIZE%) AS SINGLE

'ASSIGNS CONSTANTS
WATER.CHG(1) = 0.1
WATER.CHG(2) = 0.12
WATER.CHG(3) = 0.15
WATER.CHG(4) = 0
SEWAGE.CHG% = 15
RUBBISH.CHG% = 25
DISC! = 0.1

'EXECUTES PROGRAM IN ORDER
'READING INPUT, DATA VALIDATION, MAIN LOGIC AND PROCESSING, SORTING THE DATA AND PRINTING THE REPORT
GOSUB GET.INPUT
GOSUB VALIDATION
GOSUB PROCESSING
GOSUB SORTING.DESC

'CHECKS IF USER REQUIRES A HARD COPY OR SOFT COPY OF THE REPORT
IF PRINT.OPTION$ = "Y" THEN
    GOSUB HARD.PRINT
ELSE
    GOSUB SOFT.PRINT
END IF

'TERMINATES EXECUTION
END

'DATA
DATA "N"
DATA "Ryan Bingham",-1,100,0,0
DATA "Brandon Ryder",1,-100,0,0
DATA "Shotgun Rider",1,100,10,10
DATA "Cody Canada",9,3999,1,1
DATA "Gretchen Peters",10,4000,1,1
DATA "Natalie Maines",11,4001,0,1
DATA "Cody Jinks",9,9999,1,0
DATA "Stoney LaRue",10,10000,0,0
DATA "Randy Rogers",11,10001,1,1
DATA "Reckless Kelly",1,0,0,0

'SUBROUTINE FOR READING DATA
'FIRST READS THE OPTION OF WHETHER IT WILL BE A HARD COPY OR SOFT COPY
'THEN GOES THROUGH EACH COLUMN AND READS EACH CUSTOMERS: YEARS WITH THE COMPANY, WATER USAGE, AND WHETHER OR NOT THEY HAVE SEWAGE AND RUBBISH SERVICE
GET.INPUT:

    READ PRINT.OPTION$
       FOR COLUMN% = 1 TO UBOUND(F.NAME)
        READ F.NAME(COLUMN%), YEARS(COLUMN%), USAGE(COLUMN%), SEWAGE(COLUMN%), RUBBISH(COLUMN%)
    NEXT COLUMN%

RETURN

'SUBROUTINE FOR DATA VALIDATION
'CHECKS IF THERE IS ANY DATA, PER COLUMN, THAT IS INVALID AND IF THERE IS, WILL ASSIGN AN ERROR FLAG TO THAT COLUMN IN THE DATA
VALIDATION:
    FOR COLUMN% = 1 TO UBOUND(F.NAME)
        IF (YEARS(COLUMN%) < 0) OR (USAGE(COLUMN%) < 0) OR ((SEWAGE(COLUMN%) <> 1) AND (SEWAGE(COLUMN%) <> 0)) OR ((RUBBISH(COLUMN%) <> 1) AND (RUBBISH(COLUMN%) <> 0)) THEN
            FLAG(COLUMN%) = 1
        ELSE
            FLAG(COLUMN%) = 0
        END IF

    NEXT COLUMN%

RETURN

'SUBROUTINE FOR THE MAIN LOGIC AND PROCESSING OF DATA
PROCESSING:
    ACC.CHG% = 0
    
    'LOOP FOR GOING THROUGH EACH COLUMN OF DATA
    FOR COLUMN% = 1 TO UBOUND(F.NAME)
        TOTAL.CHG(COLUMN%) = 0
        
        'IF THE USER HAS ANY INVALID DATA IT WILL SKIP PROCESSING
        IF FLAG(COLUMN%) = 0 THEN
        
        'CHECKS HOW MUCH WATER IS USED AND THEN ASSIGNS A VARIBLE FOR CALCULATION LATER
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
            
            'CALCULATION OF WATER COST
            TOTAL.CHG(COLUMN%) = USAGE(COLUMN%) * WATER.CHG(PERL%)
            
            'CALCULATION FOR SEWAGE AND RUBBISH USAGE
            IF SEWAGE(COLUMN%) + RUBBISH(COLUMN%) = 2 THEN
                TOTAL.CHG(COLUMN%) = TOTAL.CHG(COLUMN%) + SEWAGE.CHG% + RUBBISH.CHG%

            ELSEIF SEWAGE(COLUMN%) = 1 THEN
                TOTAL.CHG(COLUMN%) = TOTAL.CHG(COLUMN%) + SEWAGE.CHG%

            ELSEIF RUBBISH(COLUMN%) = 1 THEN
                TOTAL.CHG(COLUMN%) = TOTAL.CHG(COLUMN%) + RUBBISH.CHG%

            END IF
            
            ' CALCULATION FOR DISCOUNT CHARGE
            IF YEARS(COLUMN%) >= 10 THEN
                DISC.CHG(COLUMN%) = TOTAL.CHG(COLUMN%) * DISC!
                TOTAL.CHG(COLUMN%) = TOTAL.CHG(COLUMN%) - DISC.CHG(COLUMN%)
            END IF

            'CALCULATION FOR ACCUMULATED CHARGE
           ACC.CHG! = ACC.CHG! + TOTAL.CHG(COLUMN%)
           ELSE
           USAGE(COLUMN%) = COLUMN% * -1
        END IF

    NEXT COLUMN%

RETURN

'SUBROUTINE FOR SORTING DATA BASED ON WATER USAGE
'UTILISES A BUBBLE SORT TO SORT THE DATA IN DESCENDING ORDER BASED ON WATER USAGE
SORTING.DESC:

    FOR COUNT% = 1 TO UBOUND(F.NAME)
        FOR COUNTER% = 1 TO UBOUND(F.NAME)
            IF USAGE(COUNTER%) < USAGE(COUNT%) THEN
                GOSUB SWAPPER
            END IF
        NEXT COUNTER%
    NEXT COUNT%

RETURN

'SUBROUTINE FOR SWAPPING DATA FROM THE BUBBLE SORT
'THIS SUBROUTINE IS HERE SO THAT IF MORE ARRAYS ARE NEEDED TO BE ADDED TO THE PROGRAM, THE USER CAN PUT THE ARRAYS THAT NEED TO BE SORTED IN HERE
'THIS IS ALSO HERE SO I CAN SEE IF ANY MISTAKES HAPPEN DURING THE SWAPPING PROCESS
SWAPPER:
    SWAP F.NAME(COUNT%), F.NAME(COUNTER%)
    SWAP YEARS(COUNT%), YEARS(COUNTER%)
    SWAP USAGE(COUNT%), USAGE(COUNTER%)
    SWAP SEWAGE(COUNT%), SEWAGE(COUNTER%)
    SWAP RUBBISH(COUNT%), RUBBISH(COUNTER%)
    SWAP DISC.CHG(COUNT%), DISC.CHG(COUNTER%)
    SWAP TOTAL.CHG(COUNT%), TOTAL.CHG(COUNTER%)
    SWAP FLAG(COUNT%), FLAG(COUNTER%)
RETURN

'SUBROUTINE FOR PRINTING A SOFT COPY OF THE REPORT
SOFT.PRINT:

    PRINT TAB(26); "Paradise City Water Department"
    PRINT TAB(25); "Quarterly Customer Billing Report"
    PRINT TAB(25); "---------------------------------"
    PRINT
    PRINT TAB(3); "Customer"; TAB(15); "Account"; TAB(28); "Water"; TAB(38); "Sewage"; TAB(49); "Rubbish"; TAB(60); "Discount"; TAB(73); "Total"
    PRINT TAB(5); "Name"; TAB(17); "Age"; TAB(28); "Usage"; TAB(38); "Service"; TAB(49); "Service"; TAB(61); "Amount"; TAB(74); "Due"
    PRINT TAB(3); "--------"; TAB(15); "-------"; TAB(28); "-----"; TAB(38); "-------"; TAB(49); "-------"; TAB(60); "--------"; TAB(73); "-----"
    FOR COLUMN% = 1 TO UBOUND(F.NAME)
        PRINT USING "\        \"; TAB(3); F.NAME(COLUMN%);
        IF FLAG(COLUMN%) <> 0 THEN
            PRINT TAB(28); "RECORD CONTAINS INVALID DATA!"
        ELSE
            PRINT USING "## #####,.##L # # $$###,.## $$#####,.##"; TAB(15); YEARS(COLUMN%); TAB(23); USAGE(COLUMN%); TAB(38); SEWAGE(COLUMN%); TAB(49); RUBBISH(COLUMN%); TAB(57); DISC.CHG(COLUMN%); TAB(68); TOTAL.CHG(COLUMN%)
        END IF
    NEXT COLUMN%
    PRINT STRING$(80, 45)
    PRINT TAB(3); "Total Accumulated Charges = ";
    PRINT USING "$##,###.##"; TAB(68); ACC.CHG!

    PRINT TAB(3); "Customer With Highest Consumption:"
    PRINT USING "\        \ #####,.##L"; TAB(3); F.NAME(1); TAB(15); USAGE(1)

RETURN

'SUBROUTINE FOR PRINTING A HARD COPY OF THE REPORT
HARD.PRINT:
    LPRINT TAB(26); "Paradise City Water Department"
    LPRINT TAB(25); "Quarterly Customer Billing Report"
    LPRINT TAB(25); "---------------------------------"
    LPRINT
    LPRINT TAB(3); "Customer"; TAB(15); "Account"; TAB(28); "Water"; TAB(38); "Sewage"; TAB(49); "Rubbish"; TAB(60); "Discount"; TAB(73); "Total"
    LPRINT TAB(5); "Name"; TAB(17); "Age"; TAB(28); "Usage"; TAB(38); "Service"; TAB(49); "Service"; TAB(61); "Amount"; TAB(74); "Due"
    LPRINT TAB(3); "--------"; TAB(15); "-------"; TAB(28); "-----"; TAB(38); "-------"; TAB(49); "-------"; TAB(60); "--------"; TAB(73); "-----"
    FOR COLUMN% = 1 TO UBOUND(F.NAME)
        LPRINT USING "\        \"; TAB(3); F.NAME(COLUMN%);
        IF FLAG(COLUMN%) > 0 THEN
            LPRINT TAB(28); "RECORD CONTAINS INVALID DATA!"
        ELSE
            LPRINT USING "## #####,.##L # # $$###,.## $$#####,.##"; TAB(15); YEARS(COLUMN%); TAB(23); USAGE(COLUMN%); TAB(38); SEWAGE(COLUMN%); TAB(49); RUBBISH(COLUMN%); TAB(57); DISC.CHG(COLUMN%); TAB(68); TOTAL.CHG(COLUMN%)
        END IF
    NEXT COLUMN%
    LPRINT STRING$(80, 45)
    LPRINT TAB(3); "Total Accumulated Charges = ";
    LPRINT USING "$$#####,.##"; TAB(68); ACC.CHG!

    LPRINT TAB(3); "Customer With Highest Consumption:"
    LPRINT USING "\        \ #####,.##L"; TAB(3); F.NAME(1); TAB(15); USAGE(1)

RETURN
