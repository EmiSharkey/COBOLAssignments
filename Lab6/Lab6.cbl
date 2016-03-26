      * Program Name:  Lab6.cbl 
      * Author:        Emilie Sharkey
      * Date:          2015-03-04
      * Version:       1.0
      * Description:   Analyzes the information from the provided lab6.dat file in order
      *                to produce file output stating the employee's number, name, 
      *                number of years, position, current salary, percentage of pay
      *                percentage of pay increase, total increase of salary,
      *                and new salary. 
      *            
      * Based Off:     COBOL PROGRAMMING ASSIGNMENT LAB 6 by Andrew Mayne 
      * See:           https://durhamcollege.desire2learn.com/d2l/common/viewFile.d2lfile/Database/MTYxOTc5OQ/lab6.dat?ou=139751
  
       identification division.
       program-id. lab6.
       author. Emilie.

       environment division.
       
       input-output section.
        
       file-control.
      *    employee-info will be assigned to the file path given as a reference. The file organization will be line sequential 
           select employee-info assign to "../../employee/lab6.dat" 
           organization is line sequential.
           
      *    employee-records will be assigned to the file path given as a reference. The file organization will be line sequential
           select employee-records assign to display
      *    "employeeInfo.out"
            organization is line sequential.
           
       data division.
       
       file section.
       
      *    file description for employee-info that will states that employee-record will be used to store the lines from the file
           fd employee-info
               data record is employee-record.
       
      *    employee-record is a record that uses each of its different elementary items to store a different section 
      *    protaining of employee-record as a type of employee information accessible by the application
           01 employee-record. 
               05 er-employee-number       pic x(3).
               05 er-employee-name         pic x(15).        
               05 er-employee-years        pic x(2).
               05 er-employee-code         pic x.
               05 er-employee-salary       pic 9(5)v9(2).     
           
      *    file description for employee-records that will states that employee-line will be used to store the lines from the file
           fd employee-records
               data record is employee-line.
      *    blank record that will be used to write data from the application into employee-records  
           01 employee-line pic x(92).
               
       working-storage section.
      *    ws-end-file is used as an indicator that the end of file has been reached 
           77  ws-end-file                 pic x(5)
               value "TRUE".  
      *    ws-page-increment is a constant numerical record storing the number of pages that the application increments by  
           77 ws-page-increment            pic 9
               value  1.
      *    ws-percent-sign is a constant character record storing the percentage sign to be called upon
           77 ws-percent-sign              pic x
               value "%".
      *    ws-plus-sign is a constant character record storing the plus sign to be called upon        
           77 ws-plus-sign                 pic x
               value "+".    
      *    ws-plus-sign is a constant character record storing the character representing a graduate employee      
           77 ws-graduate                  pic x
               value "G".
      *    ws-plus-sign is a constant character record storing the character representing a non-graduate employee      
           77 ws-non-graduate              pic x
               value "N".    
      *    ws-analyst-years is a constant numerical record storing the number of years required to be an analyst   
           77 ws-analyst-years             pic 99
               value 15.
      *    ws-analyst is a constant character record storing the how analyst position name will be written  
           77 ws-analyst                   pic x(7)
               value "ANALYST".
      *    ws-senior-programmer  is a constant character record storing the how senior programmer position name will be written      
           77 ws-senior-programmer         pic x(8)
               value "SEN PROG".
      *    ws-programmer  is a constant character record storing the how programmer position name will be written      
           77 ws-programmer                pic x(4)
               value "PROG".
      *    ws-junior-programmer is a constant character record storing the how programmer position name will be written          
           77 ws-junior-programmer         pic x(7)
               value "JR PROG".    
      *    ws-senior-min is a constant numerical record storing the number of years minimum required to be a senior programmer   
           77 ws-senior-min                pic 99
               value 7.
      *    ws-programmer-years-minimum is a constant numerical record storing the number of years minimum required to be a programmer       
           77 ws-programmer-years-minimum  pic 99
               value 10. 
      *    ws-programmer-graduate-max is a constant numerical record storing the number of years maximum to be a programmer           
           77 ws-programmer-graduate-max   pic 99
               value 7.
      *    ws-minimum-junior-programmer is a constant numerical record storing the number of years minimum required to be a junior programmer  
           77 ws-minimum-junior-programmer pic 99
               value 4.
      *    ws-graduate-unclassified is a constant numerical record storing the number of years maximum to be labelled as unclassified while being
      *    a graduate
           77 ws-graduate-unclassified     pic 99
               value 2.      
      *    ws-double-space is a numerical record storing number of lines required to be advance for double spacing to occur
           77 ws-double-space              pic 9
               value 2.    
      *    ws-single-space is a numerical record storing number of lines required to be advance for single spacing to occur    
           77 ws-single-space              pic 9
               value 1.
      *    ws-line-per-page is a numerical record storing number of lines permitted per page     
           77 ws-line-per-page             pic 99
               value 15.
      *    ws-percent-analyst is a numerical record storing the percentage of pay increase for analysts
           77 ws-percent-analyst           pic 99V9
               value 11.9.     
      *    ws-percent-senior-programmer is a numerical record storing the percentage of pay increase for senior programmers
           77 ws-percent-senior-programmer pic 9V9
               value 9.3. 
      *    ws-percent-programmer is a numerical record storing the percentage of pay increase for programmers
           77 ws-percent-programmer        pic 9V9
               value 6.7. 
      *    ws-percent-junior-programmer is a numerical record storing the percentage of pay increase for junior programmers 
           77 ws-percent-junior-programmer pic 9V9
               value 3.7.
      *   ws-percent-unclassified is a numerical record storing the percentage of pay increase for unclassified workers
          77 ws-percent-unclassified       pic 99
               value 0. 
      *   ws-page-start stores the first numerical value that the page starts on    
          77 ws-page-start                 pic 99
               value 1.      
      *   employee-display-record is a record that uses its elementary items to display employee information in a readable output for the user     
          01 employee-display-record. 
               05  filler                  pic x(1).      
               05  edr-number              pic x(3).
               05  filler                  pic x(2).
               05  edr-name                pic x(15).
               05  filler                  pic x(8).
               05  edr-years               pic z9.
               05  filler                  pic x(3).
               05  edr-position            pic x(8).
               05  filler                  pic x(4).     
               05  edr-salary              pic zz,zz9.9(2).
               05  filler                  pic x(2).
               05  edr-increase-percent    pic zzz.z.
               05  edr-percent-sign        pic x
                   value "%".
               05  filler                  pic x(2).
               05  edr-increase            pic $$$$,$$9.99. 
               05  edr-plus-sign           pic x
                   value "+".
               05  filler                  pic x(2). 
               05  edr-new-salary          pic $zzz,zzz.99.    
               
      *    ws-page-line stores the line count for the page as a numerical value     
           01  ws-page-line                pic 99.   
      *    ws-page-number stores the page count as a numerical value    
           01  ws-page-number              pic 999.  
      *    ws-percent-store stores the percentage value of the employee's pay increase as a numerical value
           01  ws-percent-store            pic 9(3)v9(2). 
      *    ws-pay-increase stores the calculated employee pay increase as a numerical value
           01  ws-pay-increase             pic 9(6)v9(2).   
      *    ws-new-salary stores the calculated employee new salary as a numerical value
           01  ws-new-salary               pic 9(6)v9(2).
      *    ws-analyst-count stores the number of analyst processed as a numerical value
           01  ws-analyst-count            pic 999. 
      *    ws-analyst-increase stores the total analyst salary pay increase as a numerical value
           01  ws-analyst-increase         pic 9(7)v9(2).   
      *    ws-senior-count stores the number of senior programmers processed as a numerical value
           01  ws-senior-count             pic 999. 
      *    ws-senior-increase stores the total senior programmer pay increase as a numerical value
           01  ws-senior-increase          pic 9(7)v9(2).   
      *    ws-programmer-count stores the number of programmers processed as a numerical value
           01  ws-programmer-count         pic 999. 
      *    ws-programmer-increase stores the total programmer pay increase as a numerical value
           01  ws-programmer-increase      pic 9(7)v9(2).   
      *    ws-programmer-count stores the number of junior programmers processed as a numerical value
           01  ws-junior-count             pic 999. 
      *    ws-programmer-increase stores the total junior programmer pay increase as a numerical value
           01  ws-junior-increase          pic 9(7)v9(2).   
      *    ws-end-file-check is used to check if end of file has been reached
           01  ws-end-file-check           pic x(5) 
               value "FALSE". 
      *   first-line is a record that uses its elementary items to store the author name, program name, time, and date to be written to the file      
           01  first-line.
                   05  filler              pic x(5)  value spaces.
                   05  filler              pic x(28)
                           value "Emilie Sharkey, Lab 6".         
                   05  filler              pic x(5)  value spaces.
                   05  fl-date-display     pic x(6).
                   05  filler              pic x(5)  value spaces.
                   05  fl-time-display     pic x(7).   
                   
      *    top-page is a record that uses its elementary items to store the document type and page number to be written to the file      
           01  top-page.
               05  filler              pic x(15).
               05  filler              pic x(22)
                   value "EMPLOYEE SALARY REPORT".
               05  filler              pic x(24).
               05  filler              pic x(4)
                   value "PAGE".
               05  tp-page-number      pic zz9.
               
      *    heading-top-column is a record that uses its elementary items to store the top part of column names to be written to the file          
           01  heading-top-column.
               05  filler              pic x(1)  value spaces.
               05  filler              pic x(3)  value "EMP".
               05  filler              pic x(3)  value spaces.
               05  filler              pic x(4)  value "EMP".
               05  filler              pic x(34)  value spaces.   
               05  filler              pic x(8)  value "PRESENT".
               05  filler              pic x(3)  value spaces.   
               05  filler              pic x(8)  value "INCREASE".
               05  filler              pic x(3)  value spaces.
               05  filler              pic x(4)  value "PAY".
               05  filler              pic x(9)  value spaces.
               05  filler              pic x(6)  value "NEW".
      *    heading-top-column-below is a record that uses its elementary items to store the bottom part of column names to be written to the file
           01  heading-top-column-below.
               05  filler              pic x(1)  value spaces.
               05  filler              pic x(3)  value "NUM".
               05  filler              pic x(3)  value spaces.
               05  filler              pic x(4)  value "NAME".
               05  filler              pic x(15)  value spaces.
               05  filler              pic x(5)  value "YEARS".
               05  filler              pic x(3)  value spaces.
               05  filler              pic x(9)  value "POSITION".
               05  filler              pic x(2)  value spaces.
               05  filler              pic x(6)  value "SALARY".
               05  filler              pic x(9)  value spaces.
               05  filler              pic x(1)  value "%".
               05  filler              pic x(5)  value spaces.
               05  filler              pic x(8)  value "INCREASE".
               05  filler              pic x(5)  value spaces.
               05  filler              pic x(6)  value "SALARY".    
      *    average-increases-top is a record that uses its elementary items to store the positional page increase top part of
      *    averages to be written to the file
           01  average-increases-top. 
               05  filler              pic x(19)  
                   value "AVERAGE INCREASES: ".
               05  filler              pic x(3)  
                   value spaces.
               05  filler              
                   pic x(9)  value "ANALYST= ".
               05  ait-analyst          pic zzz,zz9.99.
               05  filler              pic x(3)  
                   value spaces.
               05  filler              
                   pic x(10)  value "SEN PROG= ".
               05  ait-senior          pic zzz,zz9.99.
               05  filler              pic x(3)  
                   value spaces. 
      *     average-increases-bottom is a record that uses its elementary items to store the positional page increase bottom part of
      *     averages to be written to the file       
            01  average-increases-bottom. 
               05  filler              pic x(22)
                   value spaces.
               05  filler              
                   pic x(5)  value "PROG= ".
               05  filler              pic x(4)
                   value spaces.    
               05  aib-programmer      pic zzz,zz9.99.
               05  filler              pic x(3)  
                   value spaces.
               05  filler              
                   pic x(8)  value "JR PROG=".
               05  filler              pic x(2)  
                   value spaces. 
               05  aib-junior          pic zzz,zz9.99.
                       
       procedure division.     
       
      * the employee-info file is opened as a readable, and employee-records is opened as a writable
        open input employee-info,
        output employee-records.
                
      * fl-date-display stores the present date 
        accept fl-date-display from date.
            
      * fl-time-display stores the present time
        accept fl-time-display from time.
        
      * ws-page-increment is equal to ws-page-number    
        move ws-page-increment to ws-page-number.
            
      * ws-page-number is equal to tp-page-number
        move ws-page-number to tp-page-number.
         
      * writes the first-line, top-page, heading-top-column, and heading-top-column-below records at the top of the page 
        perform 100-inital-heading.
        
      * employee-info is read and ws-end-file is set equal to ws-end-file-check when the end of file is reached
        read employee-info at end move ws-end-file to ws-end-file-check.
         
      * 150-employee-information-records is performed until ws-end-file-check is equal to ws-end-file  
        perform 150-employee-information-records until  ws-end-file-check = ws-end-file.
         
      *  ait-analyst is equal to the the rounded value of ws-analyst-increase divided by ws-analyst-count
         compute ait-analyst rounded = ws-analyst-increase/ws-analyst-count.
      *  ait-senior is equal to the the rounded value of ws-senior-increase divided by ws-senior-count
         compute ait-senior rounded = ws-senior-increase/ws-senior-count.
      *  aib-programmer is equal to the the rounded value of ws-programmer-increase divided by ws-programmer-count
         compute aib-programmer rounded = ws-programmer-increase/ws-programmer-count.
      *  aib-junior is equal to the the rounded value of ws-junior-increase divided by ws-junior-count
         compute aib-junior rounded= ws-junior-increase/ws-junior-count.
         
      * average-increases-top is written to employee-line  
         write employee-line from average-increases-top.
      * average-increases-bottom is written to employee-line    
         write employee-line from average-increases-bottom.
         
      *  employee-info and employee-records files are closed
         close employee-info, employee-records.
         
      *  pauses application until enter key is pressed
         accept return-code.
      
      * stops the application run    
       stop run.
       
      * Code run when perform 100-inital-heading is called upon 
       100-inital-heading.
      * first-line is written to employee-line  
        write employee-line from first-line. 
      * top-page is written to employee-line after skipping ws-single-space amount of lines    
        write employee-line from top-page after advancing ws-single-space line.
      * heading-top-column is written to employee-line after skipping ws-double-space amount of lines   
        write employee-line from heading-top-column after advancing ws-double-space line.  
      * heading-top-column-below is written to employee-line  
        write employee-line from heading-top-column-below.  
      * prints a blank line to employee-line
        write employee-line from spaces.
        
      * Code run when perform 150-employee-information-records is called upon   
       150-employee-information-records. 
      *    ws-page-start is added to the value of ws-page-line  
           add ws-page-start to ws-page-line.
      *    if the value of ws-page-line is greater than the value of ws-line-per-page
            if (ws-page-line greater ws-line-per-page) 
      *        ws-page-number is incremented by one
               add 1 to ws-page-number
      *        ws-page-line is set to zero        
               move 1 to ws-page-line
      *        tp-page-number is set to the value of ws-page-number     
               move ws-page-number to tp-page-number
      *        Code from 200-page-heading is run to print information to the top of the new page including author name, date, time, page number, and column names
               perform 200-page-heading
      *     closes if statement
            end-if.
            
      *    er-employee-number value is set equal to er-employee-number
           move er-employee-number to edr-number.
      *    er-employee-name value is set equal to er-employee-name
           move er-employee-name to edr-name.
      *    er-employee-years value is set equal to er-years
           move er-employee-years to edr-years.
      *    er-employee-salary value is set equal to er-salary
           move er-employee-salary to edr-salary.
           
      *    if er-employee-code is the same as ws-graduate
           if er-employee-code equal ws-graduate
      *        run the code from 250-graduate section that classifies the graduate employee's position
               perform 250-graduate
      *    otherwise if er-employee-code is the same as ws-non-graduate
           else if er-employee-code equal ws-non-graduate
      *        run the code from 300-non-graduate section that classifies the not graduated employee's position
               perform 300-non-graduate
           end-if.
           
      *    make edr-new-salary value equal to ws-new-salary
           move ws-new-salary to edr-new-salary.
      *    make edr-increase-percent value equal to ws-percent-store   
           move ws-percent-store to edr-increase-percent.
      *    make edr-increase value equal to ws-pay-increase    
           move ws-pay-increase to edr-increase.
      *    make edr-plus-sign value equal to ws-plus-sign     
           move ws-plus-sign to edr-plus-sign.
           
      *    employee-display-record is written into employee-line 
           write employee-line from employee-display-record.
      *    read from employee-info and make ws-end-file equal to ws-end-file-check when end of file is reached
           read employee-info at end move ws-end-file to ws-end-file-check.
      *    write a blank line to employee-line 
           write employee-line from spaces.
          
      * Code run when perform 200-page-heading is called upon    
       200-page-heading.
      *  first-line is written to employee-line after a page is advanced
         write employee-line from first-line after advancing page.
      *  top-page is written to employee-line after advancing ws-double-space lines
         write employee-line from top-page after advancing ws-double-space line.
      *  heading-top-column is written to employee-line
         write employee-line from heading-top-column .  
      *  heading-top-column-below is written to employee-line
         write employee-line from heading-top-column-below.  
      *  a blank line is written to employee-line 
         write employee-line from spaces.
         
      *Code run when perform 250-graduate is called upon   
       250-graduate.
      * if the value of er-employee-years is greater than ws-analyst-years
        if (er-employee-years > ws-analyst-years)
      *    runs the code that corresponds to the analyst position so that the position, the calculated pay can be totaled with the 
      *    corrresponding percent increase, and the averages for the position can be calculated and properly written to file
           perform 350-analyst
           
      * otherwise if the value of er-employee-years is greater or equal to ws-senior-min
        else if (er-employee-years >= ws-senior-min)
      *    runs the code that corresponds to the senior programmer position so that the position, the calculated pay can be totaled with the 
      *    corrresponding percent increase, and the averages for the position can be calculated and properly written to file
           perform 400-senior-programmer
           
      * otherwise if the value of er-employee-years less then ws-graduate-unclassified and more than ws-graduate-unclassified
        else if  (er-employee-years < ws-programmer-graduate-max and er-employee-years > ws-graduate-unclassified)
      *    runs the code that corresponds to the programmer position so that the position, the calculated pay can be totaled with the 
      *    corrresponding percent increase, and the averages for the position can be calculated and properly written to file 
           perform 450-programmer
           
      *    if the value er-employee-years is less than or equal to er-employee-years 
           else 
      *    runs the code that corresponds to unclassified so that the calculated pay can be totaled with the corrresponding percent increase, 
      *    and the averages for the position can be calculated and properly written to file 
            perform 600-not-classified    
      *    closes the if statement 
           end-if.
       
      * Code run when perform 300-non-graduate is called upon 
       300-non-graduate.
        if (er-employee-years > ws-programmer-years-minimum)
      *    runs the code that corresponds to the programmer position so that the position, the calculated pay can be totaled with the 
      *    corrresponding percent increase, and the averages for the position can be calculated and properly written to file 
           perform 450-programmer
        else if (er-employee-years > ws-minimum-junior-programmer)
      *    runs the code that corresponds to the junior programmer position so that the position, the calculated pay can be totaled with the 
      *    corrresponding percent increase, and the averages for the position can be calculated and properly written to file 
           perform 550-junior-programmer
        else    
      *    runs the code that corresponds to unclassified so that the calculated pay can be totaled with the corrresponding percent increase, 
      *    and the averages for the position can be calculated and properly written to file   
           perform 600-not-classified    
      *    closes the if statement 
           end-if.
           
      * Code run when perform 350-analyst is called upon  
       350-analyst.
      *    edr-position is made equal to ws-analyst 
           move ws-analyst to edr-position.
      *    ws-percent-store is made equal to ws-percent-analyst
           move ws-percent-analyst to ws-percent-store.
      *    650-calculate-salary code is run in order to perform the calculations for ws-pay-increase, and determines if edr-percent-sign is blank   
           perform 650-calculate-salary.
      *    increments the ws-analyst-count by one   
           add 1 to ws-analyst-count.
      *    ws-pay-increase is added to ws-analyst-increase 
           add ws-pay-increase to ws-analyst-increase.
           
      * Code run when perform 400-senior-programmer is called upon      
       400-senior-programmer.
      *    edr-position is made equal to ws-senior-programmer 
           move ws-senior-programmer to edr-position.
      *    ws-percent-store is made equal to ws-percent-senior-programmer
           move ws-percent-senior-programmer to ws-percent-store.
      *    650-calculate-salary code is run in order to perform the calculations for ws-pay-increase, and determines if edr-percent-sign is blank   
           perform 650-calculate-salary.
      *    increments the ws-senior-count by one      
           add 1 to ws-senior-count.
      *    ws-pay-increase is added to ws-senior-increase 
           add ws-pay-increase to ws-senior-increase.
       
      * Code run when perform 450-programmer is called upon  
       450-programmer.
      *    edr-position is made equal to ws-programmer 
           move ws-programmer to edr-position.
      *    ws-percent-store is made equal to ws-percent-programmer 
           move ws-percent-programmer to ws-percent-store.
      *    Code run when perform 650-calculate-salary is called upon  
           perform 650-calculate-salary.
      *    increments the ws-programmer-count by one   
           add 1 to ws-programmer-count.
      *    ws-pay-increase is added to ws-programmer-increase 
           add ws-pay-increase to ws-programmer-increase.
           
      * Code run when perform 550-junior-programmer is called upon      
       550-junior-programmer.
      *    edr-position is made equal to ws-junior-programmer 
           move ws-junior-programmer to edr-position.
      *    ws-percent-store is made equal to ws-percent-junior-programmer
           move ws-percent-junior-programmer to ws-percent-store.
      *    650-calculate-salary code is run in order to perform the calculations for ws-pay-increase, and determines if edr-percent-sign is blank       
           perform 650-calculate-salary.
      *    ws-pay-increase is added to ws-junior-increase    
           add ws-pay-increase to ws-junior-increase.
      *    increments the ws-junior-count by one
           add 1 to ws-junior-count.
        
      * Code run when perform 600-not-classified is called upon     
       600-not-classified.
      *    clears edr-position
           move spaces to edr-position.
      *    ws-percent-unclassified is made equal to ws-percent-store
           move ws-percent-unclassified to ws-percent-store.
      *    650-calculate-salary code is run in order to perform the calculations for ws-pay-increase, and determines if edr-percent-sign is blank      
           perform 650-calculate-salary.
           
      * Code run when perform 650-calculate-salary is called upon      
       650-calculate-salary.
      *    ws-pay-increase is set equal to the rounded value of ws-percent-store divided by 100 then multiplied by er-employee-salary
           compute ws-pay-increase rounded equals ws-percent-store/100 * er-employee-salary.
      *    ws-new-salary is set equal to the rounded value of er-employee-salary added by ws-pay-increase
           compute ws-new-salary rounded equals er-employee-salary + ws-pay-increase.
      *    if the value of ws-percent-store equals ws-percent-unclassified
           if (ws-percent-store equal ws-percent-unclassified)
      *        edr-percent-sign is set equal to blank
               move spaces to edr-percent-sign
      *    if the value of ws-percent-store is not equal to ws-percent-unclassified
           else
      *        make edr-percent-sign equal to ws-percent-sign
               move ws-percent-sign to edr-percent-sign
           end-if.