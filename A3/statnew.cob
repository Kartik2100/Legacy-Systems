     
       identification division.
       program-id. stats.

       environment division.
       input-output section.
       file-control.
*> file names for the user to give will be place in the variables
       select ifile assign to dynamic ws-fname
               organization is line sequential.
       select ofile assign to dynamic ws-fname2
               organization is line sequential.
*> initializing the files to be opened
       data division.
       file section.
       fd ifile.
       01 sample-input     pic x(80).
       fd ofile.
       01 sample-output    pic x(80).

*> variables are initialized here
       working-storage section.
       77 sum-mean         pic s9(14)v9(4) usage is computational-3.
       77 number-amount    pic s9999 usage is computational.
       77 meana            pic s9(14)v9(4) usage is computational-3.
       77 meanh            pic s9(14)v9(4) usage is computational-3.
       77 meanq            pic s9(14)v9(4) usage is computational-3.
       77 i                pic s9999 usage is computational.
       77 n                pic s9999 usage is computational.
       77 j                pic s9999 usage is computational.
       77 k                pic s9999 usage is computational.
       77 v1               pic s9999 usage is computational.
       77 v2               pic s9999 usage is computational.
       77 temp             pic s9(14)v9(4) usage is computational-3.
       77 t                pic s9999v9 usage is computational.
       77 standarddev      pic s9(14)v9(4) usage is computational-3.
       77 var              pic s9(14)v9(4) usage is computational-3. 
       77 sum-std          pic s9(14)v9(4) usage is computational-3. 
       77 sum-var          pic s9(14)v9(4) usage is computational-3. 
       77 med              pic s9(14)v9(4) usage is computational-3. 
       77 sum-q            pic s9(34)v9(4) usage is computational-3. 
       77 sum-h            pic s9(14)v9(20) usage is computational-3.        
       77 feof             pic a(1).

       01 array-area.
           02 x pic s9(14)v9(4) usage is computational-3
               occurs 1000 times.

*> input value in the input file until it ends
       01 input-value.
           02 inputx pic s9(14)v9(4).
           02 filler pic x(62).

*> first line title of the output file
       01 title-line.
           02 filler pic x(41) value
               '  statistical information of data values'.

*> underline for the output file       
       01 under-line.
           02 filler pic x(42) value
               '******************************************'.
       
*> header to show the folling after are the data values in the output file
       01 col-heads.
           02 filler pic x(32) value
               '           |    data values    |'.
       
*> prints out each of the data from the input file to the output file       
       01 data-line.
           02 filler pic x(5) values spaces.
           02 out-x pic -(14)9.9(4).

*> prints out the mean value
       01 print-line1.
           02 filler pic x(21) value
               'mean (average) = '.
           02 out-meana pic -(14)9.9(4).
       
*> prints out the standard deviation
       01 print-line2.
           02 filler picture x(21) value
               'standard deviation = '.
           02 out-std pic -(14)9.9(4).

*> prints out the quadratic mean
       01 print-line3.
           02 filler picture x(21) value
               'quadratic mean = '.
           02 out-meanq pic -(14)9.9(4).

*> prints out harmonic mean
       01 print-line4.
           02 filler picture x(21) value
               'harmonic mean = '.
           02 out-meanh pic -(14)9.9(4).

*> prints out the median
       01 print-line5.
           02 filler picture x(21) value
               'median = '.
           02 out-median pic -(14)9.9(4).

*> prints out the variance
       01 print-line6.
           02 filler picture x(21) value
               'variance = '.
           02 out-var pic -(14)9.9(4).



       procedure division.
*> recieves the inputs from the user of the file names
           display "what is the input file name?: ".
           accept ws-fname.
           display "what is the output file name?: ".
           accept ws-fname2.

*> opens up the files and writes the basic headers of the file
*> also initializes variables to zero
           open input ifile, output ofile.
           write sample-output from title-line after advancing 0 lines.
           write sample-output from under-line after advancing 1 lines.
           write sample-output from col-heads after advancing 1 lines.
           write sample-output from under-line after advancing 1 lines.
           move 0 to sum-mean.
           move 0 to sum-std.
           move 0 to sum-h.
           move 0 to sum-q.
           move 0 to sum-var

*> performs each equation from its paragraph
           perform input-loop varying number-amount from 1 by 1
               until feof = 'y'.
           write sample-output from under-line after advancing 1 lines.
           perform mean-calc.
           perform std-calc.
           perform meanq-calc.
           perform meanh-calc.
           perform var-calc.
           perform median-calc.
           perform finish.

*> recieves the data values from the input file
           input-loop.
               read ifile into input-value at end move 'y' to feof
                   not at end
                   move inputx to x(number-amount), out-x
                   write sample-output from data-line after advancing 1 line
                   compute sum-mean = sum-mean + x(number-amount)
               end-read.

*> calculates the mean value of the data values and writes it to the output file           
           mean-calc.
               compute number-amount = number-amount - 2.
               compute meana rounded = sum-mean / number-amount.
               move meana to out-meana.
               write sample-output from print-line1 after advancing 1 line.

*> calculates the standard deviation value of the data values and writes it to the output file           
           std-calc.
               perform varying i from 1 by 1 until i is greater than number-amount 
                   compute sum-std = sum-std + ((x(i) - meana) ** 2)
               end-perform.
               compute standarddev = (sum-std / number-amount) ** 0.5.
               move standarddev to out-std.
               write sample-output from print-line2 after advancing 1 line.

*> calculates the quadratic mean value of the data values and writes it to the output file           
           meanq-calc.
               perform varying i from 1 by 1 until i is greater than number-amount
                   compute sum-q = sum-q + (x(i) ** 2)
               end-perform.
               compute meanq = ((1 / number-amount) * sum-q) ** 0.5.
               move meanq to out-meanq.
               write sample-output from print-line3 after advancing 1 line.

*> calculates the harmonic mean value of the data values and writes it to the output file           
           meanh-calc.
               perform varying i from 1 by 1 until i is greater than number-amount
                   compute sum-h = sum-h + (1 / x(i))
               end-perform
               compute meanh rounded = number-amount / sum-h.
               move meanh to out-meanh.
               write sample-output from print-line4 after advancing 1 line.

*> calculates the variance value of the data values and writes it to the output file           
           var-calc.
               perform varying i from 1 by 1 until i is greater than number-amount 
                   compute sum-var = sum-var + ((x(i) - meana) ** 2)
               end-perform.
               compute var rounded= sum-var / (number-amount).
               move var to out-var.
               write sample-output from print-line6 after advancing 1 line.

*> calculates the median value of the data values and writes it to the output file           
           median-calc.
               perform varying j from 1 by 1 until j is greater than number-amount
                   compute v1 = number-amount - j
                   perform varying k from 1 by 1 until k is greater than v1
                       compute v2 = k + 1
                       if (x(k) > x(v2))
                           move x(k) to temp
                           move x(v2) to x(k)
                           move temp to x(v2)
                       end-if
                   end-perform
               end-perform.
               compute t = number-amount / 2.
               compute i = number-amount / 2.
               compute n = i + 1.
               if ((t - i) = 0)
                   compute med = (x(i) + x(n)) / 2
               else
                   move x(n) to med
               end-if.
               move med to out-median.
               write sample-output from print-line5 after advancing 1 line.

*> closes the files and stops the program
           finish.
               close ifile, ofile.
               stop run.
