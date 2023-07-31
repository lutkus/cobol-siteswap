       *> Siteswap validation or something
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Siteswap.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
         01 userInput PIC X(64).
         01 i PIC 9(2).
         01 j PIC 9(2).
         01 ssLength PIC 9(2) VALUE 64.
         01 ssTable.
           02 ssValue PIC 9(2) OCCURS 64 TIMES.
         01 validityData.
           02 ssState.
             03 stateValue PIC 9(1) OCCURS 64 TIMES.
           02 landingPosition PIC 9(2).
           02 ssValidity PIC 9 VALUE 0.
             88 ssValid VALUE 1.
             88 ssInvalid VALUE 0.
          01 ssStateTable.
            02 ssStateValue PIC 9(1) OCCURS 64 TIMES.
            02 ssStateTableLength PIC 9(2).
          01 numObjects PIC 9(3).
          01 stateCalculationDate.
            02 currentThrow PIC 9(2).
            02 numZeros PIC 9(2).

       PROCEDURE DIVISION.
          MAIN-PARAGRAPH.
          display "Please enter a siteswap: " no advancing
          accept userInput from console.
          move FUNCTION UPPER-CASE(userInput) to userInput
          display "User input: " userInput
          PERFORM GET-LENGTH
          display "Siteswap length is: " ssLength
          PERFORM CONVERT-INPUT-TO-TABLE
          *> DISPLAY "ssTable: " ssTable
          PERFORM DISPLAY-SS-TABLE
          display space
          PERFORM VALIDATE-SS-TABLE
          if ssValid
            PERFORM GET-NUM-OBJECTS
            display "Siteswap is for " numObjects " objects."
            PERFORM GET-STATE
          end-if
          STOP RUN.

          *> set ssLength based on userInput
          GET-LENGTH.
          move 64 to i
          PERFORM GET-LENGTH-LOOP WITH TEST AFTER UNTIL i=0.
          GET-LENGTH-LOOP.
            if userInput(i:1) NOT = space
              move i to ssLength
              move 1 to i
            end-if
            subtract 1 from i.

          *> convert userInput to ssTable (string to int)
          *> any non-alpahnumeric characters are converted to 0.
          CONVERT-INPUT-TO-TABLE.
            move 1 to i
            PERFORM UNTIL i>ssLength
              evaluate userInput(i:1)
                when "A" thru "Z"
                  compute ssValue(i) = FUNCTION ORD(userInput(i:1)) - 56
                when "0" thru "9"
                  compute ssValue(i) = FUNCTION ORD(userInput(i:1)) - 49
                when OTHER
                  move 0 to ssValue(i)
                end-evaluate
                add 1 to i
              END-PERFORM.

          DISPLAY-SS-TABLE.
            display "Siteswap converted to integers is: " no advancing
            move 1 to i
            PERFORM UNTIL i>ssLength
              display ssValue(i) space no advancing
              add 1 to i
            END-PERFORM.      
              
          *> Check if ssTable is a valid vanilla siteswap,
          *> and set ssValid accordingly
          VALIDATE-SS-TABLE.
            move zeros to ssState
            move 1 to i
            PERFORM UNTIL i>ssLength
              move function MOD((ssValue(i) + i) ssLength) 
                to landingPosition
              if landingPosition = 0
                move ssLength to landingPosition
              end-if
              add 1 to stateValue(landingPosition)
              add 1 to i
            END-PERFORM.
            move 1 to i
            set ssValid to true
            PERFORM UNTIL i>ssLength
              if stateValue(i) = 0
                set ssInvalid to true
              end-if
              add 1 to i
            END-PERFORM.
            if ssValid
              display "This is a valid siteswap."
            else 
              display 
               "This is not a valid vanilla asynchronous siteswap."
            end-if.

          GET-NUM-OBJECTS.
            move zeros to numObjects
            move 1 to i
            perform until i>ssLength
              add ssValue(i) to numObjects
              add 1 to i
            END-PERFORM.
            divide numObjects by ssLength giving numObjects.

          GET-STATE.
            move zeros to ssStateTable
            move 1 to i
            perform until i>numObjects
              perform THROW-I
              add 1 to i
            end-perform.
            move zero to numZeros
            perform GET-NUM-ZEROS
            if numZeros NOT = numObjects
              perform until numZeros = numObjects or i=64
                perform THROW-I
                perform GET-NUM-ZEROS
                add 1 to i
              end-perform
            end-if
            subtract 1 from landingPosition giving j.
            perform until j=0
              if ssStateValue(j) = 0
                move 1 to ssStateValue(j)
              else 
                move 0 to ssStateValue(j)
              end-if
              subtract 1 from j
            end-perform
            move landingPosition to j
            perform until j=64
              move 0 to ssStateValue(j)
              add 1 to j
            end-perform
            move landingPosition to ssStateTableLength
            perform DISPLAY-STATE.

          THROW-I.
              move function MOD(i ssLength) to currentThrow
              if currentThrow = 0 
                add ssLength to currentThrow
              end-if
              compute landingPosition = ssValue(currentThrow) + i
              add 1 to ssStateValue(landingPosition).
          GET-NUM-ZEROS.
              move zero to numZeros
              move landingPosition to j
              perform until j=0
                if ssStateValue(j) = 0
                  add 1 to numZeros
                end-if
                subtract 1 from j
              end-perform. 

          DISPLAY-STATE.
            move 65 to i
            perform until i=0
              subtract 1 from i
              if ssStateValue(i) NOT = 0
                move i to ssStateTableLength
                add 1 to ssStateTableLength
                move 0 to i
              end-if
            end-perform.
            move 1 to i
            display "State: " no advancing
            perform until i=ssStateTableLength
              display ssStateValue(i) no advancing
              add 1 to i
            end-perform.
            display space.
           
            