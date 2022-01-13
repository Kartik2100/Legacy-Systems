! Name: Kartik Patel
! Student ID Number: 1052085
! Assignment 4
program flesch
    ! Declaring Variable
    implicit none
    character (len=30) :: fname, fname2
    character (len=30) :: tempName
    integer :: fileSize, io, wordCount, syllableCount, sentenceCount, lineLen, j, start, k, tLine
    real :: wPerS, sPerW, fleschIndex, fleschKincaid
    logical :: lexist
    character :: formL
    character (len=100) :: inputName

    !Getting user input for the file names
    write(*,*) 'Enter input file name: '
    read(*,*) fname
    write(*,*) 'Enter output file name: '
    read(*,*) fname2

    wordCount = 0
    sentenceCount = 0
    start = 0
    k = 0
    inquire(file=fname, exist=lexist, form=formL, size=fileSize)
    ! checking to see if the file names are valid 
    if (lexist) then
        open(unit=20, file=fname, status='old', action='read')
        open(unit=10,file=fname2,status='replace',action='write')
    else
        write(*,*) 'File does not exist'
    end if
        
    do
        ! reads in input from the input file line by line
        read(20,'(A)',IOSTAT=io) inputName
        if (io > 0) then
            write(*,*) 'Exiting program... '
            exit
        !exits loop once file is at the end
        else if (io < 0) then
            exit
        else
            lineLen = len(trim(inputName))
            ! makes all inputs to lowercase
            call lowerCase(inputName, lineLen)
            ! getting the number of words in the file
            call numWords(inputName, lineLen, wordCount)
            ! getting the number of sentences in the file
            call numSentence(inputName, lineLen, sentenceCount)
            
            do j = 1, lineLen
                !splitting each word from the sentence
                if(inputName(j:j) == ' ' .or. inputName(j:j) == '.' .or. inputName(j:j) == '!' .or.inputName(j:j) == '?' &
                &.or. inputName(j:j) == ':' .or. inputName(j:j) == ';') then
                    if(inputName(j-1:j-1) == ',') then
                        tempName = inputName(start+1:j-2)
                    else
                        tempName = inputName(start+1:j-1)
                    end if
                    start = j
                    k = 1
                else
                    k = 0
                end if
                if(k == 1)then
                    if(len(trim(tempName)) > 3) then
                        tLine = len(tempName)
                        ! returns the total syllable value if the word 
                        ! is greater than 3.
                        call numSuffix(tempName, tLine, syllableCount)
                        call numVowel(tempName, tLine, syllableCount)
                    else
                        ! less that 3 character words count as one syllable.
                        syllableCount = syllableCount + 1
                    end if
                end if
            end do
            start = 0
        end if
        
    end do
    ! calculations
    wPerS = real(wordCount) / real(sentenceCount)
    sPerW = real(syllableCount) / real(wordCount)
    fleschIndex = 206.835 - ((1.015*wPerS) + (84.6*sPerW))
    fleschKincaid = (0.39 * wPerS) + (11.8 * sPerW) - 15.59
    ! output to the output file
    write(10,*) '*************************************'
    write(10,*) '               OUTPUT'
    write(10,*) '*************************************'
    write(10,*) 'Number of words:     ', wordCount
    write(10,*) 'Number of sentences: ', sentenceCount
    write(10,*) 'Number of syllables: ', syllableCount
    write(10,*) 'Words per sentences: '
    write(10,30) wPerS
30  format(f10.2)
    write(10,*) 'Syllables per words: '
    write(10,40) sPerW
40  format(f10.2)
    write(10,*) 'Flesch readability index: '
    write(10,50) fleschIndex
    50  format (f10.2)
    write(10,*) 'Flesch-Kincaid grade level: '
    write(10,60) fleschKincaid
60  format (f10.2)
    write(10,*) '*************************************'
    close(20)
    close(10)
end program

! This subroutine counts the number of words in the input file
subroutine numWords(lineInput, lineLength, words)
    implicit none
    character (len=100), intent(in) :: lineInput
    integer, intent(in) :: lineLength
    integer, intent(inout) :: words
    integer :: i
    character (len=lineLength) :: temp
    temp = lineInput

    !checks for spaces to count for each word
    do i = 1, lineLength
        if(temp(i:i) == ' ') then
            words = words + 1
        end if
    end do
    words = words + 1
end subroutine numWords

! This subroutine counts the number of sentences in the input file.
subroutine numSentence(lineInput, lineLength, sentences)
    implicit none
    character (len=100), intent(in) :: lineInput
    integer, intent(in) :: lineLength
    integer, intent(inout) :: sentences
    integer :: i
    character (len=lineLength) :: temp
    temp = lineInput

    !checks for each puntuation and adds it to the total
    do i = 1, lineLength
        if(temp(i:i) == '.' .or. temp(i:i) == '!' .or. temp(i:i) == '?' .or. temp(i:i) == ':' .or. temp(i:i) == ';') then
            sentences = sentences + 1
        end if
    end do
end subroutine numSentence

! checks for the number of suffix to ignore
! tries to work with all puntuation and spaces
subroutine numSuffix(lineInput, lineLength, syllable)
    implicit none
    character (len=30), intent(in) :: lineInput
    integer, intent(in) :: lineLength
    integer, intent(inout) :: syllable
    integer :: i
    character (len = 3) :: es, ed, le
    character (len = 2) :: e

    do i=1, lineLength
        es = 'es '
        ed = 'ed '
        le = 'le '
        e = 'e '
        if (lineInput(i:i+2) == es .or. lineInput(i:i+2) == ed .or. lineInput(i:i+1) == e) then
            syllable = syllable - 1
        end if
        if (lineInput(i:i+2) == le) then
            syllable = syllable + 1
        end if

        es = 'es.'
        ed = 'ed.'
        le = 'le.'
        e = 'e.'
        if (lineInput(i:i+2) == es .or. lineInput(i:i+2) == ed .or. lineInput(i:i+1) == e) then
            syllable = syllable - 1
        end if
        if (lineInput(i:i+2) == le) then
            syllable = syllable + 1
        end if

        es = 'es!'
        ed = 'ed!'
        le = 'le!'
        e = 'e!'
        if (lineInput(i:i+2) == es .or. lineInput(i:i+2) == ed .or. lineInput(i:i+1) == e) then
            syllable = syllable - 1
        end if
        if (lineInput(i:i+2) == le) then
            syllable = syllable + 1
        end if

        es = 'es?'
        ed = 'ed?'
        le = 'le?'
        e = 'e?'
        if (lineInput(i:i+2) == es .or. lineInput(i:i+2) == ed .or. lineInput(i:i+1) == e) then
            syllable = syllable - 1
        end if
        if (lineInput(i:i+2) == le) then
            syllable = syllable + 1
        end if

        es = 'es:'
        ed = 'ed:'
        le = 'le:'
        e = 'e:'
        if (lineInput(i:i+2) == es .or. lineInput(i:i+2) == ed .or. lineInput(i:i+1) == e) then
            syllable = syllable - 1
        end if
        if (lineInput(i:i+2) == le) then
            syllable = syllable + 1
        end if

        es = 'es;'
        ed = 'ed;'
        le = 'le;'
        e = 'e;'
        if (lineInput(i:i+2) == es .or. lineInput(i:i+2) == ed .or. lineInput(i:i+1) == e) then
            syllable = syllable - 1
        end if
        if (lineInput(i:i+2) == le) then
            syllable = syllable + 1
        end if
    end do

end subroutine numSuffix

! checks for consecutive vowels and removes one
! from syllables list if there is back to back vowels
subroutine numVowel(lineInput, lineLength, syllable)
    implicit none
    character (len=30), intent(in) :: lineInput
    integer, intent(in) :: lineLength
    integer, intent(inout) :: syllable
    integer :: i
    character (len = 2) :: c1, c2, c3, c4, c5

    do i = 1, lineLength
        if(lineInput(i:i) == 'a') then
            syllable = syllable + 1
        else if(lineInput(i:i) == 'e') then
            syllable = syllable + 1
        else if(lineInput(i:i) == 'i') then
            syllable = syllable + 1
        else if(lineInput(i:i) == 'o') then
            syllable = syllable + 1 
        else if(lineInput(i:i) == 'u') then
            syllable = syllable + 1
        else if(lineInput(i:i) == 'y') then
            syllable = syllable + 1
        end if
    end do

    do i = 1, lineLength
        c1 = 'ae'
        c2 = 'ai'
        c3 = 'ao'
        c4 = 'au'
        c5 = 'ay'
        if(lineInput(i:i+1) == c1) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c2) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c3) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c4) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c5) then
            syllable = syllable - 1
        end if

        c1 = 'ea'
        c2 = 'ei'
        c3 = 'eo'
        c4 = 'eu'
        c5 = 'ey'
        if(lineInput(i:i+1) == c1) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c2) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c3) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c4) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c5) then
            syllable = syllable - 1
        end if

        c1 = 'ia'
        c2 = 'ie'
        c3 = 'io'
        c4 = 'iu'
        c5 = 'iy'
        if(lineInput(i:i+1) == c1) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c2) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c3) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c4) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c5) then
            syllable = syllable - 1
        end if

        c1 = 'oa'
        c2 = 'oe'
        c3 = 'oi'
        c4 = 'ou'
        c5 = 'oy'
        if(lineInput(i:i+1) == c1) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c2) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c3) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c4) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c5) then
            syllable = syllable - 1
        end if

        c1 = 'ua'
        c2 = 'ue'
        c3 = 'ui'
        c4 = 'uo'
        c5 = 'uy'
        if(lineInput(i:i+1) == c1) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c2) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c3) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c4) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c5) then
            syllable = syllable - 1
        end if

        c1 = 'ya'
        c2 = 'ye'
        c3 = 'yi'
        c4 = 'yo'
        c5 = 'yu'
        if(lineInput(i:i+1) == c1) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c2) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c3) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c4) then
            syllable = syllable - 1
        else if (lineInput(i:i+1) == c5) then
            syllable = syllable - 1
        end if
    end do
end subroutine numVowel

! makes the inputline to all lowercase if it is uppercase
subroutine lowerCase (lineInput, lineLength)

    implicit none
    character (len=30), intent(inout) :: lineInput
    integer, intent(in) :: lineLength
    integer :: i, tempc

    do i=1, lineLength
        tempc = ichar(lineInput(i:i))
        if(tempc >= 65 .and. tempc < 90) then
            lineInput(i:i) = char(tempc+32)
        end if
    end do

end subroutine lowerCase
