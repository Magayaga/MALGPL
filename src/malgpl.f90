! 
! MALGPL - MAGAYAGAIAN ALGORITHMIC PROGRAMMING LANGUAGE
! It was originally written in the Fortran programming language
! Copyright (c) 2024 Cyril John Magayaga (cjmagayaga957@gmail.com, cyrilmagayaga@proton.me)
!
program malgpl
    implicit none
    character(len=256) :: filename
    character(len=1024) :: line
    integer :: ios
    logical :: in_main_block, in_sub_block

    ! Check command-line arguments
    if (command_argument_count() /= 1) then
        print *, 'Usage: ./malgpl <filename>'
        stop
    endif

    call get_command_argument(1, filename)

    open(unit=10, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'Error: Could not open file ', trim(filename)
        stop
    endif

    in_main_block = .false.
    in_sub_block = .false.

    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit

        line = adjustl(line)  ! Remove leading spaces

        ! Handle the BEGIN keyword
        if (index(line, 'BEGIN') == 1) then
            if (.not. in_main_block) then
                in_main_block = .true.
            else
                in_sub_block = .true.
            endif
            cycle
        endif

        ! Handle the END keyword
        if (index(line, 'END') == 1) then
            if (in_sub_block) then
                in_sub_block = .false.
            else
                in_main_block = .false.
            endif
            cycle
        endif

        ! Ignore COMMENT lines
        if (index(line, 'COMMENT') == 1) cycle

        ! Handle the WRITE keyword
        if (index(adjustl(line), 'WRITE(') == 1) then
            call handle_write(line)
        endif
    end do

    close(10)
contains
    subroutine handle_write(line)
        character(len=1024), intent(in) :: line
        character(len=1024) :: message, expression
        integer :: start_pos, end_pos, start_text, end_text, value
        real :: result

        ! Find the start and end of the argument within parentheses
        start_text = index(line, 'WRITE(') + len('WRITE(')
        end_text = len_trim(line)
        if (line(end_text:end_text) == ')') then
            end_text = end_text - 1
        endif

        ! Find the start and end of the expression within parentheses
        start_pos = index(line, 'WRITE(') + len('WRITE(')
        end_pos = index(line(start_pos:), ')') + start_pos - 1

        ! Extract the argument
        message = adjustl(line(start_text:end_text))

        ! Check if the argument is a text or an expression
        if (message(1:1) == '"' .and. message(len_trim(message):len_trim(message)) == '"') then
            ! It's a text
            message = message(2:len_trim(message)-1)
            print *, trim(message)
        else if (message(1:1) == "'" .and. message(len_trim(message):len_trim(message)) == "'") then
            ! It's a text
            message = message(2:len_trim(message)-1)
            print *, trim(message)
        else if (message(1:1) == '“' .and. message(len_trim(message):len_trim(message)) == '”') then
            ! It's a text
            message = message(2:len_trim(message)-1)
            print *, trim(message)
        else if (message(1:1) == "‘" .and. message(len_trim(message):len_trim(message)) == "’") then
            ! It's a text
            message = message(2:len_trim(message)-1)
            print *, trim(message)
        else if (start_pos > 0 .and. end_pos > start_pos) then
            ! It's a expression
            expression = line(start_pos:end_pos-1)
            call evaluate_expression(trim(adjustl(expression)), result, value)
            if (value == 0) then
                print *, result
            else
                print *, 'Syntax error in expression: ', trim(adjustl(expression))
            endif
        else
            print *, 'Syntax error: Expected WRITE(<expression>).'
        endif
    end subroutine handle_write

    subroutine evaluate_expression(expr, result, ios)
        character(len=1024), intent(in) :: expr
        real, intent(out) :: result
        integer, intent(out) :: ios
        character(len=1024) :: left_operand, right_operand
        integer :: op_pos

        ios = 0  ! Default to no error
        result = 0.0  ! Initialize result

        ! Detect and evaluate basic operations
        op_pos = index(expr, '+')
        if (op_pos > 0) then
            ! Addition
            left_operand = expr(1:op_pos-1)
            right_operand = expr(op_pos+1:)
            result = real(read_integer(trim(left_operand))) + real(read_integer(trim(right_operand)))
            return
        endif

        op_pos = index(expr, '-')
        if (op_pos > 0) then
            ! Subtraction
            left_operand = expr(1:op_pos-1)
            right_operand = expr(op_pos+1:)
            result = real(read_integer(trim(left_operand))) - real(read_integer(trim(right_operand)))
            return
        endif

        op_pos = index(expr, '*')
        if (op_pos > 0) then
            ! Multiplication
            left_operand = expr(1:op_pos-1)
            right_operand = expr(op_pos+1:)
            result = real(read_integer(trim(left_operand))) * real(read_integer(trim(right_operand)))
            return
        endif

        op_pos = index(expr, '/')
        if (op_pos > 0) then
            ! Division
            left_operand = expr(1:op_pos-1)
            right_operand = expr(op_pos+1:)
            result = real(read_integer(trim(left_operand))) / real(read_integer(trim(right_operand)))
            return
        endif

        ! If no operator is found
        ios = 1
    end subroutine evaluate_expression

    integer function read_integer(str)
        character(len=1024), intent(in) :: str
        integer :: num, ios

        read(str, *, iostat=ios) num
        if (ios /= 0) then
            num = 0
        endif

        read_integer = num
    end function read_integer
end program malgpl
