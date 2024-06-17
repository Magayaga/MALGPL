!
! MALGPL - MAGAYAGAIAN ALGORITHMIC PROGRAMMING LANGUAGE
! It was originally written in the Fortran programming language
! Copyright (c) 2024 Cyril John Magayaga (cjmagayaga957@gmail.com)
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

        ! Handle the WRITENUM keyword
        if (index(adjustl(line), 'WRITENUM(') == 1) then
            call handle_writenum(line)
        endif
    end do

    close(10)
contains
    subroutine handle_write(line)
        character(len=1024), intent(in) :: line
        character(len=1024) :: message
        integer :: start_text, end_text

        ! Find the start and end of the argument within parentheses
        start_text = index(line, 'WRITE(') + len('WRITE(')
        end_text = len_trim(line)
        if (line(end_text:end_text) == ')') then
            end_text = end_text - 1
        endif

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
        endif
    end subroutine handle_write

    subroutine handle_writenum(line)
        character(len=1024), intent(in) :: line
        character(len=1024) :: expression
        integer :: start_text, end_text
        real :: result

        ! Find the start and end of the argument within parentheses
        start_text = index(line, 'WRITENUM(') + len('WRITENUM(')
        end_text = len_trim(line)
        if (line(end_text:end_text) == ')') then
            end_text = end_text - 1
        endif

        ! Extract the argument
        expression = adjustl(line(start_text:end_text))

        ! Evaluate the expression
        result = evaluate_expression(expression)
        print *, result
    end subroutine handle_writenum

    recursive function evaluate_expression(expr) result(res)
        character(len=*), intent(in) :: expr
        real :: res
        character(len=1024) :: left_expr, right_expr
        integer :: pos
        real :: left_val, right_val

        ! Remove any surrounding parentheses
        if (expr(1:1) == '(' .and. expr(len_trim(expr):len_trim(expr)) == ')') then
            res = evaluate_expression(expr(2:len_trim(expr)-1))
            return
        endif

        ! Find the position of the operator
        pos = index(expr, '+')
        if (pos /= 0) then
            left_expr = adjustl(expr(1:pos-1))
            right_expr = adjustl(expr(pos+1:))
            left_val = evaluate_expression(left_expr)
            right_val = evaluate_expression(right_expr)
            res = left_val + right_val
            return
        endif

        pos = index(expr, '-')
        if (pos /= 0) then
            left_expr = adjustl(expr(1:pos-1))
            right_expr = adjustl(expr(pos+1:))
            left_val = evaluate_expression(left_expr)
            right_val = evaluate_expression(right_expr)
            res = left_val - right_val
            return
        endif

        pos = index(expr, '*')
        if (pos /= 0) then
            left_expr = adjustl(expr(1:pos-1))
            right_expr = adjustl(expr(pos+1:))
            left_val = evaluate_expression(left_expr)
            right_val = evaluate_expression(right_expr)
            res = left_val * right_val
            return
        endif

        pos = index(expr, '/')
        if (pos /= 0) then
            left_expr = adjustl(expr(1:pos-1))
            right_expr = adjustl(expr(pos+1:))
            left_val = evaluate_expression(left_expr)
            right_val = evaluate_expression(right_expr)
            res = left_val / right_val
            return
        endif

        pos = index(expr, '^')
        if (pos /= 0) then
            left_expr = adjustl(expr(1:pos-1))
            right_expr = adjustl(expr(pos+1:))
            left_val = evaluate_expression(left_expr)
            right_val = evaluate_expression(right_expr)
            res = left_val ** right_val
            return
        endif

        ! If no operator is found, convert the expression to a number
        read(expr, *) res
    end function evaluate_expression
end program malgpl
