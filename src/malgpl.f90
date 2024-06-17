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
    type :: variable
        character(len=32) :: name
        character(len=1024) :: value
    end type variable
    type(variable), allocatable :: vars(:)

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

        ! Handle the VAR keyword
        if (index(adjustl(line), 'VAR ') == 1) then
            call handle_var(line)
            cycle
        endif

        ! Handle the WRITE keyword
        if (index(adjustl(line), 'WRITE(') == 1) then
            call handle_write(line)
            cycle
        endif

        ! Handle the WRITENUM keyword
        if (index(adjustl(line), 'WRITENUM(') == 1) then
            call handle_writenum(line)
            cycle
        endif
    end do

    close(10)

contains
    subroutine handle_var(line)
        character(len=1024), intent(in) :: line
        character(len=32) :: var_name
        character(len=1024) :: var_value
        integer :: eq_pos
        type(variable), allocatable :: temp(:)

        eq_pos = index(line, ':=')
        if (eq_pos == 0) return

        var_name = adjustl(line(5:eq_pos-1))
        var_value = adjustl(line(eq_pos+2:len_trim(line)))

        if (.not. allocated(vars)) then
            allocate(vars(1))
            vars(1)%name = var_name
            vars(1)%value = var_value
        else
            allocate(temp(size(vars)+1))
            temp(1:size(vars)) = vars
            temp(size(vars)+1)%name = var_name
            temp(size(vars)+1)%value = var_value
            deallocate(vars)
            call move_alloc(temp, vars)
        endif
    end subroutine handle_var

    subroutine handle_write(line)
        character(len=1024), intent(in) :: line
        character(len=1024) :: message
        integer :: start_text, end_text, var_index

        ! Find the start and end of the argument within parentheses
        start_text = index(line, 'WRITE(') + len('WRITE(')
        end_text = len_trim(line)
        if (line(end_text:end_text) == ')') then
            end_text = end_text - 1
        endif

        ! Extract the argument
        message = adjustl(line(start_text:end_text))

        ! Check if the argument is a variable reference
        if (message(1:1) == '\') then
            message = message(2:)
            var_index = find_variable_index(message)
            if (var_index > 0) then
                print *, trim(vars(var_index)%value)
            else
                print *, 'Error: Undefined variable ', trim(message)
            endif
        else
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
        endif
    end subroutine handle_write

    subroutine handle_writenum(line)
        character(len=1024), intent(in) :: line
        character(len=1024) :: expression
        integer :: start_text, end_text, var_index
        real :: result

        ! Find the start and end of the argument within parentheses
        start_text = index(line, 'WRITENUM(') + len('WRITENUM(')
        end_text = len_trim(line)
        if (line(end_text:end_text) == ')') then
            end_text = end_text - 1
        endif

        ! Extract the argument
        expression = adjustl(line(start_text:end_text))

        ! Check if the argument is a variable reference
        if (expression(1:1) == '\') then
            expression = expression(2:)
            var_index = find_variable_index(expression)
            if (var_index > 0) then
                result = evaluate_expression(vars(var_index)%value)
            else
                print *, 'Error: Undefined variable ', trim(expression)
                return
            endif
        else
            ! Evaluate the expression
            result = evaluate_expression(expression)
        endif
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

        ! If no operators are found, treat as a number
        read(expr, *) res
    end function evaluate_expression

    function find_variable_index(name) result(index)
        character(len=*), intent(in) :: name
        integer :: index
        integer :: i

        index = -1
        do i = 1, size(vars)
            if (trim(vars(i)%name) == trim(name)) then
                index = i
                exit
            endif
        end do
    end function find_variable_index
end program malgpl
