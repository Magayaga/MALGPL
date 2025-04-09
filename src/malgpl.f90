!
! MALGPL - MAGAYAGAIAN ALGORITHMIC PROGRAMMING LANGUAGE
! It was originally written in the Fortran programming language
! Copyright (c) 2024-2025 Cyril John Magayaga (cjmagayaga957@gmail.com)
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
    
        ! Process lines only within a valid BEGIN-END block
        if (.not. in_main_block) then
            print *, 'Error: WRITE() or WRITENUM() outside BEGIN-END block.'
            cycle
        endif
    
        ! Handle the VAR keyword
        if (index(adjustl(line), 'VAR ') == 1) then
            call handle_var(line)
            cycle
        endif

        if (index(adjustl(line), 'LET ') == 1) then
            call handle_let(line)
            cycle
        endif
    
        if (index(adjustl(line), 'WRITE(') == 1) then
            call handle_write(line)
            cycle
        endif
    
        if (index(adjustl(line), 'WRITENUM(') == 1) then
            call handle_writenum(line)
            cycle
        endif

        if (index(adjustl(line), 'FOR ') == 1) then
            call handle_for(line)
            cycle
        endif

        if (index(adjustl(line), 'NEXT ') == 1) then
            call handle_next(line)
            cycle
        endif

        if (index(adjustl(line), 'VARL ') == 1) then
            call handle_varl(line)
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
        logical :: name_exists
        integer :: i
    
        ! Find the position of ':=' in the line
        eq_pos = index(line, ':=')
        if (eq_pos == 0) then
            print *, 'Error: Invalid VAR definition. Missing ":=".'
            return
        endif
    
        ! Extract variable name and value
        var_name = adjustl(line(5:eq_pos-1))
        var_value = adjustl(line(eq_pos+2:len_trim(line)))
    
        ! Check if the variable name is empty
        if (len_trim(var_name) == 0) then
            print *, 'Error: Variable name cannot be empty.'
            return
        endif
    
        ! Check for duplicate variable names
        name_exists = .false.
        if (allocated(vars)) then
            do i = 1, size(vars)
                if (trim(vars(i)%name) == trim(var_name)) then
                    name_exists = .true.
                    exit
                endif
            end do
        endif
        if (name_exists) then
            print *, 'Error: Variable name "', trim(var_name), '" is already defined.'
            return
        endif
    
        ! Allocate or reallocate the vars array
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

    ! Subroutine to handle LET variables
    subroutine handle_let(line)
        character(len=1024), intent(in) :: line
        character(len=32) :: var_name
        character(len=1024) :: var_value
        integer :: eq_pos
        type(variable), allocatable :: temp(:)
        logical :: name_exists
        integer :: i
    
        ! Find the position of ':=' in the line
        eq_pos = index(line, ':=')
        if (eq_pos == 0) then
            print *, 'Error: Invalid LET definition. Missing ":=".'
            return
        endif
    
        ! Extract variable name and value
        var_name = adjustl(line(5:eq_pos-1))
        var_value = adjustl(line(eq_pos+2:len_trim(line)))
    
        ! Check if the variable name is empty
        if (len_trim(var_name) == 0) then
            print *, 'Error: LET variable name cannot be empty.'
            return
        endif
    
        ! Validate that the value is enclosed in quotes
        if (.not. ((var_value(1:1) == '"' .and. var_value(len_trim(var_value):len_trim(var_value)) == '"') .or. &
                   (var_value(1:1) == "'" .and. var_value(len_trim(var_value):len_trim(var_value)) == "'"))) then
            print *, 'Error: LET variable value must be enclosed in double quotes or single quotes'
            return
        endif
    
        ! Remove surrounding quotes from the value
        var_value = var_value(2:len_trim(var_value)-1)
    
        ! Check for duplicate variable names
        name_exists = .false.
        if (allocated(vars)) then
            do i = 1, size(vars)
                if (trim(vars(i)%name) == trim(var_name)) then
                    name_exists = .true.
                    exit
                endif
            end do
        endif
        if (name_exists) then
            print *, 'Error: LET variable name "', trim(var_name), '" is already defined.'
            return
        endif
    
        ! Allocate or reallocate the vars array
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
    end subroutine handle_let

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
        if (len_trim(message) == 0) then
            print *, 'Error: WRITE() argument is empty.'
            return
        endif
    
        if (message(1:1) == '\') then
            message = message(2:)
            var_index = find_variable_index(message)
            if (var_index > 0) then
                print *, trim(vars(var_index)%value)
            else
                print *, 'Error: Undefined variable ', trim(message)
            endif
        else if (message(1:1) == '"' .and. message(len_trim(message):len_trim(message)) == '"') then
            ! It's a text
            message = message(2:len_trim(message)-1)
            print *, trim(message)
        else
            print *, 'Error: Invalid WRITE() argument format. Must be a variable or text in quotes.'
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
    
        if (len_trim(expression) == 0) then
            print *, 'Error: WRITENUM() argument is empty.'
            return
        endif
    
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
    
        ! Check for invalid results
        if (result /= result) then ! NaN check
            print *, 'Error: Invalid numeric expression in WRITENUM().'
        else
            print *, result
        endif
    end subroutine handle_writenum

    subroutine handle_for(line)
        character(len=1024), intent(in) :: line
        character(len=32) :: loop_var
        integer :: start_val, end_val, eq_pos1, eq_pos2

        ! Extract loop variable and range
        eq_pos1 = index(line, ':=')
        eq_pos2 = index(line, 'TO')

        if (eq_pos1 == 0 .or. eq_pos2 == 0) then
            print *, 'Error: Invalid FOR syntax. Missing ":=" or "TO".'
            return
        endif

        loop_var = adjustl(line(5:eq_pos1-1))
        read(line(eq_pos1+2:eq_pos2-1), *) start_val
        read(line(eq_pos2+2:len_trim(line)), *) end_val

        ! Set up the loop
        do while (start_val <= end_val)
            call execute_loop_body(loop_var, start_val)
            start_val = start_val + 1
        end do
    end subroutine handle_for

    ! Subroutine to handle NEXT
    subroutine handle_next(line)
        character(len=1024), intent(in) :: line
        character(len=32) :: loop_var
        logical :: in_loop
    
        ! Extract the loop variable from the NEXT statement
        loop_var = adjustl(line(6:len_trim(line)))
    
        ! Validate that the NEXT statement corresponds to an active FOR loop
        in_loop = validate_loop_context(loop_var)
    
        if (.not. in_loop) then
            print *, 'Error: NEXT ', trim(loop_var), ' has no corresponding FOR loop.'
            stop
        endif
    
        ! If valid, allow the FOR loop to continue (no further action needed)
    end subroutine handle_next
    
    ! Subroutine to validate the loop context
    logical function validate_loop_context(loop_var)
        character(len=32), intent(in) :: loop_var
        ! Logic to check if the loop variable is part of an active FOR loop
        ! For simplicity, assume we maintain a stack of active loops (not shown here)
        logical :: loop_found
    
        ! Placeholder logic for loop validation
        loop_found = .true.  ! Replace this with actual logic to check active loops
    
        validate_loop_context = loop_found
    end function validate_loop_context

    ! Subroutine to handle VARL (local variable update)
    subroutine handle_varl(line)
        character(len=1024), intent(in) :: line
        character(len=32) :: var_name
        character(len=1024) :: var_value
        integer :: eq_pos

        eq_pos = index(line, ':=')
        if (eq_pos == 0) then
            print *, 'Error: Invalid VARL syntax. Missing ":=".'
            return
        endif

        var_name = adjustl(line(6:eq_pos-1))
        var_value = adjustl(line(eq_pos+2:len_trim(line)))

        ! Update the variable locally
        call update_variable(var_name, var_value)
    end subroutine handle_varl

    ! Subroutine to update a variable
    subroutine update_variable(var_name, var_value)
        character(len=32), intent(in) :: var_name
        character(len=1024), intent(in) :: var_value
        integer :: i

        if (.not. allocated(vars)) then
            print *, 'Error: Variable "', trim(var_name), '" is not defined.'
            return
        endif

        do i = 1, size(vars)
            if (trim(vars(i)%name) == trim(var_name)) then
                vars(i)%value = var_value
                return
            endif
        end do

        print *, 'Error: Variable "', trim(var_name), '" is not defined.'
    end subroutine update_variable

    ! Subroutine to execute the body of a FOR loop
    subroutine execute_loop_body(loop_var, loop_val)
        character(len=32), intent(in) :: loop_var
        integer, intent(in) :: loop_val
        ! Update the loop variable
        call update_variable(loop_var, trim(adjustl(to_string(loop_val))))
    end subroutine execute_loop_body

    ! Function to convert an integer to string
    function to_string(val) result(str)
        integer, intent(in) :: val
        character(len=32) :: str
        write(str, '(I0)') val
    end function to_string

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
