/*
 * MALGPL - MAGAYAGAIAN ALGORITHMIC PROGRAMMING LANGUAGE
 * It was originally written in the C programming language
 * Copyright (c) 2024 Cyril John Magayaga (cjmagayaga957@gmail.com)
 */
#include <stdio.h>
#include <stdarg.h>

void c_printf(const char* message, ...) {
    va_list args;
    va_start(args, message);
    vprintf(message, args);
    va_end(args);
    printf("");
}

void c_println(const char* message, ...) {
    va_list args;
    va_start(args, message);
    vprintf(message, args);
    va_end(args);
    printf("\n");
}
