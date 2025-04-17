#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#define LINE_BUF 1024

typedef struct {
    char name[32];
    char value[LINE_BUF];
} Variable;

Variable *vars = NULL;
int vars_count = 0;

// Utility: trim whitespace
char *ltrim(char *s) { while (isspace((unsigned char)*s)) s++; return s; }
char *rtrim(char *s) { char *end = s + strlen(s) - 1; while (end > s && isspace((unsigned char)*end)) *end-- = '\0'; return s; }
char *trim(char *s) { return rtrim(ltrim(s)); }

// Convert integer to string
char *to_string(int val) { static char buf[32]; snprintf(buf, sizeof(buf), "%d", val); return buf; }

// Find constant index, or -1 if not found
int find_variable_index(const char *name) {
    for (int i = 0; i < vars_count; i++) {
        if (strcmp(vars[i].name, name) == 0) return i;
    }
    return -1;
}

// CONST declaration
void handle_const(char *line) {
    char *eq = strstr(line, ":=");
    if (!eq) { fprintf(stderr, "Error: Invalid CONST definition.\n"); return; }
    *eq = '\0';
    char *name = trim(line + 6);
    char *value = trim(eq + 2);
    if (strlen(name) == 0) { fprintf(stderr, "Error: CONST name cannot be empty.\n"); return; }
    if (find_variable_index(name) >= 0) { fprintf(stderr, "Error: CONST '%s' already defined.\n", name); return; }
    vars = realloc(vars, (vars_count + 1) * sizeof(Variable));
    strncpy(vars[vars_count].name, name, sizeof(vars[0].name));
    int len = strlen(value);
    // Strip quotes if string literal
    if ((value[0] == '"' && value[len-1] == '"') || (value[0] == '\'' && value[len-1] == '\'')) {
        value[len-1] = '\0';
        strncpy(vars[vars_count].value, value + 1, LINE_BUF);
    } else {
        // Store expression or number as-is
        strncpy(vars[vars_count].value, value, LINE_BUF);
    }
    vars_count++;
}

// Forward declarations for parsing
void skip_ws(const char **s);
double parse_expression(const char **s);
double parse_term(const char **s);
double parse_power(const char **s);
double parse_unary(const char **s);
double parse_primary(const char **s);

// Skip whitespace
void skip_ws(const char **s) {
    while (isspace((unsigned char)**s)) (*s)++;
}

double parse_expression(const char **s) {
    skip_ws(s);
    double val = parse_term(s);
    skip_ws(s);
    while (**s == '+' || **s == '-') {
        char op = **s; (*s)++;
        double rhs = parse_term(s);
        val = (op == '+') ? val + rhs : val - rhs;
        skip_ws(s);
    }
    return val;
}

double parse_term(const char **s) {
    skip_ws(s);
    double val = parse_power(s);
    skip_ws(s);
    while (**s == '*' || **s == '/') {
        char op = **s; (*s)++;
        double rhs = parse_power(s);
        val = (op == '*') ? val * rhs : val / rhs;
        skip_ws(s);
    }
    return val;
}

double parse_power(const char **s) {
    skip_ws(s);
    double val = parse_unary(s);
    skip_ws(s);
    if (**s == '^') {
        (*s)++;
        double rhs = parse_power(s);
        val = pow(val, rhs);
    }
    return val;
}

double parse_unary(const char **s) {
    skip_ws(s);
    if (**s == '+') { (*s)++; return parse_unary(s); }
    if (**s == '-') { (*s)++; return -parse_unary(s); }
    return parse_primary(s);
}

double parse_primary(const char **s) {
    skip_ws(s);
    if (**s == '(') {
        (*s)++;
        double val = parse_expression(s);
        skip_ws(s);
        if (**s == ')') (*s)++;
        else fprintf(stderr, "Error: Missing ')'.\n");
        return val;
    }
    char *end;
    double num = strtod(*s, &end);
    if (end != *s) {
        *s = end;
        return num;
    }
    char namebuf[32]; int i = 0;
    while (isalnum((unsigned char)**s) || **s == '_') {
        if (i < (int)sizeof(namebuf)-1) namebuf[i++] = **s;
        (*s)++;
    }
    namebuf[i] = '\0';
    int idx = find_variable_index(namebuf);
    if (idx >= 0) return atof(vars[idx].value);
    fprintf(stderr, "Error: Undefined constant '%s'.\n", namebuf);
    return NAN;
}

// Evaluate expression entry
double evaluate_expression(const char *expr) {
    const char *p = expr;
    double val = parse_expression(&p);
    skip_ws(&p);
    if (*p != '\0') fprintf(stderr, "Error: Unexpected '%c' in expression.\n", *p);
    return val;
}

// PRINT both text, variables, and evaluated expressions
void handle_print(char *line) {
    char *start = strchr(line, '(');
    char *end = strrchr(line, ')');
    if (!start || !end || end <= start) { fprintf(stderr, "Error: Invalid PRINT.\n"); return; }
    *end = '\0';
    char *arg = trim(start + 1);
    if (arg[0] == '\\') {
        int idx = find_variable_index(arg + 1);
        if (idx >= 0) printf("%s\n", vars[idx].value);
        else fprintf(stderr, "Error: Undefined constant '%s'.\n", arg + 1);
    } else if (arg[0] == '$') {
        int idx = find_variable_index(arg + 1);
        if (idx >= 0) {
            double res = evaluate_expression(vars[idx].value);
            if (isnan(res)) fprintf(stderr, "Error: Invalid numeric expression in PRINT.\n");
            else printf("%g\n", res);
        } else fprintf(stderr, "Error: Undefined constant '%s'.\n", arg + 1);
    } else if ((arg[0] == '"' && arg[strlen(arg)-1] == '"') || (arg[0] == '\'' && arg[strlen(arg)-1] == '\'')) {
        arg[strlen(arg)-1] = '\0';
        printf("%s\n", arg + 1);
    } else {
        double res = evaluate_expression(arg);
        if (isnan(res)) fprintf(stderr, "Error: Invalid numeric expression in PRINT.\n");
        else printf("%g\n", res);
    }
}

// FOR / NEXT
void handle_for(char *line) {
    char var[32]; int start, endv;
    if (sscanf(line+3, "%31s := %d TO %d", var, &start, &endv) != 3) {
        fprintf(stderr, "Error: Invalid FOR syntax.\n"); return;
    }
    extern void update_variable(const char *, const char *);
    update_variable(var, to_string(start));
    for (int i = start; i <= endv; i++) {
        update_variable(var, to_string(i));
    }
}
void handle_next(char *line) { /* no-op */ }

// Update for PRINT/loops
void update_variable(const char *name, const char *value) {
    int idx = find_variable_index(name);
    if (idx < 0) { fprintf(stderr, "Error: Constant '%s' not defined.\n", name); return; }
    strncpy(vars[idx].value, value, LINE_BUF);
}

int main(int argc, char *argv[]) {
    if (argc != 2) { fprintf(stderr, "Usage: %s <filename>\n", argv[0]); return 1; }
    FILE *fp = fopen(argv[1], "r");
    if (!fp) { perror("Error opening file"); return 1; }
    char rawline[LINE_BUF];
    int in_main = 0, in_sub = 0;
    while (fgets(rawline, LINE_BUF, fp)) {
        char *line = trim(rawline);
        if (strncmp(line, "BEGIN", 5) == 0) { if (!in_main) in_main = 1; else in_sub = 1; continue; }
        if (strncmp(line, "END", 3) == 0) { if (in_sub) in_sub = 0; else in_main = 0; continue; }
        if (strncmp(line, "COMMENT", 7) == 0) continue;
        if (!in_main) { fputs("Error: PRINT() outside BEGIN-END block.\n", stderr); continue; }
        if (strncmp(line, "CONST ", 6) == 0) { handle_const(line); continue; }
        if (strncmp(line, "PRINT(", 6) == 0) { handle_print(line); continue; }
        if (strncmp(line, "FOR ", 4) == 0) { handle_for(line); continue; }
        if (strncmp(line, "NEXT ", 5) == 0) { handle_next(line); continue; }
    }
    fclose(fp);
    free(vars);
    return 0;
}
