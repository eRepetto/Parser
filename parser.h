#pragma once

#ifndef PARSER_H_
#define PARSER_H_

#include <stdio.h>   
#include <ctype.h>  
#include <stdlib.h>  

#ifndef TOKEN_H_
#include "token.h"
#endif

#ifndef BUFFER_H_
#include "buffer.h"
#endif

/* Constant definitions */
#define  NO_ATTR -2 
#define ELSE	  0
#define FALSE	  1
#define IF        2
#define PLATYPUS  3
#define READ      4
#define REPEAT    5
#define THEN      6
#define TRUE      7
#define WHILE     8
#define WRITE     9

Token lookahead;
int synerrno; 
extern char * kw_table[];
extern Buffer * str_LTBL;
extern int line;
extern Token malar_next_token();

/* Function declarations */
void parser();
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe();
void gen_incode(char* s);
void program(void);
void opt_statements();
void statements();
void assignment_statement();
void assignment_expression();
void statements_p();
void statement();
void selection_statement();
void pre_condition();
void iteration_statement();
void input_statement();
void output_statement();
void arithmetic_expression();
void additive_arithmetic_expression();
void unary_arithmetic_expression();
void primary_arithmetic_expression();
void multiplicative_arithmetic_expression();
void additive_arithmetic_expression_P();
void multiplicative_arithmetic_expression_p();
void output_list();
void variable_list();
void variable_list_p();
void variable_identifier();
void conditional_expression();
void logical_or_expression();
void logical_or_expression_p();
void logical_and_expression();
void logical_and_expression_p();
void relational_expression();
void primary_a_relational_expression();
void primary_a_relational_expression_p();
void primary_s_relational_expression();
void primary_s_relational_expression_p();
void primary_string_expression();
void string_expression();
void string_expression_p();

#endif










