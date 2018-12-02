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

Token lookahead;
int synerrno;
/*according to his notes we should add more variables eventually*/



/*: You are not allowed to copy the keyword table in parser.h or
parser.c. You must use a proper declaration to create an external link to the one
defined in table.h.
Similarly, you must use the string literal table to print the sting literals.*/


/*not sure if this is what he wants, for what I understand we cannot include table.h we should get the variable with the extern keyword*/
extern char * kw_table[];
extern Buffer * str_LTBL;
extern int line;


/*function declaration*/
void parser();
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe();
void gen_incode(char* s);




/*For each of your grammar productions write a function named after the name of the
production. For example:
void program(void){
match(KW_T,PLATYPUS);match(LBR_T,NO_ATTR);opt_statements();
match(RBR_T,NO_ATTR);
gen_incode("PLATY: Program parsed");
}
Writing a production function, follow the substeps below.
Step 7.1:
To implement the Parser, you must use the modified grammar (see Task 1). Before
writing a function, analyze carefully the production. If the production consists of a single
production rule (no alternatives), write the corresponding function without using the
FIRST set (see above). If you use the lookahead to verify in advance whether to
proceed with the production and call the syn_printe() function, your output might report
quite different syntax errors than my parser will reports.

Example: The production:
<input statement> ->
INPUT (<variable list>);
MUST be implemented as follows:
void input_statement(void){
match(KW_T,INPUT);match(LPR_T,NO_ATTR);variable_list();
match(RPR_T,NO_ATTR); match(EOS_T,NO_ATTR);
gen_incode("PLATY: Input statement parsed");
}
AND MUST NOT be implemented as shown below:
void input_statement(void){
if(lookahead.code == KW_T
&& lookahead.attribute.get_int== INPUT) {
match(KW_T,INPUT);match(LPR_T,NO_ATTR);variable_list();
match(RPR_T,NO_ATTR); match(EOS_T,NO_ATTR);
gen_incode("PLATY: Input statement parsed");
}else
syn_printe();
}
This implementation will �catch� the syntax error but will prevent the match() function
from calling the error handler at the right place.




*/















#endif