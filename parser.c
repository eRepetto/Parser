#include "parser.h"


void parser(Buffer * in_buf) {
	lookahead = malar_next_token();
	program();
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed\n");

}

void match(int pr_token_code, int pr_token_attribute) {

	/*The match() function matches two tokens: the current input token (lookahead) and the
	token required by the parser. The token required by the parser is represented by two
	integers - the token code (pr_token_code), and the token attribute
	(pr_token_attribute). The attribute code is used only when the token code is one of
	the following codes: KW_T, LOG_OP_T, ART_OP_T, REL_OP_T. In all other cases
	the token code is matched only.
	If the match is successful and the lookahead is SEOF_T, the function returns.
	If the match is successful and the lookahead is not SEOF_T, the function advances to
	the next input token by executing the statement:
	lookahead = malar_next_token ();
	If the new lookahead token is ERR_T, the function calls the error printing function
	syn_printe(), advances to the next input token by calling malar_next_token () again,
	increments the error counter synerrno, and returns.
	If the match is unsuccessful, the function calls the error handler
	syn_eh(pr_token_code) and returns.
	Note: Make your match() function as efficient as possible. This function is called many
	times during the parsing. The function will be graded with respect to design and efficiency */

	if (lookahead.code != SEOF_T) {

		switch (pr_token_code) {
		case KW_T:
		case LOG_OP_T:
		case ART_OP_T:
		case REL_OP_T:

			if (pr_token_attribute != lookahead.attribute.get_int) {
				syn_eh(pr_token_code);
				return;
			}

		default:
			break;
		}

	}
	else
		return;

	lookahead = malar_next_token();

	if (lookahead.code == ERR_T) {
		syn_printe();
		lookahead = malar_next_token();
		synerrno++;
		return;
	}

}


void syn_eh(int sync_token_code) {

	/*This function implements a simple panic
	mode error recovery.
	First, the function calls syn_printe() and increments the error counter. Then the
	function implements a panic mode error recovery: the function advances the input token
	(lookahead) until it finds a token code matching the one required by the parser
	(pr_token_code passed to the function as sync_token_code ).
	It is possible, when advancing, that the function can reach the end of the source file
	without finding the matching token. To prevent from overrunning the input buffer, before
	every move the function checks if the end of the file is reached. If the function looks for
	sync_token_code different from SEOF_T and reaches the end of the source file, the
	function calls exit(synerrno).
	If a matching token is found and the matching token is not SEOF_T, the function
	advances the input token one more time and returns. If a matching token is found and
	the matching token is SEOF_T, the function returns.*/

	syn_printe();
	++synerrno;

	do {
		lookahead = malar_next_token();
		if (lookahead.code == sync_token_code) {
			lookahead = malar_next_token();
			return;
		}

	} while (lookahead.code != SEOF_T);

}


/*he provided us this function I don't know if we have to make any change eventually*/
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
		break;
	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;
	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;
	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;
	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);


	}/*end switch*/
}/* end syn_printe()*/


void gen_incode(char* s) {

	printf("%s", s);

	/*Write the gen_incode() function. In Part 1 of this assignment the function takes a string
	as an argument and prints it. Later the function can be modified and used to emit
	intermediate (Bonus 1) or machine code. The function may be called any time a
	production is recognized (see parser()). The format of the message is: “PLATY:
	Program parsed”, “PLATY: Assignment statement parsed”, and so on (see the sample
	output files).
	*/

}


void program(void) {
	match(KW_T, PLATYPUS);  /*i don't know if we need to define a constant with the name platypus here, thats what i did*/
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed\n");
}

void opt_statements() {
	/* FIRST set: {AVID_T, SVID_T, KW_T(but not PLATYPUS, ELSE, THEN, REPEAT, TRUE, FALSE), e} */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT, TRUE, FALSE here
		and in statements_p()*/
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT
			&& lookahead.attribute.get_int != TRUE
			&& lookahead.attribute.get_int != FALSE) {
			statements();
			break;
		}
	default: /*empty string – optional statements*/;
		gen_incode("PLATY: Opt_statements parsed\n");
	}

}
/*i don't know about this but the first set is the same than op_statements, so i just copied and paste the same for now*/
void statements(void) {

	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT, TRUE, FALSE here
		and in statements_p()*/
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT
			&& lookahead.attribute.get_int != TRUE
			&& lookahead.attribute.get_int != FALSE) {
			statements();
			break;
		}
	default: /*empty string – optional statements*/;
		gen_incode("PLATY: Statements parsed\n");
	}

}

/* FIRST set: {AVID_T, FPL_T, INLT_T, SVID_T, STR_T} 
*  PRODUCTION: <conditional_expression> -> <logical OR expression>
*/
void conditional_expression(void) {
	logical_or_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/* FIRST set: {AVID_T, FPL_T, INL_T, SVID_T, STR_T} 
*  PRODUCTION: <logical OR expression> -> <logical AND expression><logical OR expression'>
*/
void logical_or_expression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
	case SVID_T:
	case STR_T: 
		logical_and_expression(); 
		break;
	default: 
		syn_printe();
	}

	logical_or_expression_p();
}

/* FIRST set: {LOG_OP_T(.OR.), e} 
*  PRODUCTION: <logical OR expression'> -> .OR.<logical AND expression><logical OR expression'> | e
*/
void logical_or_expression_p(void) {
	if (lookahead.code == LOG_OP_T && lookahead.attribute.get_int == OR) {
		match(LOG_OP_T, OR);
		logical_and_expression();
		logical_or_expression_p();
	}
	else /* Empty string - optional statements */
		gen_incode("PLATY: Logical OR expression parsed");
}

/* FIRST set: {AVID_T, FPL_T, INL_T, SVID_T, STR_T} 
*  PRODUCTION: <logical AND expression> -> <relational expression><logical AND expression'>
*/
void logical_and_expression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
	case SVID_T:
	case STR_T: 
		relational_expression(); 
		break;
	default: 
		syn_printe();
	}

	logical_and_expression_p();
}

/* FIRST set: { LOG_OP_T(.AND.), e} 
*  PRODUCTION: <logical AND expression'> -> .AND.<relational expression><logical AND expression'> | e
*/
void logical_and_expression_p(void) {
	if (lookahead.code == LOG_OP_T && lookahead.attribute.get_int == AND) {
		match(LOG_OP_T, AND);
		relational_expression();
		logical_and_expression_p();
	}
	else /* Empty string - optional statements */
		gen_incode("PLATY: Logical AND expression parsed");
}


/* FIRST set: {AVID_T, FPL_T, INL_T, SVID_T, STR_T} 
*  PRODUCTION: <relational expression> -> <primary a_relational expression><primary a_relational expression'>
*										| <primary s_relational expression><primary s_relational expression'>
*/
void relational_expression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T: 
		primary_a_relational_expression(); 
		break;
	case SVID_T:
	case STR_T: 
		primary_s_relational_expression(); 
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Relational expression parsed");
}

/* FIRST set: {AVID_T, FPL_T, INL_T} 
* PRODUCTION: <primary a_relational expression> -> AVID_T | FPL_T | INL_T
*/
void primary_a_relational_expression(void) {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}

/* FIRST set: {REL_OP_T(==, <>, >, <)}
* PRODUCTION: <primary a_relational expression'> -> == <primary a_relational expression>
*												| <> <primary a_relational expression>
*												| > <primary a_relational expression>
*												| < <primary a_relational expression>
*
*/
void primary_a_relational_expression_p(void) {
	if (lookahead.code == REL_OP_T) {
		switch (lookahead.attribute.get_int) {
		case EQ:
			match(REL_OP_T, EQ);
			break;
		case NE:
			match(REL_OP_T, NE);
			break;
		case GT:
			match(REL_OP_T, GT);
			break;
		case LT:
			match(REL_OP_T, LT);
			break;
		}
		primary_a_relational_expression();
	}
	else
		syn_printe();
}

/* FIRST set: {SVID_T, STR_T} 
* PRODUCTION: <primary s_relational expression> -> <primary string expression>
*/
void primary_s_relational_expression(void) {
	primary_string_expression();
	gen_incode("PLATY: Primary s_relational expression parsed");
}

/* FIRST set: {REL_OP_T(==, <>, >, <)} 
* PRODUCTION: <primary s_relational expression'> -> == <primary s_relational expression>
*												| <> <primary s_relational expression>
*												| > <primary s_relational expression>
*												| < <primary s_relational expression>
*/
void primary_s_relational_expression_p(void) {
	if (lookahead.code == REL_OP_T) {
		switch (lookahead.attribute.get_int) {
		case EQ:
			match(REL_OP_T, EQ);
			break;
		case NE:
			match(REL_OP_T, NE);
			break;
		case GT:
			match(REL_OP_T, GT);
			break;
		case LT:
			match(REL_OP_T, LT);
			break;
		}
		primary_s_relational_expression();
	}
	else
		syn_printe();
}

/* FIRST set: {SVID_T, STR_T}
* PRODUCTION: <primary string expression> -> SVID_T | STR_T
*/
void primary_string_expression(void) {
	switch (lookahead.code) {
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Primary string expression parsed");
}

/* FIRST set: {SVID_T, STR_T}
* PRODUCTION: <string expression> -> <primary string expression><string expression'>
*/
void string_expression(void) {
	switch (lookahead.code) {
	case SVID_T:
	case STR_T:
		primary_string_expression();
		string_expression_p();
		break;
	default:
		syn_printe();
	}
}

/* FIRST set: {SCC_OP_T(#), e}
* PRODUCTION: <string expression'> -> #<primary string expression><string expression'> | e
*/
void string_expression_p(void) {
	if (lookahead.code == SCC_OP_T) {
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_p();
	}
	else /* Empty string - optional statements */
		gen_incode("PLATY: String expression parsed");
}