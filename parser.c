#include "parser.h"


void parser(void) {
	lookahead = malar_next_token();
	program();
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed\n");

}

void match(int pr_token_code, int pr_token_attribute) {
	if (pr_token_code != lookahead.code) {
		syn_eh(pr_token_code);
		return;
	}

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
}


void program(void) {
	match(KW_T, PLATYPUS);  
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed\n");
}

/*<program> -> PLATYPUS {<opt_statements>}
FIRST(<opt_statements>) = {AVID_T, SVID_T, KW_T (IF, WHILE, READ, WRITE), e}*/
void opt_statements(void) {
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not … see above),e} */
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
		/* gen_incode("PLATY: Opt_statements parsed\n"); */
		break;
	}

}
/*<statements> -> <statement> <statements’>
FIRST(<statements>) = {AVID_T, SVID_T, KW_T (IF, WHILE, READ, WRITE)}*/
void statements(void) {
	statement();
	statements_p();
}

/*<statement> -> <assignment statement>|<selection statement>|<iteration statement>|<input statement>|<output statement>
FIRST(<statement>) = {AVID_T, SVID_T, KW_T (IF, WHILE, READ, WRITE)}
*/
void statement(void) {
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		assignment_statement();
		break;
	case KW_T:
		if (lookahead.attribute.kwt_idx == IF)
			selection_statement();
		else if (lookahead.attribute.kwt_idx == WHILE)
			iteration_statement();
		else if (lookahead.attribute.kwt_idx == READ)
			input_statement();
		else if (lookahead.attribute.kwt_idx == WRITE)
			output_statement();
		break;
	default:
		syn_printe();
	}
}


/*
<output statement > -> WRITE (<output list>)
FIRST(<output statement>) = {KW_T(WRITE)}
*/
void output_statement() {
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed\n");
}

/*
<output list> -> <opt_variable list> | STR_T
FIRST(<output list>) = {AVID_T, SVID_T, STR_T}
*/
void output_list(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		variable_list();
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed\n");
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed\n");
	}
}

/* FIRST set: {KW_T(READ)}
* PRODUCTION: <input statement> -> READ (<variable list>);
*/
void input_statement(void) {
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed\n");
}

/* FIRST set: {AVID_T, SVID_T}
* PRODUCTION: <variable list> -> <variable identifier><variable list'> 
*/
void variable_list(void) {
	variable_identifier();
	variable_list_p();
}

/* FIRST set: {AVID_T, SVID_T}
* PRODUCTION: <variable identifier> -> AVID_T | SVID_T
*/
void variable_identifier(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		match(lookahead.code, NO_ATTR);
		break;
	default:
		syn_printe();
	}
}

/* FIRST set: {COM_T, e}
* PRODUCTION: <variable list'> -> ,<variable identifier><variable list'> | e
*/
void variable_list_p(void) {
	if (lookahead.code == COM_T) {
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_p();
	}
	else
		gen_incode("PLATY: Variable list parsed\n");
}

/* FIRST set: {KW_T(WHILE)}
* PRODUCTION: <iteration statement> -> WHILE <pre-condition> 
	(<conditional expression>) REPEAT { <statements> };
*/
void iteration_statement(void) {
	match(KW_T, WHILE);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed\n");
}

/* FIRST set: {KW_T (IF)}
* PRODUCTION: <selection statement> -> IF <pre-condition> (<conditional expression>) THEN { <opt_statements> }
ELSE { < opt_statements> }
*/
void selection_statement(void) {
	match(KW_T, IF);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed\n");
}

/* FIRST set: {TRUE, FALSE}
* PRODUCTION: <pre-condition> -> TRUE | FALSE
*/
void pre_condition(void) {
	if (lookahead.code == KW_T && lookahead.attribute.get_int == TRUE)
		match(KW_T, TRUE);
	else if (lookahead.code == KW_T && lookahead.attribute.get_int == FALSE)
		match(KW_T, FALSE);
	else
		syn_printe();
}

/* FIRST set: {AVID_T, SVID_T, KW_T (IF, WHILE, READ, WRITE), e}
* PRODUCTION: <statements'> -> <statement> <statements'> | e
*/
void statements_p(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: 
		statement(); 
		statements_p();
		break;
	case KW_T:
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT
			&& lookahead.attribute.get_int != TRUE
			&& lookahead.attribute.get_int != FALSE) {
			statement();
			statements_p();
			break;
		}
	default: /* Empty string – optional statements */;
		/* gen_incode("PLATY: Statements parsed\n"); */
		break;
	}
}

/*<assignment statement> -> <assignment expression>;
FIRST(<assignment statement>) = {FIRST(<assignment expression>)}*/
void assignment_statement(void) {
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed\n");
}

/*<assignment expression> -> AVID = <arithmetic expression> | SVID = <string expression>
FIRST(<assignment expression>) = {AVID_T, SVID_T}*/
void assignment_expression(void) {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed\n");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		gen_incode("PLATY: Assignment expression (string) parsed\n");
		string_expression();
	default: break;
	}
}

/*<arithmetic expression> - > <unary arithmetic expression> | <additive arithmetic expression>
FIRST(<arithmetic expression>) = { ART_OP_T(-), ART_OP_T(+), AVID_T, FPL_T, INL_T, LPR_T}
*/
void arithmetic_expression(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op != MULT
			&& lookahead.attribute.arr_op != DIV) {
			unary_arithmetic_expression();
			break;
		}
		else {
			syn_printe();
			return;
		}
	case LPR_T:
	case AVID_T:
	case FPL_T:
	case INL_T:
		additive_arithmetic_expression();
	}
	gen_incode("PLATY: Arithmetic expression parsed\n");
}

/*
<additive arithmetic expression> -> <multiplicative arithmetic expression> <additive arithmetic expression’>
FIRST(<additive arithmetic expression>) = {AVID_T, FPL_T, INL_T, LPR_T, ART_OP_T(+), ART_OP_T(-)}
*/
void additive_arithmetic_expression(void) {
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_P();
}

/*
<additive arithmetic expression’>-> +< multiplicative arithmetic expression><additive arithmetic expression’>
|-< multiplicative arithmetic expression><additive arithmetic expression’>
|e
FIRST(<additive arithmetic expression’>) = { ART_OP_T(+), ART_OP_T(-), e}
*/
void additive_arithmetic_expression_P() {
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == PLUS) {
			match(lookahead.code, PLUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_P();
			gen_incode("PLATY: Additive arithmetic expression parsed\n");
			break;
		}
		else if (lookahead.attribute.arr_op == MINUS) {
			match(lookahead.code, MINUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_P();
			gen_incode("PLATY: Additive arithmetic expression parsed\n");
			break;
		}
		else
			return;
	default:
		return;
	}
}

/*<multiplicative arithmetic expression> -> <primary arithmetic expression> <multiplicative arithmetic expression’>
FIRST(<multiplicative arithmetic expression>) = { ART_OP_T(-), ART_OP_T(+), AVID_T, FPL_T, INL_T, LPR_T}
*/
void multiplicative_arithmetic_expression(void) {
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_p();
}

/*<multiplicative arithmetic expression’> ->
* <primary arithmetic expression><multiplicative arithmetic expression’>
|/ <primary arithmetic expression><multiplicative arithmetic expression’>
|e
FIRST(<multiplicative arithmetic expression’>) = { ART_OP_T(*)  , ART_OP_T(/)  , e}
*/
void multiplicative_arithmetic_expression_p() {

	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == MULT) {
			match(lookahead.code, MULT);
			multiplicative_arithmetic_expression();
			multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed\n");
		}

		else if (lookahead.attribute.arr_op == DIV) {
			match(lookahead.code, DIV);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_P();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed\n");
		}
		else
			return;

	default:
		return;
	}
}

/*<unary arithmetic expression> -> -  <primary arithmetic expression> | + <primary arithmetic expression>
FIRST(<unary arithmetic expression>) = { ART_OP_T(-), ART_OP_T(+) }
*/
void unary_arithmetic_expression(void) {

	switch (lookahead.code)
	{

	case ART_OP_T:
		if (lookahead.attribute.arr_op != MULT
			&& lookahead.attribute.arr_op != DIV) {

			match(lookahead.code, lookahead.attribute.get_int);
			primary_arithmetic_expression();
			gen_incode("PLATY: Unary arithmetic expression parsed\n");
		}
		break;
	default:
		syn_printe();
		return;
	}
}

/*<primary arithmetic expression> -> AVID_T| FPL_T| INL_T | (<arithmetic expression>)
FIRST(<primary arithmetic expression>) = { AVID_T, FPL_T, INL_T,RPR_T}  --> trying with the grammar like that
*/
void primary_arithmetic_expression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		match(lookahead.code, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed\n");

}

/* FIRST set: {AVID_T, FPL_T, INLT_T, SVID_T, STR_T}
*  PRODUCTION: <conditional_expression> -> <logical OR expression>
*/
void conditional_expression(void) {
	logical_or_expression();
	gen_incode("PLATY: Conditional expression parsed\n");
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
		gen_incode("PLATY: Logical OR expression parsed\n");
	}
	else {}/* Empty string - optional statements */
		/* gen_incode("PLATY: Logical OR expression parsed\n"); */
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
		logical_and_expression_p();
		break;
	default:
		syn_printe();
	}
}

/* FIRST set: { LOG_OP_T(.AND.), e}
*  PRODUCTION: <logical AND expression'> -> .AND.<relational expression><logical AND expression'> | e
*/
void logical_and_expression_p(void) {
	if (lookahead.code == LOG_OP_T && lookahead.attribute.get_int == AND) {
		match(LOG_OP_T, AND);
		relational_expression();
		logical_and_expression_p();
		gen_incode("PLATY: Logical AND expression parsed\n");
	}
	else {} /* Empty string */
		/* gen_incode("PLATY: Logical AND expression parsed\n"); */
		
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
		primary_a_relational_expression_p();
		break;
	case SVID_T:
	case STR_T:
		primary_s_relational_expression();
		primary_s_relational_expression_p();
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Relational expression parsed\n");
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
	gen_incode("PLATY: Primary a_relational expression parsed\n");
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
	gen_incode("PLATY: Primary s_relational expression parsed\n");
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
		gen_incode("PLATY: Primary s_relational expression parsed");
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
	gen_incode("PLATY: Primary string expression parsed\n");
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
		gen_incode("PLATY: String expression parsed");
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
		gen_incode("PLATY: String expression parsed\n");
}
