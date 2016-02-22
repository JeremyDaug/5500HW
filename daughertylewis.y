/*
 *  bison specifications for the MIPL language.
 *  Written to meet requirements for CS 5500, Spring 2016.
 */

/*
 *  Declaration section.
 */
%{

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <string>
#include <ctype.h>
#include <stack>
#include <list>
#include "SymbolTable.h"
using namespace std;

void ignoreComment();
int ckInt();
void prRule(const char*, const char*);
void printTokenInfo(const char* tokenType, 
                    const char* lexeme);
void prSymbolTableAddition(const string identName, 
                           const TYPE_INFO typeInfo);
void beginScope();
void endScope();
void cleanUp();
TYPE_INFO findEntryInAnyScope(const string theName);

int yyerror(const char*);

extern "C" {
    int yyparse(void);
    int yylex(void);
    int yywrap() {return 1;}
}

#define MAX_INT	"2147483647"

#define OUTPUT_TOKENS	     1
#define OUTPUT_PRODUCTIONS 1
#define OUTPUT_ST_MGT      1

#define POSITIVE		1
#define NEGATIVE		-1

int lineNum = 1;                   // source line number

stack<SYMBOL_TABLE> scopeStack;    // stack of scope hashtables
list<string> variableNames;		  // list of declared variables

%}

%union {
  char* text;
  char ch;
  int num;
  bool boolean;
  TYPE_INFO typeInfo;
};

/*
 *  Token declaration. 'N_...' for rules, 'T_...' for tokens.
 *  Note: tokens are also used in the flex specification file.
 */
%token      T_LPAREN    T_RPAREN    T_MULT	    T_PLUS
%token      T_COMMA     T_MINUS     T_DOT       T_DOTDOT
%token      T_COLON     T_ASSIGN    T_SCOLON    T_LT
%token      T_LE        T_NE        T_EQ        T_GT
%token      T_GE        T_LBRACK    T_RBRACK    T_DO
%token      T_AND       T_ARRAY     T_BEGIN     T_BOOL
%token      T_CHAR      T_CHARCONST T_DIV 	     T_END       
%token      T_FALSE     T_IDENT	    T_IF        T_INT
%token 	 T_INTCONST 
%token      T_NOT       T_OF        T_OR        T_PROC
%token      T_PROG      T_READ      T_TRUE      
%token      T_VAR       T_WHILE     T_WRITE     T_UNKNOWN

%token      ST_EOF

%type <ch> T_CHARCONST
%type <num> N_IDX T_INTCONST N_INTCONST N_SIGN
%type <text> T_IDENT N_IDENT
%type <typeInfo> N_ARRAY N_BOOLCONST N_CONST 
%type <typeInfo> N_ENTIREVAR N_ARRAYVAR
%type <typeInfo> N_VARIDENT N_FACTOR N_TERM N_VARIABLE 
%type <typeInfo> N_INPUTVAR N_TYPE
%type <typeInfo> N_IDXRANGE N_EXPR N_SIMPLE N_SIMPLEEXPR 
%type <typeInfo> N_PROCIDENT N_PROCSTMT N_IDXVAR 

/*
 *  To eliminate ambiguities.
 */
%nonassoc   T_THEN
%nonassoc   T_ELSE

/*
 *  Starting point.
 */
%start      N_START

/*
 *  Translation rules.
 */
%%
N_START         : N_PROG
                    {
                    prRule("N_START", "N_PROG");
			    printf("\n---- Completed parsing ----\n\n");
			    return 0;
                    }
                ;
N_ADDOP         : T_PLUS
                    {
                    prRule("N_ADDOP", "T_PLUS");
                    }
                | T_MINUS
                    {
                    prRule("N_ADDOP", "T_MINUS");
                    }
                | T_OR
                    {
                    prRule("N_ADDOP", "T_OR");
                    }
                ;
N_ADDOPLST      : /* epsilon */
                    {
                    prRule("N_ADDOPLST", "epsilon");
                    }
                | N_ADDOP N_TERM N_ADDOPLST
                    {
                    prRule("N_ADDOPLST", 
                           "N_ADDOP N_TERM N_ADDOPLST");
                    }
                ;
N_ARRAY         : T_ARRAY T_LBRACK N_IDXRANGE T_RBRACK T_OF
			  N_SIMPLE
                    {
                    	prRule("N_ARRAY",
                        "T_ARRAY T_LBRACK N_IDXRANGE T_RBRACK T_OF N_SIMPLE");
				$$.type = ARRAY; 
                      $$.startIndex = $3.startIndex;
                      $$.endIndex = $3.endIndex;
		        	$$.baseType = $6.type;
                    }
                ;
N_ARRAYVAR      : N_ENTIREVAR
                    {
                    prRule("N_ARRAYVAR", "N_ENTIREVAR");
			    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
                    }
                ;
N_ASSIGN        : N_VARIABLE T_ASSIGN N_EXPR
                    {
                    prRule("N_ASSIGN", 
                           "N_VARIABLE T_ASSIGN N_EXPR");
                    }
                ;
N_BLOCK         : N_VARDECPART N_PROCDECPART N_STMTPART
                    {
                    prRule("N_BLOCK", 
                        "N_VARDECPART N_PROCDECPART N_STMTPART");
                    endScope();
                    }
                ;
N_BOOLCONST     : T_TRUE
                    {
                    prRule("N_BOOLCONST", "T_TRUE");
			    $$.type = BOOL; 
                    $$.startIndex = NOT_APPLICABLE;
			    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
                    }
                | T_FALSE
                    {
                    prRule("N_BOOLCONST", "T_FALSE");
			    $$.type = BOOL; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
                    }
                ;
N_COMPOUND      : T_BEGIN N_STMT N_STMTLST T_END
                    {
                    prRule("N_COMPOUND", 
                           "T_BEGIN N_STMT N_STMTLST T_END");
                    }
                ;
N_CONDITION     : T_IF N_EXPR T_THEN N_STMT
                    {
                    prRule("N_CONDITION", 
                           "T_IF N_EXPR T_THEN N_STMT");
                    }
                | T_IF N_EXPR T_THEN N_STMT T_ELSE N_STMT
                    {
                    prRule("N_CONDITION",
                      "T_IF N_EXPR T_THEN N_STMT T_ELSE N_STMT");
                    }
                ;
N_CONST         : N_INTCONST
                    {
                    prRule("N_CONST", "N_INTCONST");
			    $$.type = INT; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
                    }
                | T_CHARCONST
                    {
                    prRule("N_CONST", "T_CHARCONST");
			    $$.type = CHAR; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
                    }
                | N_BOOLCONST
                    {
                    prRule("N_CONST", "N_BOOLCONST");
			    $$.type = BOOL; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
                    }
                ;
N_ENTIREVAR     : N_VARIDENT
                    {
                    prRule("N_ENTIREVAR", "N_VARIDENT");
                    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
                    }
                ;
N_EXPR          : N_SIMPLEEXPR
                    {
                    prRule("N_EXPR", "N_SIMPLEEXPR");
			    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
                    }
                | N_SIMPLEEXPR N_RELOP N_SIMPLEEXPR
                    {
                    prRule("N_EXPR", 
                           "N_SIMPLEEXPR N_RELOP N_SIMPLEEXPR");
			    $$.type = BOOL; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
                    }
                ;
N_FACTOR        : N_SIGN N_VARIABLE
                    {
                    prRule("N_FACTOR", "N_SIGN N_VARIABLE");
      		    $$.type = $2.type; 
                    $$.startIndex = $2.startIndex;
                    $$.endIndex = $2.endIndex;
		         $$.baseType = $2.baseType;
                    }
                | N_CONST
                    {
                    prRule("N_FACTOR", "N_CONST");
			    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
                    }
                | T_LPAREN N_EXPR T_RPAREN
                    {
                    prRule("N_FACTOR", 
                          "T_LPAREN N_EXPR T_RPAREN");
			    $$.type = $2.type; 
                    $$.startIndex = $2.startIndex;
                    $$.endIndex = $2.endIndex;
		         $$.baseType = $2.baseType;
                    }
                | T_NOT N_FACTOR
                    {
                    prRule("N_FACTOR", "T_NOT N_FACTOR");
			    $$.type = BOOL; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
                    }
                ;
N_IDENT         : T_IDENT
                    {
                    prRule("N_IDENT", "T_IDENT");
                    $$ = $1;
                    }
                ;
N_IDENTLST      : /* epsilon */
                    {
                    prRule("N_IDENTLST", "epsilon");
                    }
                | T_COMMA N_IDENT N_IDENTLST
                    {
                    prRule("N_IDENTLST", 
                           "T_COMMA N_IDENT N_IDENTLST");
			    string varName = string($2);
			    variableNames.push_front(varName);
                    }
                ;
N_IDX           : N_INTCONST
                    {
                    prRule("N_IDX", "N_INTCONST");
                    $$ = $1;
                    }
                ;
N_IDXRANGE      : N_IDX T_DOTDOT N_IDX
                    {
                    prRule("N_IDXRANGE", "N_IDX T_DOTDOT N_IDX");
		         $$.type = INDEX_RANGE; 
                    $$.startIndex = $1;
                    $$.endIndex = $3;
		         $$.baseType = NOT_APPLICABLE;
                    }
                ;
N_IDXVAR        : N_ARRAYVAR T_LBRACK N_EXPR T_RBRACK
                    {
                    prRule("N_IDXVAR", 
                          "N_ARRAYVAR T_LBRACK N_EXPR T_RBRACK");
                    }
                ;
N_INPUTLST      : /* epsilon */
                    {
                    prRule("N_INPUTLST", "epsilon");
                    }
                | T_COMMA N_INPUTVAR N_INPUTLST
                    {
                    prRule("N_INPUTLST", 
                           "T_COMMA N_INPUTVAR N_INPUTLST");
                    }
                ;
N_INPUTVAR      : N_VARIABLE
                    {
                    prRule("N_INPUTVAR", "N_VARIABLE");
			    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
                    }
                ;
N_INTCONST      : N_SIGN T_INTCONST
                    {
                    prRule("N_INTCONST", "N_SIGN T_INTCONST");
			    $$ = $1 * $2;
                    }
                ;
N_MULTOP        : T_MULT
                    {
                    prRule("N_MULTOP", "T_MULT");
                    }
                | T_DIV
                    {
                    prRule("N_MULTOP", "T_DIV");
                    }
                | T_AND
                    {
                    prRule("N_MULTOP", "T_AND");
                    }
                ;
N_MULTOPLST     : /* epsilon */
                    {
                    prRule("N_MULTOPLST", "epsilon");
                    }
                | N_MULTOP N_FACTOR N_MULTOPLST
                    {
                    prRule("N_MULTOPLST", "N_MULTOP N_FACTOR N_MULTOPLST");
                    }
                ;
N_OUTPUT        : N_EXPR
                    {
                    prRule("N_OUTPUT", "N_EXPR");
                    }
                ;
N_OUTPUTLST     : /* epsilon */
                    {
                    prRule("N_OUTPUTLST", "epsilon");
                    }
                | T_COMMA N_OUTPUT N_OUTPUTLST
                    {
                    prRule("N_OUTPUTLST", "T_COMMA N_OUTPUT N_OUTPUTLST");
                    }
                ;
N_PROCDEC       : N_PROCHDR N_BLOCK
                    {
                    prRule("N_PROCDEC", "N_PROCHDR N_BLOCK");
                    }
                ;
N_PROCHDR       : T_PROC T_IDENT T_SCOLON
                    {
                    prRule("N_PROCHDR", 
                           "T_PROC T_IDENT T_SCOLON");
			    string lexeme = string($2);
			    TYPE_INFO info = {PROCEDURE, NOT_APPLICABLE,
				           NOT_APPLICABLE, NOT_APPLICABLE};
			    prSymbolTableAddition(lexeme, info);
                    bool success = scopeStack.top().addEntry
                               (SYMBOL_TABLE_ENTRY(lexeme,info));
                	    if (! success) {
                      yyerror("Multiply defined identifier");
                      return(0);
                    }

			    beginScope();
                    }
                ;
N_PROCDECPART   : /* epsilon */
                    {
                    prRule("N_PROCDECPART", "epsilon");
                    }
                | N_PROCDEC T_SCOLON N_PROCDECPART
                    {
                    prRule("N_PROCDECPART",
                           "N_PROCDEC T_SCOLON N_PROCDECPART");
                    }
                ;
N_PROCIDENT     : T_IDENT
                    {
                    prRule("N_PROCIDENT", "T_IDENT");
			    string ident = string($1);
                	    TYPE_INFO typeInfo = 
						findEntryInAnyScope(ident);
                	    if (typeInfo.type == UNDEFINED) {
                	      yyerror("Undefined identifier");
                	      return(0);
               	    }
			    $$.type = typeInfo.type;
			    $$.startIndex = typeInfo.startIndex;
			    $$.endIndex = typeInfo.endIndex;
			    $$.baseType = typeInfo.baseType;
                    }
                ;
N_PROCSTMT      : N_PROCIDENT
                    {
                    prRule("N_PROCSTMT", "N_PROCIDENT");
			    $$.type = $1.type;
			    $$.startIndex = $1.startIndex;
			    $$.endIndex = $1.endIndex;
			    $$.baseType = $1.baseType;
                    }
                ;
N_PROG          : N_PROGLBL T_IDENT T_SCOLON 
                    {
                    prRule("N_PROG",
                     "N_PROGLBL T_IDENT T_SCOLON N_BLOCK T_DOT");
			    string lexeme = string($2);
			    TYPE_INFO info = {PROGRAM, NOT_APPLICABLE,
				           NOT_APPLICABLE, NOT_APPLICABLE};
			    prSymbolTableAddition(lexeme, info);
                    bool success = scopeStack.top().addEntry
                               (SYMBOL_TABLE_ENTRY(lexeme,info));
			    }
			  N_BLOCK T_DOT
		         { }
                ;
N_PROGLBL       : T_PROG
                    {
                    prRule("N_PROGLBL", "T_PROG");
			    beginScope();
                    }
                ;
N_READ          : T_READ T_LPAREN N_INPUTVAR N_INPUTLST T_RPAREN
                    {
                    prRule("N_READ",
                        "T_READ T_LPAREN N_INPUTVAR N_INPUTLST T_RPAREN");
			   }
                ;
N_RELOP         : T_LT
                    {
                    prRule("N_RELOP", "T_LT");
                    }
                | T_GT
                    {
                    prRule("N_RELOP", "T_GT");
                    }
                | T_LE
                    {
                    prRule("N_RELOP", "T_LE");
                    }
                | T_GE
                    {
                    prRule("N_RELOP", "T_GE");
                    }
                | T_EQ
                    {
                    prRule("N_RELOP", "T_EQ");
                    }
                | T_NE
                    {
                    prRule("N_RELOP", "T_NE");
                    }
                ;
N_SIGN          : /* epsilon */
                    {
                    prRule("N_SIGN", "epsilon");
			    $$ = POSITIVE;
                    }
                | T_PLUS
                    {
                    prRule("N_SIGN", "T_PLUS");
			    $$ = POSITIVE;
                    }
                | T_MINUS
                    {
                    prRule("N_SIGN", "T_MINUS");
			    $$ = NEGATIVE;
                    }
                ;
N_SIMPLE        : T_INT
                    {
                    prRule("N_SIMPLE", "T_INT");
			    $$.type = INT; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
                    }
                | T_CHAR
                    {
                    prRule("N_SIMPLE", "T_CHAR");
			    $$.type = CHAR; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
                    }
                | T_BOOL
                    {
                    prRule("N_SIMPLE", "T_BOOL");
			    $$.type = BOOL; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
                    }
                ;
N_SIMPLEEXPR    : N_TERM N_ADDOPLST
                    {
                    prRule("N_SIMPLEEXPR", 
                           "N_TERM N_ADDOPLST");
			    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
                    }
                ;
N_STMT          : N_ASSIGN
                    {
                    prRule("N_STMT", "N_ASSIGN");
                    }
                | N_PROCSTMT
                    {
                    prRule("N_STMT", "N_PROCSTMT");
                    }
                | N_READ
                    {
                    prRule("N_STMT", "N_READ");
                    }
                | N_WRITE
                    {
                    prRule("N_STMT", "N_WRITE");
                    }
                | N_CONDITION
                    {
                    prRule("N_STMT", "N_CONDITION");
                    }
                | N_WHILE
                    {
                    prRule("N_STMT", "N_WHILE");
                    }
                | N_COMPOUND
                    {
                    prRule("N_STMT", "N_COMPOUND");
                    }
                ;
N_STMTLST       : /* epsilon */
                    {
                    prRule("N_STMTLST", "epsilon");
                    }
                | T_SCOLON N_STMT N_STMTLST
                    {
                    prRule("N_STMTLST", 
                           "T_SCOLON N_STMT N_STMTLST");
                    }
                ;
N_STMTPART      : N_COMPOUND
                    {
                    prRule("N_STMTPART", "N_COMPOUND");
                    }
                ;
N_TERM          : N_FACTOR N_MULTOPLST
                    {
                    prRule("N_TERM", "N_FACTOR N_MULTOPLST");
			    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
                    }
                ;
N_TYPE          : N_SIMPLE
                    {
                    prRule("N_TYPE", "N_SIMPLE");
			    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
                    }
                | N_ARRAY
                    {
                    prRule("N_TYPE", "N_ARRAY");
			    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
                    }
                ;
N_VARDEC        : N_IDENT N_IDENTLST T_COLON N_TYPE 
                    {
                    prRule("N_VARDEC", 
                           "N_IDENT N_IDENTLST T_COLON N_TYPE");
			    string varName = string($1);
			    variableNames.push_front(varName);
		
			    for (std::list<string>::iterator
                          it=variableNames.begin();
			          it != variableNames.end(); it++) {
				 string varName = string(*it);
			       prSymbolTableAddition(varName, $4);
                       bool success = scopeStack.top().addEntry
                               (SYMBOL_TABLE_ENTRY(varName, $4));
                	       if (! success) {
                         yyerror("Multiply defined identifier");
                         return(0);
               	       }
				}
                     variableNames.clear();
                    }
                ;
N_VARDECLST     : /* epsilon */
                    {
                    prRule("N_VARDECLST", "epsilon");
                    }
                | N_VARDEC T_SCOLON N_VARDECLST
                    {
                    prRule("N_VARDECLST", 
                           "N_VARDEC T_SCOLON N_VARDECLST");
                   }
                ;
N_VARDECPART    : /* epsilon */
                    {
                    prRule("N_VARDECPART", "epsilon");
                    }
                | T_VAR N_VARDEC T_SCOLON N_VARDECLST
                    {
                    prRule("N_VARDECPART",
                        "T_VAR N_VARDEC T_SCOLON N_VARDECLST");
                    }
                ;
N_VARIABLE      : N_ENTIREVAR
                    {
                    prRule("N_VARIABLE", "N_ENTIREVAR");
			    $$.type = $1.type;
			    $$.startIndex = $1.startIndex;
			    $$.endIndex = $1.endIndex;
			    $$.baseType = $1.baseType;
                    }
                | N_IDXVAR
                    {
                    prRule("N_VARIABLE", "N_IDXVAR");
			    $$.type = $1.type;
			    $$.startIndex = $1.startIndex;
			    $$.endIndex = $1.endIndex;
			    $$.baseType = $1.baseType;
                    }
                ;
N_VARIDENT      : T_IDENT
                    {
                    prRule("N_VARIDENT", "T_IDENT");
			    string ident = string($1);
                	    TYPE_INFO typeInfo = 
						findEntryInAnyScope(ident);
                	    if (typeInfo.type == UNDEFINED) {
                	      yyerror("Undefined identifier");
                	      return(0);
               	    }
			    $$.type = typeInfo.type;
			    $$.startIndex = typeInfo.startIndex;
			    $$.endIndex = typeInfo.endIndex;
			    $$.baseType = typeInfo.baseType;
			    }
                ;
N_WHILE         : T_WHILE N_EXPR T_DO N_STMT
                    {
                    prRule("N_WHILE", 
                           "T_WHILE N_EXPR T_DO N_STMT");
                    }
                ;
N_WRITE         : T_WRITE T_LPAREN N_OUTPUT N_OUTPUTLST T_RPAREN
                    {
                    prRule("N_WRITE",
                      "T_WRITE T_LPAREN N_OUTPUT N_OUTPUTLST T_RPAREN");
                    }
                ;
%%

#include    "lex.yy.c"
extern FILE *yyin;

void prRule(const char *lhs, const char *rhs) 
{
  if (OUTPUT_PRODUCTIONS)
    printf("%s -> %s\n", lhs, rhs);
  return;
}

int yyerror(const char *s) 
{
  printf("Line %d: %s\n", lineNum, s);
  cleanUp();
  exit(1);
}

int ckInt() 
{
  char *ptr;
  int	rc = 0;
  ptr = yytext;

  /* ignore sign and leading zeroes */
  if (*ptr == '-' || *ptr == '+')
    ++ptr;
  while (*ptr == '0')
    ++ptr;

  switch (*ptr) 
  {
  case '1':	/* ALL are valid */
			break;

  case '2':	/* it depends */
			if (strcmp(MAX_INT, ptr) < 0)
			  rc = 1;
			break;

  default:	     /* ALL are invalid */
			rc = 1;
			break;
  }
  return rc;
}

void ignoreComment() 
{
  char c, pc = 0;

  /* read and ignore input until you get an ending token */
  while (((c = yyinput()) != ')' || pc != '*') && c != 0) {
    pc = c;
    if (c == '\n') lineNum++;
  }

  return;
}

void printTokenInfo(const char* tokenType, 
                    const char* lexeme) 
{
  if (OUTPUT_TOKENS)
    printf("TOKEN: %-15s  LEXEME: %s\n", tokenType, lexeme);
}

void beginScope() 
{
  scopeStack.push(SYMBOL_TABLE());
  if (OUTPUT_ST_MGT) 
    printf("\n___Entering new scope...\n\n");
}

void endScope() 
{
  scopeStack.pop();
  if (OUTPUT_ST_MGT) 
    printf("\n___Exiting scope...\n\n");
}

void prSymbolTableAddition(const string identName, 
                           const TYPE_INFO typeInfo) 
{
  if (OUTPUT_ST_MGT) 
  {
   char *cstr = new char[identName.length() + 1];
   strcpy(cstr, identName.c_str());
   printf("___Adding %s to symbol table with type ", cstr);
   delete [] cstr;
   switch (typeInfo.type) {
	case PROGRAM	: printf("PROGRAM\n"); break;
	case PROCEDURE	: printf("PROCEDURE\n"); break;
	case INT		: printf("INTEGER\n"); break;
	case CHAR		: printf("CHAR\n"); break;
	case BOOL		: printf("BOOLEAN\n"); break;
	case ARRAY		: printf("ARRAY ");
				  printf("%d .. %d OF ",
				         typeInfo.startIndex, 
				         typeInfo.endIndex);
				  switch (typeInfo.baseType) {
				    case INT : printf("INTEGER\n"); 
                                     break;
				    case CHAR: printf("CHAR\n"); 
                                     break;
				    case BOOL: printf("BOOLEAN\n"); 
                                     break;
				    default  : printf("UNKNOWN\n"); 
                                     break;
				  }
				  break;
	default 		: printf("UNKNOWN\n"); break;
   }
  }
}

TYPE_INFO findEntryInAnyScope(const string theName) 
{
  TYPE_INFO info = {UNDEFINED, NOT_APPLICABLE, NOT_APPLICABLE,
                    NOT_APPLICABLE};
  if (scopeStack.empty( )) return(info);
  info = scopeStack.top().findEntry(theName);
  if (info.type != UNDEFINED)
    return(info);
  else { // check in "next higher" scope
	   SYMBOL_TABLE symbolTable = scopeStack.top( );
	   scopeStack.pop( );
	   info = findEntryInAnyScope(theName);
	   scopeStack.push(symbolTable); // restore the stack
	   return(info);
  }
}

void cleanUp() 
{
  if (scopeStack.empty()) 
    return;
  else {
        scopeStack.pop();
        cleanUp();
  }
}

int main()
{
  // loop as long as there is anything to parse
  do {
    yyparse();
  } while (!feof(yyin));

  cleanUp();
  return 0;
