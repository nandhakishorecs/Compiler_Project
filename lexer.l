%{ 
   #include "y.tab.h"
   
   char* getToken();
   
   int LT = 1;
   int LE = 2;
   int GT  =3;
   int GE = 4;
   int NE = 5;
   int EQ = 6;
   
   int ADD = 1;
   int SUB = 2;
   int MUL = 3;
   int DIV = 4;
   int MOD = 5;
   
   int lineno = 1;
%}

%option noyywrap
%option yylineno

/* regular definitions */

ws              [ \t\n]
alphabets	[A-Za-z]
numbers	[0-9]
id		({alphabets}|$)({alphabets}|{numbers}|_|$)*
constant	{numbers}+(\.{numbers}+)?(E[+|-]?{numbers}+)?
literal	\".*\"
relops		[<|<=|>|>=|==|\\=]
arithmatic_ops	[+|-|*|/|%]
operators	({relops}|{arithmatic_ops}|=)


%%

[ ]		{ ; }

[\t]		{ return TAB; }

[\n]		{ lineno++; }

"suppose"	{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return SUPPOSE;
		}
"ifnot"		{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return(IFNOT);
		}
"forever"	{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return(FOREVER);
		}
"until"		{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return(UNTIL);
		}
"print"		{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return(PRINT);
		}
"scan"		{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return(SCAN);
		}
"true"		{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return(TRUE);
		}
"false"	{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return(FALSE);
		}
"^import.*"	{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return(IMPORT);
		}
"return"	{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return(RETURN);
		}
"func"		{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return(FUNC); 
		}
{id}		{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return(ID);
 		}
 		
{constant}/({ws}|{operators}|:|\)|{operators}{constant}|{operators}{id})		{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return(CONSTANT);
		}
		
{constant}[^ \t\n]+	{ char *t;
			  sprintf(t, "Invalid identifier %s", yytext);
			  yyerror(t);}

{literal}	{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return(LITERAL);
		}
"<"		{ printf("\n%s", yytext); 
		  yylval.nd_obj2.value = LT;
		  strcpy(yylval.nd_obj2.name, yytext);
		  return(RELOPS);
		}
"<="		{ printf("\n%s", yytext);
		  yylval.nd_obj2.value = LE;
		  strcpy(yylval.nd_obj2.name, yytext);
		  return(RELOPS);
		}
">"		{ printf("\n%s", yytext);
		  yylval.nd_obj2.value = GT;
		  strcpy(yylval.nd_obj2.name, yytext);
		  return(RELOPS);
		}
">="		{ printf("\n%s", yytext); 
		  yylval.nd_obj2.value=GE;
		  strcpy(yylval.nd_obj2.name, yytext);
		  return(RELOPS);
		}
"="		{ printf("\n%s", yytext);
		  return(ASSIGN);
		}
"\\="		{ printf("\n%s", yytext);
		  yylval.nd_obj2.value=NE;
		  strcpy(yylval.nd_obj2.name, yytext);
		  return(RELOPS);
		}
"=="		{ printf("\n%s", yytext);
		  yylval.nd_obj2.value=EQ;
		  strcpy(yylval.nd_obj2.name, yytext);
		  return(RELOPS);
		}
"+"		{ printf("\n%s", yytext);
		  yylval.nd_obj2.value=ADD;
		  strcpy(yylval.nd_obj2.name, yytext);
		  return(ARITHMETIC_OPS);
		}
"-"		{ printf("\n%s", yytext);
		  yylval.nd_obj2.value=SUB;
		  strcpy(yylval.nd_obj2.name, yytext);
		  return(ARITHMETIC_OPS);
		}
"*"		{ printf("\n%s", yytext);
		  yylval.nd_obj2.value=MUL;
		  strcpy(yylval.nd_obj2.name, yytext);
		  return(ARITHMETIC_OPS);
		}
"/"		{ printf("\n%s", yytext);
		  //yylval.value=DIV;
		  strcpy(yylval.nd_obj.name, yytext);
		  return(ARITHMETIC_OPS);
		}
"%"		{ printf("\n%s", yytext);
		  //yylval.value=MOD;
		  strcpy(yylval.nd_obj.name, yytext);
		  return(ARITHMETIC_OPS);
		}
":"		{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return(COLON);
		}
"("		{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return(LEFT_PARAN);
		}
")"		{ printf("\n%s", yytext);
		  strcpy(yylval.nd_obj.name, yytext);
		  return(RIGHT_PARAN);
		}
		
"#.*"		{ ; }
		
.		{ printf("\n%s", yytext);
		  char *t;
		  sprintf(t, "Unknown symbol %s", yytext);
		  yyerror(t);
		}

%%


char* getToken() {
    return strdup(yytext);
} 


