%{
    #include <stdio.h>
    #include <string.h>
    #include <stdlib.h>
    #include <ctype.h>
    #include <stdbool.h>
   
    
    void yyerror(const char *s);
    int yylex();
    
    // for symbol table
    
    void add(char);
    int search(char *);    
    
    struct symbolTableEntry {
        char* id_name;
        char* data_type;
        char* type;
        int line_no;
    } symbol_table[100];
    
    
    // for syntax tree
    
    struct node *head;
    struct node { 
	struct node *left; 
	struct node *right; 
	char *token; 
    };
    
    void printtree(struct node*);
    void printInorder(struct node *);
    void printLevelOrder(struct node*);
    void printCurrentLevel(struct node*, int);
    int height(struct node*);
    struct node* mknode(struct node *left, struct node *right, char *token);
    
    // for semantic analysis
    
    void check_declaration(char *);
    char *get_type(char *);
    void set_type(char *, char *);


    int count=0;
    int q;
    char type[10];
    extern int lineno;
    int sem_errors_i=0;
    int syn_errors_i=0;
    
    char sem_errors[10][100], syn_errors[10][100];
    char reserved[7][10] = {"suppose", "ifnot", "forever", "until", "return", "import", "func"};
    char icg[50][100];
    int ic_idx=0;
    int temp_var=0;
    int label=0; 
    int is_for=0;

    
%}

%union {
    struct var_name { 
	char name[100]; 
	struct node* nd;
    } nd_obj;
    
    struct var_name2 { 
	char name[100];
	int value; 
	struct node* nd;
    } nd_obj2;
    
    struct var_name3 { 
	char name[100]; 
	struct node* nd;
	char type[5];
    } nd_obj3;
    
    struct var_name4 {
       char name[100];
       struct node* nd;
       char if_body[5];
       char else_body[5];  
    } nd_obj4;
}


%token <nd_obj> SUPPOSE IFNOT FOREVER ID CONSTANT RELOPS ARITHMETIC_OPS LITERAL PUNCTUATOR ASSIGN UNTIL COLON LEFT_PARAN RIGHT_PARAN RETURN PRINT SCAN TRUE FALSE IMPORT TAB NEWLINE UNKNOWN FUNC

%type <nd_obj> program imports body block func_block print scan ifnot statement return ifnotTab

%type <nd_obj3> init value expression 

%type <nd_obj4> condition 

%%

program: imports body  	{ printf("\t\tprogram -> imports body\n");
				  $$.nd = mknode($1.nd, $2.nd, "program");
				  head = $$.nd;
				}
;

imports: imports imports	{ printf("\t\timports -> imports imports\n"); 
  				  $$.nd = mknode($1.nd, $2.nd, "imports");
				}
| IMPORT { add('H'); 
	   $$.nd = mknode(NULL, NULL, $1.name);
	 }
|        { $$.nd = NULL; }
;


body: FOREVER { add('K'); } COLON block  { printf("\t\tbody -> FOREVER: block\n"); 
					    $$.nd = mknode(NULL, $4.nd, $1.name);}
| FOREVER { add('K'); } UNTIL { add('K'); } condition COLON block	{ printf("\t\tbody -> FOREVER UNTIL condition: block\n"); 
  $$.nd = mknode($5.nd, $7.nd, $1.name);
  
	sprintf(icg[ic_idx++], "JUMP to %s\n", $5.if_body);
	sprintf(icg[ic_idx++], "\nLABEL %s:\n", $5.else_body);
	}
| SUPPOSE { add('K'); is_for = 0; } condition { sprintf(icg[ic_idx++], "\nLABEL %s:\n", $3.if_body); } COLON block { sprintf(icg[ic_idx++], "\nLABEL %s:\n", $3.else_body); } ifnot	{ printf("\t\tbody-> SUPPOSE condition: block ifnot\n"); 
							struct node *iff = mknode($3.nd, $6.nd, $1.name); 	
							$$.nd = mknode(iff, $6.nd, "suppose-until");
							
	sprintf(icg[ic_idx++], "GOTO next\n");
							} 
| statement	{ printf("\t\tbody -> statement\n"); 
		  $$.nd = $1.nd;
		}
| print	{ printf("\t\tbody -> print\n"); 
		  $$.nd = $1.nd;
		}
| FUNC { add('K'); } ID { add('V'); } LEFT_PARAN RIGHT_PARAN COLON func_block	{ printf("\t\tbody -> FUNC ID(): func_block\n"); }
| body body	{ printf("\t\tbody -> body body\n"); 
		  $$.nd = mknode($1.nd, $2.nd, "statements");
		}
| error block { sprintf(syn_errors[syn_errors_i], "Colon missing at line %d\n", lineno);
		  syn_errors_i++; }
| ifnot { sprintf(syn_errors[syn_errors_i], "matching SUPPOSE missing at line %d\n", lineno);
		  syn_errors_i++; }
;

block: TAB FOREVER { add('K'); } COLON TAB block { printf("\t\tblock -> '\t' FOREVER: block\n"); 
					    $$.nd = mknode(NULL, $6.nd, $2.name);}
| TAB FOREVER { add('K'); } UNTIL { add('K'); } condition COLON TAB block	{ printf("\t\tblock -> '\t' FOREVER UNTIL condition: block\n"); 
	  $$.nd = mknode($6.nd, $9.nd, $2.name);
	   
	sprintf(icg[ic_idx++], "JUMP to %s\n", $6.if_body);
	sprintf(icg[ic_idx++], "\nLABEL %s:\n", $6.else_body); }
| TAB SUPPOSE { add('K'); is_for = 0; } condition { sprintf(icg[ic_idx++], "\nLABEL %s:\n", $4.if_body); } COLON TAB block { sprintf(icg[ic_idx++], "\nLABEL %s:\n", $4.else_body); } TAB ifnotTab	{ printf("\t\tblock -> '\t' SUPPOSE condition: block TAB ifnotTab\n"); 
  struct node *iff = mknode($4.nd, $8.nd, $2.name); 	
  $$.nd = mknode(iff, $8.nd, "suppose-until");
  
	sprintf(icg[ic_idx++], "GOTO next\n");
}
| TAB statement    { printf("\t\tblock -> '\t' statement\n"); 
			  $$.nd = $2.nd; }
| TAB print  	{ printf("\t\tblock -> '\t' print\n"); 
			  $$.nd = $2.nd; }
| block block		{ printf("\t\tblock -> block block\n"); 
			  $$.nd = mknode($1.nd, $2.nd, "blocks"); }
| error FOREVER { sprintf(syn_errors[syn_errors_i], "Indentation missing at line %d\n", lineno);
		  syn_errors_i++; }
| error PRINT { sprintf(syn_errors[syn_errors_i], "Indentation missing at line %d\n", lineno);
		syn_errors_i++;  }
| error SCAN { sprintf(syn_errors[syn_errors_i], "Indentation missing at line %d\n", lineno);
		syn_errors_i++;   }
;

func_block: block return  { printf("\t\tfunc_block -> block return\n"); }
| TAB RETURN { add('K'); } expression   { printf("\t\tfunc_block ->   RETURN expression\n"); }
;

print: PRINT { add('K'); } LEFT_PARAN expression RIGHT_PARAN  { printf("\t\tprint -> PRINT(expression)\n"); 
  $1.nd = mknode(NULL, NULL, $1.name);
  $$.nd = mknode($1.nd, $4.nd, "print");
  sprintf(icg[ic_idx++], "%s %s\n",  $1.name, $4.name); }
;

scan: SCAN { add('K'); } LEFT_PARAN LITERAL RIGHT_PARAN	{ printf("\t\tscan -> SCAN(LITERAL)\n"); 
  $1.nd = mknode(NULL, NULL, $1.name);
  $$.nd = mknode($1.nd, $4.nd, "scan"); }
;

ifnot: IFNOT { add('K'); } COLON block	{ printf("\t\tifnot -> IFNOT: body\n"); 
						  $$.nd = mknode(NULL, $4.nd, $1.name); }
|		{ printf("\t\tifnot -> epsilon\n"); 
		  $$.nd = NULL; }
;

ifnotTab: IFNOT { add('K'); } COLON TAB block	{ printf("\t\tifnotTab -> IFNOT: TAB body\n"); 
						  $$.nd = mknode(NULL, $5.nd, $1.name); }
|		{ printf("\t\tifnotTab -> epsilon\n"); 
		  $$.nd = NULL; }
;


condition: expression RELOPS expression	{ printf("\t\tcondition -> expression RELOPS expression\n"); 
									
			  char *id_type = get_type($1.name); 
			  if(strcmp(id_type, $3.type)) {
			    if(!strcmp(id_type, "int")) {
			      if(!strcmp($3.type, "float")){
				struct node *temp = mknode(NULL, $3.nd, "floattoint");
				$$.nd = mknode($1.nd, temp, $2.name); 
			      }
			
		            }
		            else if(!strcmp(id_type, "float")) {
			       if(!strcmp($3.type, "int")){
			         struct node *temp = mknode(NULL, $3.nd, "inttofloat");
				  $$.nd = mknode($1.nd, temp, $2.name); 
			       }
			
		            }
		            else {
			   	    sprintf(sem_errors[sem_errors_i], "Line %d: Cannot add char* and number\n", lineno);
				    sem_errors_i++;
			   	}
		            
	                 }
			  else {
			    $$.nd = mknode($1.nd, $3.nd, $2.name); 
			  } 
			  
			  if(is_for) {  
        sprintf($$.if_body, "L%d", label++);  
        sprintf(icg[ic_idx++], "\nLABEL %s:\n", $$.if_body);
        sprintf(icg[ic_idx++], "\nif NOT (%s %s %s) GOTO L%d\n", $1.name, $2.name, $3.name, label);  
        sprintf($$.else_body, "L%d", label++); 
    } 
    else {  
        sprintf(icg[ic_idx++], "\nif (%s %s %s) GOTO L%d else GOTO L%d\n", $1.name, $2.name, $3.name, label, label+1);
        sprintf(icg[ic_idx++], "\nLABEL L%d:\n", label);
        sprintf($$.if_body, "L%d", label++);  
        sprintf($$.else_body, "L%d", label++); 
    }
			  
			  }
| TRUE 	{ add('B'); printf("\t\tcondition -> TRUE\n");
		  $$.nd = NULL; }
| FALSE	{ add('B'); printf("\t\tcondition -> FALSE\n");
		  $$.nd = NULL; }

;

statement: ID { add('V'); } init	{ printf("\t\tstatement -> ID init\n"); 
 					  $$.nd = mknode(NULL, $3.nd, "assignment"); 
 					  set_type($1.name, $3.type);
 					  
	sprintf(icg[ic_idx++], "%s = %s\n", $1.name, $3.name);
 					  }
;

init: ASSIGN expression   { printf("\t\tinit -> =expression\n"); 
			     $1.nd = mknode(NULL, NULL, $1.name);
			     strcpy($$.type, $2.type);
			     $$.nd = mknode($1.nd, $2.nd, "=");
			     strcpy($$.name, $2.name); }
			     
| ASSIGN scan	{ printf("\t\tinit -> =scan\n"); 
		  $1.nd = mknode(NULL, NULL, $1.name);
		  strcpy($$.type, "int");
		  $$.nd = mknode($1.nd, $2.nd, "=");
		  strcpy($$.name, $2.name); }
;

expression: expression ARITHMETIC_OPS expression	{ printf("\t\texpression -> expression ARITHMETIC_OPS expression\n");
    
			  $2.nd = mknode(NULL, NULL, $2.name);
			  if(!strcmp($1.type, $3.type)) {
				sprintf($$.type, $1.type);
				$$.nd = mknode($1.nd, $3.nd, $2.name); 
			  }
			  else {
				if(!strcmp($1.type, "int") && !strcmp($3.type, "float")) {
					struct node *temp = mknode(NULL, $1.nd, "inttofloat");
					sprintf($$.type, $3.type);
					$$.nd = mknode(temp, $3.nd, $2.name); 
					strcpy($$.type, "float");
			        }
			   	else if(!strcmp($1.type, "float") && !strcmp($3.type, "int")) {
					struct node *temp = mknode(NULL, $3.nd, "inttofloat");
					sprintf($$.type, $1.type);
					$$.nd = mknode($1.nd, temp, $2.name); 
					strcpy($$.type, "float");
			   	}
			   	
			   	else {
			   	    sprintf(sem_errors[sem_errors_i], "Line %d: Cannot add char* and number\n", lineno);
				    strcpy($$.type, "N/A");
		sem_errors_i++;
			   	}	
			  }
			  
			  
	sprintf($$.name, "t%d", temp_var);
	temp_var++;
	sprintf(icg[ic_idx++], "%s = %s %s %s\n",  $$.name, $1.name, $2.name, $3.name);
	
		}
| value	{ printf("\t\texpression -> value\n"); 
		  strcpy($$.name, $1.name); 
		  sprintf($$.type, $1.type);
		  $$.nd = $1.nd; }
;

value: CONSTANT { strcpy($$.name, $1.name); 
		  if(strchr($$.name, '.') != NULL)
		  	sprintf($$.type, "float");
		  else
		  	sprintf($$.type, "int");
		  add('C');
		  printf("\t\tvalue -> CONSTANT\n");
		  $1.nd = mknode(NULL, NULL, $1.name);
		  $$.nd = $1.nd;  }
| LITERAL	{ strcpy($$.name, $1.name);
		  sprintf($$.type, "char*");
		  add('C'); 
		  printf("\t\tvalue -> LITERAL\n");
		  $1.nd = mknode(NULL, NULL, $1.name);
		  $$.nd = $1.nd; }
| ID		{ check_declaration($1.name); 
		  strcpy($$.name, $1.name);
		  sprintf($$.type, get_type($1.name));
		  printf("\t\tvalue -> ID\n");
		  $1.nd = mknode(NULL, NULL, $1.name);
		  $$.nd = $1.nd; }
;

return: RETURN { add('K'); } expression	{ printf("\t\treturn -> RETURN expression\n"); 
						  $1.nd = mknode(NULL, NULL, $1.name);
						  $$.nd = mknode($1.nd, $3.nd, "return"); }
| 	{ printf("\t\treturn -> epsilon\n");
	  $$.nd = NULL; }
;

%%


int main() {
  printf("\t\tPARSING STEPS\n");
  printf("\t\t*************\n\n");
  printf("INPUT SYMBOL\t\tPRODUCTION RULE\n");
  printf("************\t\t***************\n");
  
  yyparse();
  
  printf("\n\n");
  printf("\n\n");
  printf("\t\t SYNTAX ANALYSIS \n");
  printf("\t\t ***************** \n\n");
	
  if(syn_errors_i>0) {
	printf("Syntax analysis completed with %d errors\n", syn_errors_i);
	for(int i=0; i<syn_errors_i; i++){
	    printf("\t - %s", syn_errors[i]);
	}
  } else {
	printf("Syntax analysis completed with no errors");
  }
  if(syn_errors_i == 0) { 
  printf("\n\n");
  printf("\n\n");
  printf("\t\t SEMANTIC ANALYSIS \n");
  printf("\t\t ***************** \n\n");
	
  if(sem_errors_i>0) {
	printf("Semantic analysis completed with %d errors\n", sem_errors_i);
	for(int i=0; i<sem_errors_i; i++){
	    printf("\t - %s", sem_errors[i]);
	}
  } else {
	printf("Semantic analysis completed with no errors");
  }
	
  if(sem_errors_i == 0) {
  
      printf("\n\n\n\n");
      printf("\t\tSYMBOL TABLE\n");
      printf("\t\t************\n\n");
      printf("\nSYMBOL  DATATYPE  TYPE  LINE NUMBER");
      printf("\n******  ********  ****  *********** \n\n");
      int i=0;
      for(i=0; i<count; i++) {
	 printf("%s\t%s\t%s\t%d\t\n", symbol_table[i].id_name, symbol_table[i].data_type, symbol_table[i].type, symbol_table[i].line_no);
      }
      for(i=0;i<count;i++) {
	 free(symbol_table[i].id_name);
	 free(symbol_table[i].type);
      }
  
      printf("\n\n");
      printf("\n\n");
      printf("\t\t SYNTAX TREE \n");
      printf("\t\t *********** \n\n");
      printtree(head);
      
      printf("\n\n");
	printf("\t\t  INTERMEDIATE CODE GENERATION \n");
	printf("\t\t  **************************** \n\n");
	for(int i=0; i<ic_idx; i++){
		printf("%s", icg[i]);
	}
	printf("\n\n");
  }
  }
  printf("\n\n");
  
}

int search(char *type) {
  int i;
  for(i=count-1; i>=0; i--) {
    if(strcmp(symbol_table[i].id_name, type)==0) {
	return -1;
	break;
    }
  }
  return 0;
}

void add(char c) {
  q=search(yylval.nd_obj.name);
  
  if(c == 'V') {  
    for(int i=0; i<10; i++) {   
        if(!strcmp(reserved[i], strdup(yylval.nd_obj.name))) {
            sprintf(sem_errors[sem_errors_i], "Line %d: Variable name  \"%s\" is a reserved keyword!\n", lineno+1, yylval.nd_obj.name);
            sem_errors_i++;    
            return;
        }  
    } 
  }


  if(!q) {
    if(c == 'H') {
	symbol_table[count].id_name=strdup(yylval.nd_obj.name);
	symbol_table[count].data_type=strdup("N/A");
	symbol_table[count].line_no=lineno;
	symbol_table[count].type=strdup("Header");
	count++;
    }
    else if(c == 'K') {
	symbol_table[count].id_name=strdup(yylval.nd_obj.name);
	symbol_table[count].data_type=strdup("N/A");
	symbol_table[count].line_no=lineno;
	symbol_table[count].type=strdup("Keyword\t");
	count++;
    }
    else if(c == 'V') {
	symbol_table[count].id_name=strdup(yylval.nd_obj.name);
	symbol_table[count].data_type=strdup("N/A");
	symbol_table[count].line_no=lineno;
	symbol_table[count].type=strdup("Variable");
	count++;
    }
    else if(c == 'C') {
	symbol_table[count].id_name=strdup(yylval.nd_obj.name);
	symbol_table[count].data_type=strdup("CONST");
	symbol_table[count].line_no=lineno;
	symbol_table[count].type=strdup("Constant");
	count++;
    }
    else if(c == 'F') {
	symbol_table[count].id_name=strdup(yylval.nd_obj.name);
	symbol_table[count].data_type=strdup("N/A");
	symbol_table[count].line_no=lineno;
	symbol_table[count].type=strdup("Function");
	count++;
    }
  } 
  
}

void check_declaration(char *c) {
    q = search(c);
    if(!q) {
        sprintf(sem_errors[sem_errors_i], "Line %d: Variable \"%s\" not declared before usage!\n", lineno, c);
        sem_errors_i++;
    }
}


char *get_type(char *var){
	for(int i=0; i<count; i++) {
		// Handle case of use before declaration
		if(!strcmp(symbol_table[i].id_name, var)) {
			return symbol_table[i].data_type;
		}
	}
	return "N/A";
}


void set_type(char *var, char *type){
	for(int i=0; i<count; i++) {
	    if(!strcmp(symbol_table[i].id_name, var)) {
		strcpy(symbol_table[i].data_type, type);
	    }
	}
}

struct node* mknode(struct node *left, struct node *right, char *token) {	
    struct node *newnode = (struct node *)malloc(sizeof(struct node));
    char *newstr = (char *)malloc(strlen(token)+1);
    strcpy(newstr, token);
    newnode->left = left;
    newnode->right = right;
    newnode->token = newstr;
    return(newnode);
}

void printtree(struct node* tree) {
    printf("\n\n Inorder traversal of the Parse Tree: \n\n");
    printInorder(tree);
    printf("\n\n\n");
    printf("Level Order traversal of the syntax tree: \n\n");
    printLevelOrder(tree);
}

void printInorder(struct node *tree) {
    int i;
    if (tree->left) {
	printInorder(tree->left);
    }
    printf("%s, ", tree->token);
    if (tree->right) {
	printInorder(tree->right);
    }
}

void printLevelOrder(struct node* root)
{
    int h = height(root);
    int i;
    for (i = 1; i <= h; i++){
        printf("Level %d:\t", i);
        printCurrentLevel(root, i);
        printf("\n");
    }
}
 
/* Print nodes at a current level */
void printCurrentLevel(struct node* root, int level)
{
    if (root == NULL)
        return;
    if (level == 1)
        printf("%s ", root->token);
    else if (level > 1) {
        printCurrentLevel(root->left, level - 1);
        printCurrentLevel(root->right, level - 1);
    }
}
 
int height(struct node* node)
{
    if (node == NULL)
        return 0;
    else {
        /* compute the height of each subtree */
        int lheight = height(node->left);
        int rheight = height(node->right);
 
        /* use the larger one */
        if (lheight > rheight) {
            return (lheight + 1);
        }
        else {
            return (rheight + 1);
        }
    }
}


void yyerror(const char* msg) {
    fprintf(stderr, "%s at line %d\n", msg, lineno);
    sprintf(syn_errors[syn_errors_i], "%s at line %d\n", msg, lineno);
    syn_errors_i++;
}
