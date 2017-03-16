%{/*************************************************************************
Compiler for the Simple language
***************************************************************************/
/*=========================================================================
C Libraries, Symbol Table, Code Generator & other C code
=========================================================================*/
#include <stdarg.h>
#include <stdio.h> /* For I/O */
#include <stdlib.h> /* For malloc here and in symbol table */
#include <string.h> /* For strcmp in symbol table */
#include "ST.h" /* Symbol Table */
#include "SM.h" /* Stack Machine */
#include "CG.h" /* Code Generator */
#define YYDEBUG 1 /* For Debugging */
int errors; /* Error Count */
#define TABSIZE 1000
extern char* var_names[TABSIZE];
extern int var_def[TABSIZE];
extern int n_of_names;
extern int installs(char *txt);
extern void reset();
 

char* var_val[TABSIZE];     // array where all the values are stored

char* ReverseString(char *str)
{
    static int i=0;
    static char rev[50];
 
    if(*str)
	{
         ReverseString(str+1);
         rev[i++] = *str;		
    }
 
    return rev;		// Return index of array every time
}

char* subString(char str[], int a, int b)
{ 
	if(0<a<b){
		int c=0, lim=b-a+1, d=a-1;
		char sub[lim];
		while(c<lim){
			sub[c]=str[d];
			c++;d++;
		}
		sub[c]='\0';
		return sub;
	}
    else return str;
}

char* conc(char *test,char *sec){
	char *num ;char *mun;
	num=strdup(test);
	mun=strdup(sec);
	return strcat(num,mun);
}
char* equal(char *test,char *sec){
	char *first ;char *second;
	first=strdup(test);
	second=strdup(sec);
	if(strcmp(first,second)==0){
		return "true";}
	else{
		return "false";}
}
char* rspc(char *text){
	int c=0,d=0; char* blank;
	/*while(text[c]!='\0'){
		if(text[c]==' '){
			int temp=c+1;
				while(text[temp]==' ' && text[temp]!='\0'){
					if(text[temp]==''){
						c++;
					}
					temp++;
				}
			}
			
		}
		blank[d]=text[c];
		c++;
		d++;
	}
	blank[d]='\0';*/
	return blank;
}

double avgN ( int num, ... )
{
    va_list arguments;                     
    double sum = 0;

    /* Initializing arguments to store all values after num */
    va_start ( arguments, num );           
    /* Sum all the inputs; we still rely on the function caller to tell us how
     * many there are */
	 int x;
    for ( x = 0; x < num; x++ )        
    {
        sum += va_arg ( arguments, double ); 
    }
    va_end ( arguments );                  // Cleans up the list

    return sum / num;                      
}
double sumN ( int num, ... )
{
    va_list arguments;                     
    double sum = 0;

    /* Initializing arguments to store all values after num */
    va_start ( arguments, num );           
    /* Sum all the inputs; we still rely on the function caller to tell us how
     * many there are */
	 int x;
    for ( x = 0; x < num; x++ )        
    {
        sum += va_arg ( arguments, double ); 
    }
    va_end ( arguments );                  // Cleans up the list

    return sum;                      
}

double mulN ( int num, ... )
{
    va_list arguments;                     
    double mul = 1;

    /* Initializing arguments to store all values after num */
    va_start ( arguments, num );           
    /* Sum all the inputs; we still rely on the function caller to tell us how
     * many there are */
	 int x;
    for ( x = 0; x < num; x++ )        
    {
        mul *= va_arg ( arguments, double ); 
    }
    va_end ( arguments );                  // Cleans up the list

    return mul;                      
}

double maxN2(int count,double a,double b)
{
	double x[]= {a,b};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]>t)
			t=x[i];
	}
	return(t);

 }   

double maxN3(int count,double a,double b,double c)
{
	double x[]= {a,b,c};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]>t)
			t=x[i];
	}
	return(t);

 }   
 
double maxN4(int count,double a,double b,double c,double d)
{
	double x[]= {a,b,c,d};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]>t)
			t=x[i];
	}
	return(t);

 }   
  
double maxN5(int count,double a,double b,double c,double d,double e)
{
	double x[]= {a,b,c,d,e};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]>t)
			t=x[i];
	}
	return(t);

 }   

double maxN6(int count,double a,double b,double c,double d,double e,double f)
{
	double x[]= {a,b,c,d,e,f};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]>t)
			t=x[i];
	}
	return(t);

 }

double maxN7(int count,double a,double b,double c,double d,double e,double f,double g)
{
	double x[]= {a,b,c,d,e,f,g};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]>t)
			t=x[i];
	}
	return(t);

 }   

double maxN8(int count,double a,double b,double c,double d,double e,double f,double g,double h)
{
	double x[]= {a,b,c,d,e,f,g,h};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]>t)
			t=x[i];
	}
	return(t);

 }   

double maxN9(int count,double a,double b,double c,double d,double e,double f,double g,double h,double j)
{
	double x[]= {a,b,c,d,e,f,g,h,j};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]>t)
			t=x[i];
	}
	return(t);

 }   
 
 double maxN10(int count,double a,double b,double c,double d,double e,double f,double g,double h,double j,double k)
{
	double x[]= {a,b,c,d,e,f,g,h,j,k};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]>t)
			t=x[i];
	}
	return(t);

 }

 double maxN11(int count,double a,double b,double c,double d,double e,double f,double g,double h,double j,double k,double l)
{
	double x[]= {a,b,c,d,e,f,g,h,j,k,l};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]>t)
			t=x[i];
	}
	return(t);

 }

double maxN12(int count,double a,double b,double c,double d,double e,double f,double g,double h,double j,double k,double l,double m)
{
	double x[]= {a,b,c,d,e,f,g,h,j,k,l,m};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]>t)
			t=x[i];
	}
	return(t);

 }


double minN2(int count,double a,double b)
{
	double x[]= {a,b};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]<t)
			t=x[i];
	}
	return(t);

 }   

double minN3(int count,double a,double b,double c)
{
	double x[]= {a,b,c};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]<t)
			t=x[i];
	}
	return(t);

 }   
 
double minN4(int count,double a,double b,double c,double d)
{
	double x[]= {a,b,c,d};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]<t)
			t=x[i];
	}
	return(t);

 }   
  
double minN5(int count,double a,double b,double c,double d,double e)
{
	double x[]= {a,b,c,d,e};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]<t)
			t=x[i];
	}
	return(t);

 }   

double minN6(int count,double a,double b,double c,double d,double e,double f)
{
	double x[]= {a,b,c,d,e,f};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]<t)
			t=x[i];
	}
	return(t);

 }

double minN7(int count,double a,double b,double c,double d,double e,double f,double g)
{
	double x[]= {a,b,c,d,e,f,g};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]<t)
			t=x[i];
	}
	return(t);

 }   

double minN8(int count,double a,double b,double c,double d,double e,double f,double g,double h)
{
	double x[]= {a,b,c,d,e,f,g,h};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]<t)
			t=x[i];
	}
	return(t);

 }   

double minN9(int count,double a,double b,double c,double d,double e,double f,double g,double h,double j)
{
	double x[]= {a,b,c,d,e,f,g,h,j};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]<t)
			t=x[i];
	}
	return(t);

 }   
 
double minN10(int count,double a,double b,double c,double d,double e,double f,double g,double h,double j,double k)
{
	double x[]= {a,b,c,d,e,f,g,h,j,k};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]<t)
			t=x[i];
	}
	return(t);

 }
 
double minN11(int count,double a,double b,double c,double d,double e,double f,double g,double h,double j,double k,double l)
{
	double x[]= {a,b,c,d,e,f,g,h,j,k,l};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]<t)
			t=x[i];
	}
	return(t);

 }

double minN12(int count,double a,double b,double c,double d,double e,double f,double g,double h,double j,double k,double l,double m)
{
	double x[]= {a,b,c,d,e,f,g,h,j,k,l,m};
	double t;
	int i;
	t=x[0];
	for(i=1;i<count;i++)
        {
		if(x[i]<t)
			t=x[i];
	}
	return(t);

 }

 
 
/*-------------------------------------------------------------------------
The following support backpatching
-------------------------------------------------------------------------*/
struct lbs /* Labels for data, if and while */
{
int for_goto;
int for_jmp_false;

};
struct lbs * newlblrec() /* Allocate space for the labels */
{
return (struct lbs *) malloc(sizeof(struct lbs));
}
/*-------------------------------------------------------------------------
Install identifier & check if previously defined.
-------------------------------------------------------------------------*/
install ( char *sym_name )
{
symrec *s;
s = getsym (sym_name);
if (s == 0)
s = putsym (sym_name);
else { errors++;
printf( "%s is already defined\n", sym_name );
}
}
/*-------------------------------------------------------------------------
If identifier is defined, generate code
-------------------------------------------------------------------------*/
context_check( enum code_ops operation, char *sym_name )
{ symrec *identifier;
identifier = getsym( sym_name );
if ( identifier == 0 )
{ errors++;
printf( "%s", sym_name );
printf( "%s\n", " is an undeclared identifier" );
}
else gen_code( operation, identifier->offset );
}
/*=========================================================================
SEMANTIC RECORDS
=========================================================================*/
%}
%union semrec /* The Semantic Records */
{
int intval; /* Integer values */
double num;
char *id; /* Identifiers */
struct lbs *lbls; /* For backpatching */
char* comms;
int index;
}
/*=========================================================================
TOKENS
=========================================================================*/
%start program
%token <index> VAR
%token <intval> NUMBER /* Simple integer */
%token <id> IDENTIFIER /* Simple identifier */
%token <lbls> IF WHILE FOR /* For backpatching labels */
%token <num> DOUBLENUM mathSqrt mathCeil mathMod mathFloor mathFabs mathExp mathLog10 mathLog mathSin mathTan mathAsin mathAcos mathAtan mathSinh mathCosh mathTanh mathCos
%token SKIP THEN ELSE ENDIF ENDFOR ENDWHILE DO END MODU
%token READ WRITE FULPROG FULVAR 
%token PROGNAME AVERAGENUM
%token ASSGNOP MAXNUM MINNUM MULNUM SUMNUM
%token <comms> STMT concat remSpace toUpper toLower revstr isEqual substr copystr stringLeng
%type <comms> com command
%type <num> mathfunctn primary inBuiltFunc
/*=========================================================================
OPERATOR PRECEDENCE
=========================================================================*/
%left AND OR  LSHIFT RSHIFT  
%left LTH GTH LTHE GTHE NEQU EQU 
%left  BITANDD BITORR BITXORR COMPL
%left '-' '+'
%left '*' '/' '%'
%right '^' '~'
%right INCRE DECRE SHADD SHSUB SHMUL SHDIV SHBITXOR SHMULD SHRSHIFT SHLSHIFT SHBITOR SHBITAND
/*=========================================================================
GRAMMAR RULES for the Simple language
=========================================================================*/
%%
program : FULPROG
declarations
FULVAR { gen_code( DATA, data_location() - 1 ); }
commands
END { gen_code( HALT, 0 ); YYACCEPT; }
;
declarations : /* empty */
| id_seq IDENTIFIER ';' { install( $2 ); }
;
id_seq : /* empty */
| id_seq IDENTIFIER ',' { install( $2 ); }
;
commands : /* empty */
| commands command ';'

;
command : SKIP
| VAR ASSGNOP com    { $$ = var_val[$1] = $3; var_def[$1] = 1; }
| WRITE com {printf("%s", $2);}
| WRITE mathfunctn {printf("%f",$2);}
| WRITE inBuiltFunc {printf("%.2f",$2);}
| IDENTIFIER ASSGNOP exp { context_check( STORE, $1 ); }
| WRITE exp { gen_code( WRITE_INT, 0 ); }
| READ IDENTIFIER { context_check( READ_INT, $2 ); }
| IF exp { $1 = (struct lbs *) newlblrec();
$1->for_jmp_false = reserve_loc(); }
THEN commands { $1->for_goto = reserve_loc(); }
ELSE { back_patch( $1->for_jmp_false,
JMP_FALSE,
gen_label() ); }
commands
ENDIF { back_patch( $1->for_goto, GOTO, gen_label() ); }
| WHILE { $1 = (struct lbs *) newlblrec();
$1->for_goto = gen_label(); }

exp { $1->for_jmp_false = reserve_loc(); }
DO
commands
ENDWHILE { gen_code( GOTO, $1->for_goto );
back_patch( $1->for_jmp_false,
JMP_FALSE,
gen_label() ); }
| FOR { $1 = (struct lbs *) newlblrec();
$1->for_goto = gen_label(); }

exp { $1->for_jmp_false = reserve_loc(); }
DO
commands
ENDFOR { gen_code( GOTO, $1->for_goto );
back_patch( $1->for_jmp_false,
JMP_FALSE,
gen_label() ); }
;
exp : 
IDENTIFIER { context_check( LD_VAR, $1 ); }
| NUMBER { gen_code( LD_INT, $1 );}
| exp LTH exp { gen_code( LT, 0 ); }
| exp LTHE exp {gen_code(LTE, 0);}
| exp GTHE exp {gen_code(GTE, 0);}
| exp EQU exp { gen_code( EQ, 0 ); }
| exp GTH exp { gen_code( GT, 0 ); }
| exp NEQU exp { gen_code( NEQ, 0 ); }
| exp '+' exp { gen_code( ADD, 0 ); }
| exp '-' exp { gen_code( SUB, 0 ); }
| exp '*' exp { gen_code( MULT, 0 ); }
| exp '/' exp { gen_code( DIV, 0 ); }
| exp '^' exp { gen_code( PWR, 0 ); }
| exp '%' exp {gen_code(MODUL,0);}
| COMPL exp { gen_code( COMP, 0 ); }
| '-' exp { gen_code( NEG, 0 ); }
| exp BITANDD exp {gen_code(BITAND,0);}
| exp BITORR exp {gen_code(BITOR,0);}
| exp BITXORR exp {gen_code(BITXOR,0);}
| exp SHADD exp {gen_code(SHORTADD,0);}
| exp SHSUB exp {gen_code(SHORTSUB,0);}
| exp SHMUL exp {gen_code(SHORTMULT,0);}
| exp SHDIV exp {gen_code(SHORTDIV,0);}
| exp SHMULD exp {gen_code(SHORTMOD,0);}
| exp LSHIFT exp {gen_code(BITLSH,0);}
| exp RSHIFT exp {gen_code(BITRSH,0);}
| exp SHLSHIFT exp {gen_code(SHORTBITLSH,0);}
| exp SHRSHIFT exp {gen_code(SHORTBITRSH,0);}
| exp SHBITXOR exp {gen_code(SHORTBITXOR,0);}
| exp SHBITAND exp {gen_code(SHORTBITAND,0);}
| exp SHBITOR exp {gen_code(SHORTBITOR,0);}
| exp OR exp {gen_code(ORTEST,0);}
| exp AND exp {gen_code(ANDTEST,0);}
| exp INCRE {gen_code(POINCR,0);}
| exp DECRE {gen_code(PODECR,0);}
| '('exp')'
| mathfunctn  { gen_code( LD_INT, $1 );}
| inBuiltFunc  { gen_code( LD_INT, $1 );}
;
com : STMT		{$$ = $1;}
|   VAR        { if(!var_def[$1]) printf("%s","undefined string variable"); else var_val[$1]; }
|	concat'('com ',' com')'		{ $$ = conc($3,$5); }
|	remSpace '(' com ')'	{$$=rspc($3);}
|	toLower '(' com ')'		{$$=strlwr($3);}
|	toUpper '(' com ')'		{$$=strupr($3);}
|	isEqual '(' com ',' com ')' {$$=equal($3,$5);}
|	substr '(' com ','NUMBER ',' NUMBER ')'	{$$=subString($3,$5,$7);}
|	revstr '(' com ')' {$$=ReverseString($3);}

;
mathfunctn : primary                            { $$ = $1; }
|	mathSqrt'('mathfunctn')'						{$$ = pow($3,0.5);}
|	mathCeil '('mathfunctn')'					{$$ = ceil($3);}
|	mathFloor '('mathfunctn')'					{$$ = floor($3);}
|   mathFabs'('mathfunctn')'					{$$ = fabs($3);}
|	mathExp'('mathfunctn')'						{$$ = exp($3);}
|	mathLog10'('mathfunctn')'					{$$ = log10($3);}
|	mathLog'('mathfunctn')'						{$$ = log($3);}
|	mathMod '('mathfunctn ','mathfunctn')'	{$$ = fmod($3,$5);}
|	mathSin'('mathfunctn')'						{$$ = sin($3 * (3.141592654/180));}
|	mathTan'('mathfunctn')'						{$$ = tan($3 * (3.141592654/180));}
|	mathCos'('mathfunctn')'						{$$ = cos($3 * (3.141592654/180));}
|	mathAsin'('mathfunctn')'						{$$ = (asin($3)* (180/3.141592654));}
|	mathAcos'('mathfunctn')'						{$$ = (acos($3) * (180/3.141592654));}
|	mathAtan'('mathfunctn')'						{$$ = (atan($3) * (180/3.141592654));}
|	mathSinh'('mathfunctn')'						{$$ = sinh($3 * (3.141592654/180));}
|	mathCosh'('mathfunctn')'						{$$ = cosh($3 * (3.141592654/180));}
|	mathTanh'('mathfunctn')'						{$$ = tanh($3 * (3.141592654/180));}
|	stringLeng'('com')'						{$$ = strlen($3);}
;
inBuiltFunc: primary	{$$ = $1;}
| AVERAGENUM'('inBuiltFunc','inBuiltFunc')'	{$$ = avgN(2,$3,$5);}
| AVERAGENUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = avgN(3,$3,$5,$7);}
| AVERAGENUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = avgN(4,$3,$5,$7,$9);}
| AVERAGENUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = avgN(5,$3,$5,$7,$9,$11);}
| AVERAGENUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = avgN(6,$3,$5,$7,$9,$11,$13);}
| AVERAGENUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = avgN(7,$3,$5,$7,$9,$11,$13,$15);}
| AVERAGENUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = avgN(8,$3,$5,$7,$9,$11,$13,$15,$17);}
| AVERAGENUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = avgN(9,$3,$5,$7,$9,$11,$13,$15,$17,$19);}
| AVERAGENUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = avgN(10,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21);}
| AVERAGENUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = avgN(11,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21,$23);}
| AVERAGENUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = avgN(12,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21,$23,$25);}
| SUMNUM'('inBuiltFunc','inBuiltFunc')'	{$$ = sumN(2,$3,$5);}
| SUMNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = sumN(3,$3,$5,$7);}
| SUMNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = sumN(4,$3,$5,$7,$9);}
| SUMNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = sumN(5,$3,$5,$7,$9,$11);}
| SUMNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = sumN(6,$3,$5,$7,$9,$11,$13);}
| SUMNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = sumN(7,$3,$5,$7,$9,$11,$13,$15);}
| SUMNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = sumN(8,$3,$5,$7,$9,$11,$13,$15,$17);}
| SUMNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = sumN(9,$3,$5,$7,$9,$11,$13,$15,$17,$19);}
| SUMNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = sumN(10,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21);}
| SUMNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = sumN(11,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21,$23);}
| SUMNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = sumN(12,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21,$23,$25);}
| MULNUM'('inBuiltFunc','inBuiltFunc')'	{$$ = mulN(2,$3,$5);}
| MULNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = mulN(3,$3,$5,$7);}
| MULNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = mulN(4,$3,$5,$7,$9);}
| MULNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = mulN(5,$3,$5,$7,$9,$11);}
| MULNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = mulN(6,$3,$5,$7,$9,$11,$13);}
| MULNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = mulN(7,$3,$5,$7,$9,$11,$13,$15);}
| MULNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = mulN(8,$3,$5,$7,$9,$11,$13,$15,$17);}
| MULNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = mulN(9,$3,$5,$7,$9,$11,$13,$15,$17,$19);}
| MULNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = mulN(10,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21);}
| MULNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = mulN(11,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21,$23);}
| MULNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = mulN(12,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21,$23,$25);}
| MAXNUM'('inBuiltFunc','inBuiltFunc')'	{$$ = maxN2(2,$3,$5);}
| MAXNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = maxN3(3,$3,$5,$7);}
| MAXNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = maxN4(4,$3,$5,$7,$9);}
| MAXNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = maxN5(5,$3,$5,$7,$9,$11);}
| MAXNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = maxN6(6,$3,$5,$7,$9,$11,$13);}
| MAXNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = maxN7(7,$3,$5,$7,$9,$11,$13,$15);}
| MAXNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = maxN8(8,$3,$5,$7,$9,$11,$13,$15,$17);}
| MAXNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = maxN9(9,$3,$5,$7,$9,$11,$13,$15,$17,$19);}
| MAXNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = maxN10(10,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21);}
| MAXNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = maxN11(11,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21,$23);}
| MAXNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = maxN12(12,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21,$23,$25);}
| MINNUM'('inBuiltFunc','inBuiltFunc')'	{$$ = minN2(2,$3,$5);}
| MINNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = minN3(3,$3,$5,$7);}
| MINNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = minN4(4,$3,$5,$7,$9);}
| MINNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = minN5(5,$3,$5,$7,$9,$11);}
| MINNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = minN6(6,$3,$5,$7,$9,$11,$13);}
| MINNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = minN7(7,$3,$5,$7,$9,$11,$13,$15);}
| MINNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = minN8(8,$3,$5,$7,$9,$11,$13,$15,$17);}
| MINNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = minN9(9,$3,$5,$7,$9,$11,$13,$15,$17,$19);}
| MINNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = minN10(10,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21);}
| MINNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = minN11(11,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21,$23);}
| MINNUM'('inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc','inBuiltFunc')'	{$$ = minN12(12,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21,$23,$25);}


;
primary : NUMBER                         { $$ = $1; }
|	DOUBLENUM                            { $$ = $1; }
;

%%
/*=========================================================================
MAIN
=========================================================================*/
main( int argc, char *argv[] )
{ extern FILE *yyin;
++argv; --argc;
yyin = fopen( argv[0], "r" );
/*yydebug = 1;*/
errors = 0;
yyparse ();
printf ( "Parse Completed\n" );
if ( errors == 0 )
{ print_code ();
fetch_execute_cycle();
}
}
/*=========================================================================
YYERROR
=========================================================================*/
yyerror ( char *s ) /* Called by yyparse on error */
{
errors++;
printf ("%s\n", s);
}
/**************************** End Grammar File ***************************/
