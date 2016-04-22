/* File: parser.y
 * --------------
 * Bison input file to generate the parser for the compiler.
 *
 * pp2: your job is to write a parser that will construct the parse tree
 *      and if no parse errors were found, print it.  The parser should
 *      accept the language as described in specification, and as augmented
 *      in the pp2 handout.
 */

%{

/* Just like lex, the text within this first region delimited by %{ and %}
 * is assumed to be C/C++ code and will be copied verbatim to the y.tab.c
 * file ahead of the definitions of the yyparse() function. Add other header
 * file inclusions or C++ variable declarations/prototypes that are needed
 * by your code here.
 */
#include "scanner.h" // for yylex
#include "parser.h"
#include "errors.h"

void yyerror(const char *msg); // standard error-handling routine

%}

/* The section before the first %% is the Definitions section of the yacc
 * input file. Here is where you declare tokens and types, add precedence
 * and associativity options, and so on.
 */

/* yylval
 * ------
 * Here we define the type of the yylval global variable that is used by
 * the scanner to store attibute information about the token just scanned
 * and thus communicate that information to the parser.
 *
 * pp2: You will need to add new fields to this union as you add different
 *      attributes to your non-terminal symbols.
 */
%union {
    int integerConstant;
    bool boolConstant;
    float floatConstant;
    char identifier[MaxIdentLen+1]; // +1 for terminating null
    FnDecl *fnDecl;
    Decl *decl;
    List<Decl*> *declList;
    Expr *expr;
    //List<Expr*> *exprList;
    Type *type;
    TypeQualifier *tq;
    Operator *o;
    List<VarDecl*> *d;
    VarDecl *var;
    Stmt *b;
    List<Stmt*> *s;
    ArrayType *arr;
}


/* Tokens
 * ------
 * Here we tell yacc about all the token types that we are using.
 * Bison will assign unique numbers to these and export the #define
 * in the generated y.tab.h header file.
 */
%token   T_Void T_Bool T_Int T_Float
%token   T_LessEqual T_GreaterEqual T_EQ T_NE T_LeftAngle T_RightAngle
%token   T_And T_Or
%token   T_Equal T_MulAssign T_DivAssign T_AddAssign T_SubAssign
%token   T_While T_For T_If T_Else T_Return T_Break
%token   T_Const T_Uniform T_Layout T_Continue T_Do
%token   T_Inc T_Dec T_Switch T_Case T_Default
%token   T_In T_Out T_InOut
%token   T_Mat2 T_Mat3 T_Mat4 T_Vec2 T_Vec3 T_Vec4
%token   T_Ivec2 T_Ivec3 T_Ivec4 T_Bvec2 T_Bvec3 T_Bvec4
%token   T_Uint T_Uvec2 T_Uvec3 T_Uvec4 T_Struct
%token   T_Semicolon T_Dot T_Colon T_Question T_Comma
%token   T_Dash T_Plus T_Star T_Slash
%token   T_LeftParen T_RightParen T_LeftBracket T_RightBracket T_LeftBrace T_RightBrace

%token   <identifier> T_Identifier
%token   <integerConstant> T_IntConstant
%token   <floatConstant> T_FloatConstant
%token   <boolConstant> T_BoolConstant

/* Non-terminal types
 * ------------------
 * In order for yacc to assign/access the correct field of $$, $1, we
 * must to declare which field is appropriate for the non-terminal.
 * As an example, this first type declaration establishes that the DeclList
 * non-terminal uses the field named "declList" in the yylval union. This
 * means that when we are setting $$ for a reduction for DeclList ore reading
 * $n which corresponds to a DeclList nonterminal we are accessing the field
 * of the union named "declList" which is of type List<Decl*>.
 * pp2: You'll need to add many of these of your own.
 */
%type <declList>  DeclList
%type <decl>      Decl
%type <fnDecl>    FunctionPrototype FunctionHeader /*FunctionCall*/
/* %type <exprList>  ExprList*/
%type <expr>      Expr BoolExpr AssignExpr PostfixExpr ForInit 
%type <o>         ArithOp RelationalOp EqualityOp LogicalOp AssignOp Postfix
%type <type>      Type
%type <tq>        TypeQualifier
%type <d>         VarDeclList ParamList
%type <var>       VarDecl Param
%type <s>         StmtList
%type <b>         Stmt CompoundStmt SimpleStmt ConditionalStmt Condition LoopStmt SelectionStmt
%type <arr>       ArrayType






%%
/* Rules
 * -----
 * All productions and actions should be placed between the start and stop
 * %% markers which delimit the Rules section.

 */
Program   :    DeclList            {
                                      @1;
                                      /* pp2: The @1 is needed to convince
                                       * yacc to set up yylloc. You can remove
                                       * it once you have other uses of @n*/
                                      Program *program = new Program($1);
                                      // if no errors, advance to next phase
                                      if (ReportError::NumErrors() == 0)
                                          program->Print(0);
                                    }
          ;

DeclList  :    DeclList Decl        { ($$=$1)->Append($2); }
          |    Decl                 { ($$ = new List<Decl*>)->Append($1); }
          ;

Decl      :  FunctionPrototype      { ($$=$1); }
          |  FunctionPrototype T_Semicolon { $$=$1; }
          |  VarDecl T_Semicolon { $$=$1; }
          ;

FunctionPrototype : FunctionHeader  { $$=$1; }
                  | FunctionHeader Stmt { 
                                            $$=$1;
                                            $$->SetFunctionBody($2);
                                        }
                  ;
                  
                  
                  
                  
FunctionHeader :  Type T_Identifier T_LeftParen T_RightParen {
                                          Identifier *id = new Identifier(@2,$2);
                                          Type *t = $1;
                                          $$ = new FnDecl(id,t,new List<VarDecl*>);
                                        }
               |  TypeQualifier Type T_Identifier T_LeftParen T_RightParen {
                                          Identifier *id = new Identifier(@3,$3);
                                          Type *t = $2;
                                          TypeQualifier *tq = $1;
                                          $$ = new FnDecl(id,t,tq,new List<VarDecl*>);
                                        }

               |  Type T_Identifier T_LeftParen ParamList T_RightParen {
                                          Identifier *id = new Identifier(@2,$2);
                                          Type *t = $1;
                                          List<VarDecl*> *params = $4;
                                          $$ = new FnDecl(id,t,params);
                                        }
               |  TypeQualifier Type T_Identifier T_LeftParen ParamList T_RightParen {
                                          Identifier *id = new Identifier(@3,$3);
                                          Type *t = $2;
                                          TypeQualifier *tq = $1;
                                          List<VarDecl*> *params = $5;
                                          $$ = new FnDecl(id,t,tq,params);
                                        }
               ;

ParamList   :   ParamList T_Comma Param { ($$=$1)->Append($3); }
            |   Param { ($$ = new List<VarDecl*>)->Append($1); }
            ;

Param  :   VarDecl  { ($$=$1); }
       ;

VarDeclList :   VarDeclList VarDecl     { ($$=$1)->Append($2); }
            |   VarDecl                  { ($$ = new List<VarDecl*>)->Append($1); }
            ;

VarDecl   :    Type T_Identifier {
                                     Identifier *id = new Identifier(@2, $2);
                                     Type *t = $1;
                                     $$ = new VarDecl(id, t);
                                 }
          |    TypeQualifier Type T_Identifier { 
                                                    Identifier *id = new Identifier(@3, $3);
                                                    Type *t = $2;
                                                    TypeQualifier *tq = $1;
                                                    $$ = new VarDecl(id,t,tq); 
                                              }
          |    Type T_Identifier T_Equal AssignExpr {
                                                 Identifier *id = new Identifier(@2,$2);
                                                 Type *t = $1;
                                                 Expr *expr = $4;
                                                 $$ = new VarDecl(id,t,expr);
                                              }
          |    TypeQualifier Type T_Identifier T_Equal AssignExpr {
                                                 Identifier *id = new Identifier(@3,$3);
                                                 Type *t = $2;
                                                 TypeQualifier *tq = $1;
                                                 Expr *expr = $5;
                                                 $$ = new VarDecl(id,t,tq,expr);
                                              }
          |   Type T_Identifier ArrayType     {
                                                 Identifier *id = new Identifier(@2,$2);
                                                 Type *t = new ArrayType(@3,$1);
                                                 $$ = new VarDecl(id,t);
                                              }
          |   TypeQualifier Type T_Identifier ArrayType {
                                                       Identifier *id = new Identifier(@3,$3);
                                                       Type *t = new ArrayType(@3,$2);
                                                       TypeQualifier *tq = $1;
                                                       $$ = new VarDecl(id,t);
                                                    }
          |   /*empty*/ { }   
          ;

ArrayType     :    T_LeftBracket T_RightBracket {}
              |    T_LeftBracket T_IntConstant T_RightBracket {}
              ;

Type      :    T_Int { $$ = new Type("int");}
          |    T_Float { $$ = new Type("float");}
          |    T_Bool  { $$ = new Type("bool");}
          |    T_Void  { $$ = new Type("void"); }
          |    T_Mat2 {$$ = new Type("mat2"); }
          |    T_Mat3 {$$ = new Type("mat3"); }
          |    T_Mat4 {$$ = new Type("mat4"); }
          |    T_Vec2 {$$ = new Type("vec2"); }
          |    T_Vec3 {$$ = new Type("vec3"); }
          |    T_Vec4 {$$ = new Type("vec4"); }
          |    T_Ivec2 {$$ = new Type("ivec2"); }
          |    T_Ivec3 {$$ = new Type("ivec3"); }
          |    T_Ivec4 {$$ = new Type("ivec4"); }
          |    T_Bvec2 {$$ = new Type("bvec2"); }
          |    T_Bvec3 {$$ = new Type("bvec3"); }
          |    T_Bvec4 {$$ = new Type("bvec4"); }
          |    T_Uint  {$$ = new Type("uint"); }
          |    T_Uvec2 {$$ = new Type("uvec2"); }
          |    T_Uvec3 {$$ = new Type("uvec3"); }
          |    T_Uvec4 {$$ = new Type("uvec4"); }
          ;

TypeQualifier    :    T_In  { $$ = new TypeQualifier("in"); }
                 |    T_Out { $$ = new TypeQualifier("out");}
                 |    T_Const { $$ = new TypeQualifier("const");}
                 |    T_Uniform { $$ = new TypeQualifier("uniform");}
                 ;

StmtList  :   StmtList Stmt         { ($$=$1)->Append($2); }
          |   Stmt                  { ($$ = new List<Stmt*>)->Append($1); }
          ;

Stmt      :   CompoundStmt  { ($$=$1); }
          |   SimpleStmt    { ($$=$1); }
          ;



CompoundStmt  : T_LeftBrace T_RightBrace { $$ = new StmtBlock(new List<VarDecl*>,new List<Stmt*>); }
              | T_LeftBrace VarDeclList StmtList T_RightBrace {  
                                            List<VarDecl*> *vd = $2;
                                            List<Stmt*> *s = $3;
                                            $$ = new StmtBlock(vd,s);
                                        }
              ;

SimpleStmt   :  ConditionalStmt { ($$=$1); }
             |  T_Break   { $$ = new BreakStmt(@1); }
	         |  T_Return Expr { $$ = new ReturnStmt(@2,$2);}
	         |  Expr {$$=$1;}
              /*variable declerations and functions etc*/
          /*|   T_While T_LeftParen Stmt T_RightParen Stmt {
                                              Expr *expr = $3;
                                              Stmt *sub = $5;
                                              $$ = new WhileStmt(expr,sub);
                                            }*/
            ;

ConditionalStmt :  Condition  { ($$=$1); }
                |  Condition T_Semicolon { ($$=$1); }
		        |  T_Return  {}
		        |  LoopStmt {}
                ;

LoopStmt        : T_While T_LeftParen BoolExpr T_RightParen Stmt {
                                                                  $$=new WhileStmt($3, $5);
                                                                 }
                | T_Do Stmt T_While BoolExpr {$$=new DoWhileStmt($2, $4);}
		        | T_For T_LeftParen ForInit T_Semicolon BoolExpr T_Semicolon
		Expr T_RightParen Stmt {$$=new ForStmt($3, $5, $7, $9); }
		       | T_For T_LeftParen ForInit T_Semicolon BoolExpr T_Semicolon
		T_RightParen Stmt                   {
                                              $$=new ForStmt($3, $5, new EmptyExpr(), $8);
                                            }
		       | T_For T_LeftParen T_Semicolon BoolExpr T_RightParen Stmt {
                                              $$=new ForStmt(new EmptyExpr(), $4, new EmptyExpr(), $6);
                                            }
        ;


ForInit     :   
            ;

Condition  :  T_If T_LeftParen BoolExpr T_RightParen Stmt {
                                                             Expr *expr = $3;
                                                             Stmt *ifbody = $5;
                                                             $$ = new IfStmt(expr,ifbody, NULL);
                                                          }
           |  T_If T_LeftParen BoolExpr T_RightParen Stmt T_Else Stmt {
                                                                         Expr *expr = $3;
                                                                         Stmt *ifbody = $5;
                                                                         Stmt *elsebody = $7;
                                                                         $$ = new IfStmt(expr,ifbody,elsebody);
                                                                      }
           | SelectionStmt { ($$=$1); }
           ;

SelectionStmt  :  T_If T_LeftParen BoolExpr T_RightParen T_Question Stmt T_Colon Stmt {
                                                                                        $$ = new IfStmt($3,$6,$8);
                                                                                      }
               ;
                     

/*ExprList  :   ExprList Expr         { ($$=$1)->Append($2); }
          |   Expr                  { ($$ = new List<Expr*>)->Append($1); }
          ;*/

Expr      :  T_LeftParen Expr T_RightParen  { ($$=$2); }
          |  BoolExpr { ($$=$1); }
          |  AssignExpr { ($$=$1); }
          |  PostfixExpr { ($$=$1); }
          |  Expr ArithOp Expr {
                                  Expr *left = $1;
                                  Operator *op = $2;
                                  Expr *right = $3;
                                  $$ = new ArithmeticExpr(left,op,right);
                                }
          |  T_IntConstant   { 
                               $$ = new IntConstant(@1,$1);
                             }
          |  T_FloatConstant { 
                               $$ = new FloatConstant(@1,$1);
                             }
         /* | Decl { $$ = $1; }*/
          ;

PostfixExpr  :  Expr { ($$=$1); }
             |  PostfixExpr T_Dot T_Identifier { 
                                                 Identifier *id = new Identifier(@3,$3);
                                                 $$ = new FieldAccess($1,id); }
             |  Expr Postfix    {
                                   Expr *expr = $1;
                                   Operator *op = $2;
                                   $$ = new PostfixExpr(expr,op);
                                }
             ;

BoolExpr  : Expr RelationalOp Expr {
                                   Expr *left = $1;
                                   Operator *op = $2;
                                   Expr *right = $3;
                                   $$ = new RelationalExpr(left,op,right);
                                }
          |  Expr EqualityOp Expr {
                                   Expr *left = $1;
                                   Operator *op = $2;
                                   Expr *right = $3;
                                   $$ = new EqualityExpr(left,op,right);
                                }
          |  Expr LogicalOp Expr {
                                   Expr *left = $1;
                                   Operator *op = $2;
                                   Expr *right = $3;
                                   $$ = new LogicalExpr(left,op,right);
                                }
          |  T_BoolConstant     { 
                                   $$ = new BoolConstant(@1, $1);
                                }

           ;

AssignExpr : /*primary*/ Expr   { ($$=$1); }
           | Expr AssignOp Expr {
                                   Expr *left = $1;
                                   Operator *op = $2;
                                   Expr *right = $3;
                                   $$ = new AssignExpr(left,op,right);
                                }
           ;
          
ArithOp   :   T_Plus         {  
                               $$ = new Operator(@1,"+");
                             }
          |   T_Dash         { 
                                $$ = new Operator(@1,"-");
                             }
          |   T_Star         {
                                $$ = new Operator(@1,"*");
                             }
          |   T_Slash        {
                                $$ = new Operator(@1,"/");
                             }
          ;

RelationalOp :  T_LeftAngle     {
                                   $$ = new Operator(@1,"<");
                                }
             |   T_RightAngle   {
                                   $$ = new Operator(@1,">");
                                }
             |   T_LessEqual    {
                                   $$ = new Operator(@1,"<=");
                                }
	         |   T_GreaterEqual {
                                   $$ = new Operator(@1,">=");
                                }
             ;

EqualityOp   :   T_EQ           {
                                   $$ = new Operator(@1,"==");
                                }
	         |   T_NE           {
                                   $$ = new Operator(@1,"!=");
                                }
             ;   

LogicalOp    :   T_And          {
                                   $$ = new Operator(@1,"&&");
                                }
             |   T_Or           {
                                   $$ = new Operator(@1,"||");
                                }
             ;

AssignOp     :   T_Equal        {
                                   $$ = new Operator(@1,"=");
                                }
             |   T_AddAssign    {
                                   $$ = new Operator(@1,"+=");
                                }
             |   T_SubAssign    {
                                   $$ = new Operator(@1,"-=");
                                }
             |   T_MulAssign    {
                                   $$ = new Operator(@1,"*=");
                                }
             |   T_DivAssign    {
                                   $$ = new Operator(@1,"/=");
                                }
             ;

Postfix      :   T_Inc          {
                                  $$ = new Operator(@1,"++");
                                }
             |   T_Dec          {
                                  $$ = new Operator(@1,"--");
                                }
             ;


%%

/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated y.tab.c file.
 * This section is where you put definitions of helper functions.
 */

/* Function: InitParser
 * --------------------
 * This function will be called before any calls to yyparse().  It is designed
 * to give you an opportunity to do anything that must be done to initialize
 * the parser (set global variables, configure starting state, etc.). One
 * thing it already does for you is assign the value of the global variable
 * yydebug that controls whether yacc prints debugging information about
 * parser actions (shift/reduce) and contents of state stack during parser.
 * If set to false, no information is printed. Setting it to true will give
 * you a running trail that might be helpful when debugging your parser.
 * Please be sure the variable is set to false when submitting your final
 * version.
 */
void InitParser()
{
   PrintDebug("parser", "Initializing parser");
   yydebug = true;
   ;
}
