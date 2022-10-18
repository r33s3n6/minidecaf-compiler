/*****************************************************
 *  The GNU Bison Specification for Mind Language.
 *
 *  We have provided complete SECTION I & IV for you.
 *  Please complete SECTION II & III.
 *
 *  In case you want some debug support, we provide a
 *  "diagnose()" function for you. All you need is to
 *  call this function in main.cpp.
 *
 *  Please refer to the ``GNU Flex Manual'' if you have
 *  problems about how to write the lexical rules.
 *
 *  Keltin Leung 
 */
%output "parser.cpp"
%skeleton "lalr1.cc"
%defines
%define api.value.type variant
%define api.token.constructor
%define parse.assert
%locations
/* SECTION I: preamble inclusion */
%code requires{
#include "config.hpp"
#include "ast/ast.hpp"
#include "location.hpp"
#include "parser.hpp"

using namespace mind;

// void yyerror (char const *);
void setParseTree(ast::Program* tree);

  /* This macro is provided for your convenience. */
#define POS(pos)    (new Location(pos.begin.line, pos.begin.column))


void scan_begin(const char* filename);
void scan_end();
}
%code{
  #include "compiler.hpp"
}
/* SECTION II: definition & declaration */

/*   SUBSECTION 2.1: token(terminal) declaration */


%define api.token.prefix {TOK_}
%token
   END  0    "end of file"
   BOOL      "bool"
   INT       "int"
   RETURN    "return"
   IF        "if"
   ELSE      "else"
   DO        "do"
   WHILE     "while"
   FOR       "for"
   BREAK     "break"
   CONTINUE  "continue"
   EQU       "=="
   NEQ       "!="
   AND       "&&" 
   OR        "||"
   LEQ       "<="
   GEQ       ">="
   PLUS      "+"
   MINUS     "-"
   TIMES     "*"
   SLASH     "/"
   MOD       "%"
   LT        "<"
   GT        ">"
   COLON     ":"
   SEMICOLON ";"
   LNOT      "!"
   BNOT      "~"
   COMMA     ","
   DOT       "."
   ASSIGN    "="
   QUESTION  "?"
   LPAREN    "("
   RPAREN    ")"
   LBRACK    "["
   RBRACK    "]"
   LBRACE    "{"
   RBRACE    "}"
;
%token <std::string> IDENTIFIER "identifier"
%token <int>         ICONST     "iconst"

%nterm <mind::ast::StmtList*>  StmtList
%nterm <mind::ast::VarList*>   FormalList NonNullFormalList
%nterm <mind::ast::Program*>   Program FoDList
%nterm <mind::ast::FuncDefn*>  FuncDefn
%nterm <mind::ast::ExprList*>  ExprList NonNullExprList
%nterm <mind::ast::Type*>      Type
%nterm <mind::ast::Statement*> Stmt ReturnStmt ExprStmt IfStmt CompStmt WhileStmt VarDecl ForStmt DoWhileStmt OptExprStmt
%nterm <mind::ast::Expr*>      Expr OptionalExpr 
%nterm <mind::ast::Lvalue*>    Lvalue

/*   SUBSECTION 2.2: associativeness & precedences */
%right    "then" "else"
%right    "="
%right    "?" ":"
%left     "||"
%left     "&&"
%left     "==" "!="
%left     "<=" ">=" "<" ">"
%left     "+" "-"
%left     "*" "/" "%"
%nonassoc "~" NEG "!"
%nonassoc "[" "."

%{
  /* we have to include scanner.hpp here... */
#define YY_NO_UNISTD_H 1
%}

/*   SUBSECTION 2.5: start symbol of the grammar */
%start Program

/* SECTION III: grammar rules (and actions) */
%%
Program     : FoDList
                { /* we don't write $$ = XXX here. */
				          setParseTree($1);
                }
            ;
FoDList     : FuncDefn 
                { $$ = new ast::Program($1, POS(@1)); } 
            | FoDList FuncDefn
                {
                  $1->func_and_globals->append($2);
                  $$ = $1;
                }
            ;
FuncDefn    : Type IDENTIFIER "(" FormalList ")" "{" StmtList "}" 
                {
                  $$ = new ast::FuncDefn($2, $1, $4, $7, POS(@1));
                }
            | Type IDENTIFIER "(" FormalList ")" ";"
                {
                  $$ = new ast::FuncDefn($2, $1, $4, new ast::EmptyStmt(POS(@6)), POS(@1));
                }
            ;
FormalList  : /* empty */
                { $$ = new ast::VarList(); }
            | NonNullFormalList
                { $$ = $1; }
            ;
NonNullFormalList
            : Type IDENTIFIER
                { 
                    $$ = new ast::VarList();
                    $$->append(new ast::VarDecl($2, $1, POS(@1)));
                } 
            | FormalList "," Type IDENTIFIER
                {
                  $$ = $1;
                  $$->append(new ast::VarDecl($4, $3, POS(@3)));
                }
            ;
Type        : INT
                { $$ = new ast::IntType(POS(@1)); }
StmtList    : /* empty */
                { $$ = new ast::StmtList(); }
            | StmtList Stmt
                { 
                  $1->append($2);
                  $$ = $1; 
                }
            ;

Stmt        :  ReturnStmt  {$$ = $1;}
            |  ExprStmt    {$$ = $1;}
            |  IfStmt      {$$ = $1;}
            |  WhileStmt   {$$ = $1;}
            |  ForStmt     {$$ = $1;}
            |  DoWhileStmt {$$ = $1;}
            |  CompStmt    {$$ = $1;}
            |  VarDecl     {$$ = $1;}
            |  "break" ";"  
                {$$ = new ast::BreakStmt(POS(@1));}
            |  "continue" ";"
                {$$ = new ast::ContStmt(POS(@1));}
            |  ";"
                {$$ = new ast::EmptyStmt(POS(@1));}
            ;
CompStmt    : "{" StmtList "}"
                {$$ = new ast::CompStmt($2, POS(@1));}
            ;
WhileStmt   : "while" "(" Expr ")" Stmt
                { $$ = new ast::WhileStmt($3, $5, POS(@1)); }
            ;
DoWhileStmt : "do" Stmt "while" "(" Expr ")" ";"
                { $$ = new ast::DoWhileStmt($2, $5, POS(@1)); }
            ;
ForStmt     : "for" "(" VarDecl OptionalExpr ";" OptionalExpr ")" Stmt
                { $$ = new ast::ForStmt($3, $4, $6, $8, POS(@1)); }
            | "for" "(" OptExprStmt OptionalExpr ";" OptionalExpr ")" Stmt
                { $$ = new ast::ForStmt($3, $4, $6, $8, POS(@1)); }
OptionalExpr: /* empty */
                { $$ = new ast::IntConst(1, POS(@0)); }
            | Expr
                { $$ = $1; }
            ;
OptExprStmt : OptionalExpr ";"
                { $$ = new ast::ExprStmt($1, POS(@1)); }
            ;
IfStmt      : "if" "(" Expr ")" Stmt %prec "then"
                { $$ = new ast::IfStmt($3, $5, new ast::EmptyStmt(POS(@5)), POS(@1)); }
            | "if" "(" Expr ")" Stmt "else" Stmt
                { $$ = new ast::IfStmt($3, $5, $7, POS(@1)); }
            ;

ReturnStmt  : "return" Expr ";"
                { $$ = new ast::ReturnStmt($2, POS(@1)); }
            ;
ExprStmt    : Expr ";"
                { $$ = new ast::ExprStmt($1, POS(@1)); } 
            ;  
VarDecl     : Type IDENTIFIER ";"
                { $$ = new ast::VarDecl($2, $1, POS(@1)); }
            | Type IDENTIFIER "=" Expr ";"
                { $$ = new ast::VarDecl($2, $1, $4, POS(@1)); }
            ;
Expr        : ICONST
                { $$ = new ast::IntConst($1, POS(@1)); }
            | Lvalue
                { $$ = new ast::LvalueExpr($1, POS(@1)); }        
            | "(" Expr ")"
                { $$ = $2; }
            | Expr "+" Expr
                { $$ = new ast::AddExpr($1, $3, POS(@2)); }
            | Expr "-" Expr
                { $$ = new ast::SubExpr($1, $3, POS(@2)); }
            | Expr "*" Expr
                { $$ = new ast::MulExpr($1, $3, POS(@2)); }
            | Expr "/" Expr
                { $$ = new ast::DivExpr($1, $3, POS(@2)); }
            | Expr "%" Expr
                { $$ = new ast::ModExpr($1, $3, POS(@2)); }
            | Expr "==" Expr
                { $$ = new ast::EquExpr($1, $3, POS(@2)); }
            | Expr "!=" Expr
                { $$ = new ast::NeqExpr($1, $3, POS(@2)); }
            | Expr "<=" Expr
                { $$ = new ast::LeqExpr($1, $3, POS(@2)); }
            | Expr ">=" Expr
                { $$ = new ast::GeqExpr($1, $3, POS(@2)); }
            | Expr "<" Expr
                { $$ = new ast::LesExpr($1, $3, POS(@2)); }
            | Expr ">" Expr
                { $$ = new ast::GrtExpr($1, $3, POS(@2)); }
            | Expr "&&" Expr
                { $$ = new ast::AndExpr($1, $3, POS(@2)); }
            | Expr "||" Expr
                { $$ = new ast::OrExpr ($1, $3, POS(@2)); }
            | Expr "?" Expr ":" Expr
                { $$ = new ast::IfExpr($1, $3, $5, POS(@2)); }
            | "-" Expr %prec NEG
                { $$ = new ast::NegExpr($2, POS(@1)); }
            | "~" Expr
                { $$ = new ast::BitNotExpr($2, POS(@1)); }
            | "!" Expr
                { $$ = new ast::NotExpr($2, POS(@1)); }
            | Lvalue "=" Expr
                { $$ = new ast::AssignExpr($1, $3, POS(@2)); }
            | IDENTIFIER "(" ExprList ")"
                { $$ = new ast::CallExpr($1, $3, POS(@1)); }
            ;
ExprList    : /* empty */
                { $$ = new ast::ExprList(); }
            | NonNullExprList
                { $$ = $1; }
            ;
NonNullExprList
            : Expr
                { $$ = new ast::ExprList(); $$->append($1); }
            | NonNullExprList "," Expr
                { 
                    $1->append($3);
                    $$ = $1;
                }
            ;

Lvalue      : IDENTIFIER
                { $$ = new ast::VarRef($1, POS(@1)); }
            ;

%%

/* SECTION IV: customized section */
#include "compiler.hpp"
#include <cstdio>

static ast::Program* ptree = NULL;
// extern int myline, mycol;   // defined in scanner.l
/*
// bison will generate code to invoke me
void
yyerror (char const *msg) {
  err::issue(new Location(myline, mycol), new err::SyntaxError(msg));
  scan_end();
  std::exit(1);
}
*/
// call me when the Program symbol is reduced
void
setParseTree(ast::Program* tree) {
  ptree = tree;
}

/* Parses a given mind source file.
 *
 * PARAMETERS:
 *   filename - name of the source file
 * RETURNS:
 *   the parse tree (in the form of abstract syntax tree)
 * NOTE:
 *   should any syntax error occur, this function would not return.
 */
ast::Program*
mind::MindCompiler::parseFile(const char* filename) {  
  scan_begin(filename);

  yy::parser parse;
  parse();
  scan_end();
  
  return ptree;
}

void
yy::parser::error (const location_type& l, const std::string& m)
{
  //std::cerr << l << ": " << m << '\n';
  err::issue(new Location(l.begin.line, l.begin.column), new err::SyntaxError(m));
  
  scan_end();
  std::exit(1);
}
