/*****************************************************
 *  Implementation of the first semantic analysis pass.
 *
 *  In the first pass, we will:
 *    1. create appropriate type::Type instances for the types;
 *    2. create and manage scope::Scope instances;
 *    3. create symb::Symbol instances;
 *    4. manage the symbol tables.
 *  After this pass, ATTR(sym) is set for program, functions,
 *  variables, and variable references. ATTR(type) is set for
 *  related type of functions and variables.
 *
 *  Keltin Leung 
 */

#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "compiler.hpp"
#include "config.hpp"
#include "scope/scope.hpp"
#include "scope/scope_stack.hpp"
#include "symb/symbol.hpp"
#include "type/type.hpp"

using namespace mind;
using namespace mind::scope;
using namespace mind::symb;
using namespace mind::type;
using namespace mind::err;

/* Pass 1 of the semantic analysis.
 */
class SemPass1 : public ast::Visitor {
  public:
    // visiting declarations
    virtual void visit(ast::FuncDefn *);
    virtual void visit(ast::Program *);

    // visiting statements
    virtual void visit(ast::IfStmt *);
    virtual void visit(ast::WhileStmt *);
    virtual void visit(ast::CompStmt *);
    virtual void visit(ast::VarDecl *);
    virtual void visit(ast::ExprStmt *);
    virtual void visit(ast::ReturnStmt *);


    // unary operator
    void visitUnaryExpr(ast::UnaryExprBase* e);
    virtual void visit(ast::NegExpr    * e) { visitUnaryExpr(e); }
    virtual void visit(ast::NotExpr    * e) { visitUnaryExpr(e); }
    virtual void visit(ast::BitNotExpr * e) { visitUnaryExpr(e); }
    
    // binary operator
    void visitBinaryExpr(ast::BinaryExprBase* e);
    virtual void visit(ast::AddExpr * e) { visitBinaryExpr(e); }
    virtual void visit(ast::SubExpr * e) { visitBinaryExpr(e); }
    virtual void visit(ast::MulExpr * e) { visitBinaryExpr(e); }
    virtual void visit(ast::DivExpr * e) { visitBinaryExpr(e); }
    virtual void visit(ast::ModExpr * e) { visitBinaryExpr(e); }
    virtual void visit(ast::EquExpr * e) { visitBinaryExpr(e); }
    virtual void visit(ast::NeqExpr * e) { visitBinaryExpr(e); }
    virtual void visit(ast::LeqExpr * e) { visitBinaryExpr(e); }
    virtual void visit(ast::GeqExpr * e) { visitBinaryExpr(e); }
    virtual void visit(ast::LesExpr * e) { visitBinaryExpr(e); }
    virtual void visit(ast::GrtExpr * e) { visitBinaryExpr(e); }
    virtual void visit(ast::AndExpr * e) { visitBinaryExpr(e); }
    virtual void visit(ast::OrExpr  * e) { visitBinaryExpr(e); }

    // ternary operator
    virtual void visit(ast::IfExpr * e);


    // special expr
    virtual void visit(ast::LvalueExpr * e);
    virtual void visit(ast::AssignExpr * e);

    // lvalues
    virtual void visit(ast::VarRef *);
    // visiting types
    virtual void visit(ast::IntType *);
};

/* Visiting an ast::Program node.
 *
 * PARAMETERS:
 *   prog  - the ast::Progarm node to visit
 */
void SemPass1::visit(ast::Program *prog) {
    prog->ATTR(gscope) = new GlobalScope();
    scopes->open(prog->ATTR(gscope));

    // visit global variables and each function
    for (auto it = prog->func_and_globals->begin();
         it != prog->func_and_globals->end(); ++it) {
        (*it)->accept(this);
        if ((*it)->getKind() == mind::ast::ASTNode::FUNC_DEFN &&
            std::string("main") ==
                dynamic_cast<mind::ast::FuncDefn *>(*it)->name)
            prog->ATTR(main) =
                dynamic_cast<mind::ast::FuncDefn *>(*it)->ATTR(sym);
    }

    scopes->close(); // close the global scope
}

/* Visiting an ast::FunDefn node.
 *
 * NOTE:
 *   tasks include:
 *   1. build up the Function symbol
 *   2. build up symbols of the parameters
 *   3. build up symbols of the local variables
 *
 *   we will check Declaration Conflict Errors for symbols declared in the SAME
 *   class scope, but we don't check such errors for symbols declared in
 *   different scopes here (we leave this task to checkOverride()).
 * PARAMETERS:
 *   fdef  - the ast::FunDefn node to visit
 */
void SemPass1::visit(ast::FuncDefn *fdef) {
    fdef->ret_type->accept(this);
    Type *t = fdef->ret_type->ATTR(type);


    // checks the Declaration Conflict Error of Case 1 (but don't check Case
    // 2,3). if DeclConflictError occurs, we don't put the symbol into the
    // symbol table
    Symbol *sym = scopes->lookup(fdef->name, fdef->getLocation(), false);
    if (sym){
        issue(fdef->getLocation(), new DeclConflictError(fdef->name, sym));
        return;
    }
    
    Function *f = new Function(fdef->name, t, fdef->getLocation());
    fdef->ATTR(sym) = f;

    scopes->declare(f);

    // opens function scope
    scopes->open(f->getAssociatedScope());

    // adds the parameters
    for (ast::VarList::iterator it = fdef->formals->begin();
         it != fdef->formals->end(); ++it) {
        (*it)->accept(this);
        f->appendParameter((*it)->ATTR(sym));
    }

    // adds the local variables
    for (auto it = fdef->stmts->begin(); it != fdef->stmts->end(); ++it)
        (*it)->accept(this);

    // closes function scope
    scopes->close();
}

/* Visits an ast::IfStmt node.
 *
 * PARAMETERS:
 *   e     - the ast::IfStmt node
 */
void SemPass1::visit(ast::IfStmt *s) {
    s->condition->accept(this);
    s->true_brch->accept(this);
    s->false_brch->accept(this);
}

/* Visits an ast::WhileStmt node.
 *
 * PARAMETERS:
 *   e     - the ast::WhileStmt node
 */
void SemPass1::visit(ast::WhileStmt *s) {
    s->condition->accept(this);
    s->loop_body->accept(this);
}

/* Visiting an ast::CompStmt node.
 */
void SemPass1::visit(ast::CompStmt *c) {
    // opens function scope
    Scope *scope = new LocalScope();
    c->ATTR(scope) = scope;
    scopes->open(scope);

    // adds the local variables
    for (auto it = c->stmts->begin(); it != c->stmts->end(); ++it)
        (*it)->accept(this);

    // closes function scope
    scopes->close();
}

/* Visiting an ast::ReturnStmt node.
 */
void SemPass1::visit(ast::ReturnStmt *r) {
    r->e->accept(this);
}

void SemPass1::visit(ast::ExprStmt *s) {
    s->e->accept(this);
}

/* Visiting an ast::VarDecl node.
 *
 * NOTE:
 *   tasks include:
 *   1. build up the Variable symbol
 *   2. check Declaration Conflict Error
 *
 * PARAMETERS:
 *   vdecl - the ast::VarDecl node to visit
 */




void SemPass1::visitUnaryExpr(ast::UnaryExprBase* e) {
    e->e->accept(this);
}

void SemPass1::visitBinaryExpr(ast::BinaryExprBase* e) {
    e->e1->accept(this);
    e->e2->accept(this);
}

void SemPass1::visit(ast::IfExpr * e) {
    e->condition->accept(this);
    e->true_brch->accept(this);
    e->false_brch->accept(this);
}

void SemPass1::visit(ast::LvalueExpr * e){
    e->lvalue->accept(this);
}
void SemPass1::visit(ast::AssignExpr * e){
    e->e->accept(this);
    e->left->accept(this);
    
}

void SemPass1::visit(ast::VarRef * v){
    // search for the symbol in all scopes
    Symbol *sym = scopes->lookup(v->var, v->getLocation(), true);
    if (!sym){
        issue(v->getLocation(), new SymbolNotFoundError(v->var));
        return;
    }
        
    if(!sym->isVariable()){
        issue(v->getLocation(), new NotVariableError(sym));
        return;
    }

    v->ATTR(sym) = dynamic_cast<Variable *>(sym);

    mind_assert(v->ATTR(sym));

}

/* Visiting an ast::VarDecl node.
 *
 * NOTE:
 *   we will check Declaration Conflict Errors for symbols declared in the SAME
 *   function scope, but we don't check such errors for symbols declared in
 *   different scopes here (we leave this task to checkOverride()).
 * PARAMETERS:
 *   vdecl - the ast::VarDecl node to visit
 */
void SemPass1::visit(ast::VarDecl *vdecl) {
    Type *t = NULL;

    vdecl->type->accept(this);
    t = vdecl->type->ATTR(type);

    // checks the Declaration Conflict Error
    Symbol *sym = scopes->lookup(vdecl->name, vdecl->getLocation(), false);

    // we accept `main` as a variable name
    if (vdecl->name != "main" && sym){
        issue(vdecl->getLocation(), new DeclConflictError(vdecl->name, sym));
        return;
    }

    // Create a new `Variable` symbol
    Variable *v = new Variable(vdecl->name, t, vdecl->getLocation());
    scopes->declare(v);

    // TODO: 4. Special processing for global variables
    if (scopes->top()->getKind() == Scope::GLOBAL){
        // GlobalScope *gscope = dynamic_cast<GlobalScope *>(scopes->top());
        // gscope->
        // TODO:
    }

    // Tag the symbol to `vdecl->ATTR(sym)`
    vdecl->ATTR(sym) = v;

    if(vdecl->init){
        vdecl->init->accept(this);
    }

}

/* Visiting an ast::IntType node.
 *
 * PARAMETERS:
 *   itype - the ast::IntType node to visit
 */
void SemPass1::visit(ast::IntType *itype) { itype->ATTR(type) = BaseType::Int; }

/* Builds the symbol tables for the Mind compiler.
 *
 * PARAMETERS:
 *   tree  - the AST of the program
 */
void MindCompiler::buildSymbols(ast::Program *tree) {
    tree->accept(new SemPass1());
}
