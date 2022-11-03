/*****************************************************
 *  Implementation of the second semantic analysis pass.
 *
 *  In the second pass, we will check:
 *    1. whether all the expressions are well-typed; (and sets ATTR(type))
 *    2. whether all the statements are well-formed;
 *
 *  Keltin Leung 
 */

#include "3rdparty/list.hpp"
#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "compiler.hpp"
#include "config.hpp"
#include "scope/scope_stack.hpp"
#include "symb/symbol.hpp"
#include "type/type.hpp"

using namespace mind;
using namespace mind::type;
using namespace mind::scope;
using namespace mind::symb;
using namespace mind::util;
using namespace mind::err;

/* Pass 2 of the semantic analysis.
 */
class SemPass2 : public ast::Visitor {
    // constants
    virtual void visit(ast::IntConst *);
    // lvalues
    virtual void visit(ast::VarRef *);
    virtual void visit(ast::ArrayRef *);

    // Visiting expressions
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
    virtual void visit(ast::CallExpr   * e);


    // Visiting statements
    virtual void visit(ast::VarDecl *);
    virtual void visit(ast::CompStmt *);
    virtual void visit(ast::ExprStmt *);
    virtual void visit(ast::IfStmt *);
    virtual void visit(ast::ReturnStmt *);
    virtual void visit(ast::WhileStmt *);
    virtual void visit(ast::ForStmt *);
    virtual void visit(ast::DoWhileStmt *);

    virtual void visit(ast::BreakStmt *);
    virtual void visit(ast::ContStmt *);

    // Visiting declarations
    virtual void visit(ast::FuncDefn *);
    virtual void visit(ast::Program *);

    private:
    int loop_depth = 0;
};

// recording the current return type
static Type *retType = NULL;
// recording the current "this" type

/* Determines whether a given type is BaseType::Error.
 *
 * NOTE:
 *   don't use the == operator when comparing types
 * PARAMETERS:
 *   t     - the type to check
 */
static bool isErrorType(Type *t) { return t->equal(BaseType::Error); }

/* Checks whether an ast::Expr conforms to the expecting type.
 *
 * NOTE:
 *   if the expression type is BaseType::Error, we accept it as a legal case.
 * PARAMETERS:
 *   e     - the ast::Expr node
 *   t     - the expected type
 * SIDE-EFFECTS:
 *   Unexpected Type Error may be issued
 */
static void expect(ast::Expr *e, Type *t) {
    if (!e->ATTR(type)->equal(t) && !isErrorType(e->ATTR(type))) {
        issue(e->getLocation(), new UnexpectedTypeError(t, e->ATTR(type)));
    }
}

/* Visits an ast::IntConst node.
 *
 * PARAMETERS:
 *   e     - the ast::IntConst node
 */
void SemPass2::visit(ast::IntConst *e) { e->ATTR(type) = BaseType::Int; }


/* Visits an all node inherited from ast::UnaryExprBase
 *
 * PARAMETERS:
 *   e     - the ast::UnaryExprBase node
 */
void SemPass2::visitUnaryExpr(ast::UnaryExprBase *e) {
    e->e->accept(this);
    expect(e->e, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Visits an all node inherited from ast::BinaryExprBase
 *
 * PARAMETERS:
 *   e     - the ast::BinaryExprBase node
 */
void SemPass2::visitBinaryExpr(ast::BinaryExprBase * e) {
    e->e1->accept(this);
    expect(e->e1, BaseType::Int);

    e->e2->accept(this);
    expect(e->e2, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

void SemPass2::visit(ast::IfExpr * e){
    e->condition->accept(this);
    expect(e->condition, BaseType::Int);

    e->true_brch->accept(this);
    expect(e->true_brch, BaseType::Int);

    e->false_brch->accept(this);
    expect(e->false_brch, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}




/* Visits an ast::LvalueExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::LvalueExpr node
 */
void SemPass2::visit(ast::LvalueExpr *e) {
    e->lvalue->accept(this);
    e->ATTR(type) = e->lvalue->ATTR(type);
}

/* Visits an ast::VarRef node.
 *
 * PARAMETERS:
 *   e     - the ast::VarRef node
 */
void SemPass2::visit(ast::VarRef *ref) {
    // CASE I: owner is NULL ==> referencing a local var or a member var?

    Variable *v = ref->ATTR(sym);

    ref->ATTR(type) = v->getType();
    
    // std::cerr << ref->ATTR(type) << std::endl;

    // local int is simple, array, globals are memory vars
    if (!v->isGlobalVar() && ref->ATTR(type)->equal(BaseType::Int)) {
        
        ref->ATTR(lv_kind) = ast::Lvalue::SIMPLE_VAR;
    } else {
        ref->ATTR(lv_kind) = ast::Lvalue::MEM_VAR;
    }

    return;

}

void SemPass2::visit(ast::ArrayRef *ref) {
    // CASE I: owner is NULL ==> referencing a local var or a member var?

    ref->arr_base->accept(this);

    ref->index->accept(this);
    expect(ref->index, BaseType::Int);

    ArrayType* arr_type = dynamic_cast<ArrayType*>(ref->arr_base->ATTR(type));

    if (!arr_type) {
        issue(ref->getLocation(), new SyntaxError("base type is not array"));
        ref->ATTR(type) = BaseType::Error;
        return;
    }

    ref->ATTR(type) = arr_type->getElementType();

    ref->ATTR(lv_kind) = ast::Lvalue::MEM_VAR;


    return;

}

/* Visits an ast::VarDecl node.
 *
 * PARAMETERS:
 *   decl     - the ast::VarDecl node
 */
void SemPass2::visit(ast::VarDecl *decl) {
    if (decl->init)
        decl->init->accept(this);
}

/* Visits an ast::AssignStmt node.
 *
 * PARAMETERS:
 *   e     - the ast::AssignStmt node
 */
void SemPass2::visit(ast::AssignExpr *s) {
    s->left->accept(this);
    s->e->accept(this);

    if (!isErrorType(s->left->ATTR(type)) &&
        !s->e->ATTR(type)->compatible(s->left->ATTR(type))) {
        issue(s->getLocation(),
              new IncompatibleError(s->left->ATTR(type), s->e->ATTR(type)));
    }

    s->ATTR(type) = s->left->ATTR(type);
}

/* Visits an ast::ExprStmt node.
 *
 * PARAMETERS:
 *   e     - the ast::ExprStmt node
 */
void SemPass2::visit(ast::ExprStmt *s) { s->e->accept(this); }

/* Visits an ast::IfStmt node.
 *
 * PARAMETERS:
 *   e     - the ast::IfStmt node
 */
void SemPass2::visit(ast::IfStmt *s) {
    s->condition->accept(this);
    if (!s->condition->ATTR(type)->equal(BaseType::Int)) {
        issue(s->condition->getLocation(), new BadTestExprError());
    }
    if (dynamic_cast<ast::VarDecl *>(s->true_brch)) {
        issue(s->true_brch->getLocation(), new SyntaxError("expect statement"));
    }
    if (dynamic_cast<ast::VarDecl *>(s->false_brch)) {
        issue(s->false_brch->getLocation(), new SyntaxError("expect statement"));
    }

    

    s->true_brch->accept(this);
    s->false_brch->accept(this);
}

/* Visits an ast::CompStmt node.
 *
 * PARAMETERS:
 *   c     - the ast::CompStmt node
 */
void SemPass2::visit(ast::CompStmt *c) {
    scopes->open(c->ATTR(scope));
    for (auto it = c->stmts->begin(); it != c->stmts->end(); ++it)
        (*it)->accept(this);
    scopes->close();
}
/* Visits an ast::WhileStmt node.
 *
 * PARAMETERS:
 *   e     - the ast::WhileStmt node
 */
void SemPass2::visit(ast::WhileStmt *s) {
    
    s->condition->accept(this);
    if (!s->condition->ATTR(type)->equal(BaseType::Int)) {
        issue(s->condition->getLocation(), new BadTestExprError());
    }
    loop_depth++;
    s->loop_body->accept(this);
    loop_depth--;
}

void SemPass2::visit(ast::ForStmt *s) {
    
    s->init->accept(this);
    s->condition->accept(this);

    if (!s->condition->ATTR(type)->equal(BaseType::Int)) {
        issue(s->condition->getLocation(), new BadTestExprError());
    }

    s->update->accept(this);

    loop_depth++;
    s->loop_body->accept(this);
    loop_depth--;
}


void SemPass2::visit(ast::DoWhileStmt *s) {
    
    loop_depth++;
    s->loop_body->accept(this);
    loop_depth--;

    s->condition->accept(this);
    if (!s->condition->ATTR(type)->equal(BaseType::Int)) {
        issue(s->condition->getLocation(), new BadTestExprError());
    }

}

void SemPass2::visit(ast::BreakStmt *s) {
    if (loop_depth == 0) {
        issue(s->getLocation(), new SyntaxError("break outside loop"));
    }
}

void SemPass2::visit(ast::ContStmt *s) {
    if (loop_depth == 0) {
        issue(s->getLocation(), new SyntaxError("continue outside loop"));
    }
}

/* Visits an ast::ReturnStmt node.
 *
 * PARAMETERS:
 *   e     - the ast::ReturnStmt node
 */
void SemPass2::visit(ast::ReturnStmt *s) {
    s->e->accept(this);

    if (!isErrorType(retType) && !s->e->ATTR(type)->compatible(retType)) {
        issue(s->e->getLocation(),
              new IncompatibleError(retType, s->e->ATTR(type)));
    }
}

/* Visits an ast::FunDefn node.
 *
 * PARAMETERS:
 *   e     - the ast::FunDefn node
 */
void SemPass2::visit(ast::FuncDefn *f) {
    if (f->forward_decl) {
        return; // do nothing
    }
    ast::StmtList::iterator it;

    retType = f->ret_type->ATTR(type);

    scopes->open(f->ATTR(sym)->getAssociatedScope());
    for (it = f->stmts->begin(); it != f->stmts->end(); ++it)
        (*it)->accept(this);
    scopes->close();
}

void SemPass2::visit(ast::CallExpr * e){
    auto param_list = e->ATTR(sym)->getType()->getArgList();
    auto param_it = param_list->begin();

    // check if the number of arguments is correct
    if (e->args->length() != param_list->length()) {
        issue(e->getLocation(), new BadArgCountError(e->ATTR(sym)));
    }

    // check type of arguments
    for (auto arg_it = e->args->begin(); arg_it != e->args->end(); ++arg_it) {
        (*arg_it)->accept(this);

        if (!(*arg_it)->ATTR(type)->compatible(*param_it)) {
            issue((*arg_it)->getLocation(),
                  new IncompatibleError(*param_it, (*arg_it)->ATTR(type)));
        }
        
        ++param_it;
    }

    e->ATTR(type) = e->ATTR(sym)->getType()->getResultType();

}

/* Visits an ast::Program node.
 *
 * PARAMETERS:
 *   e     - the ast::Program node
 */
void SemPass2::visit(ast::Program *p) {
    scopes->open(p->ATTR(gscope));
    for (auto it = p->func_and_globals->begin();
         it != p->func_and_globals->end(); ++it)
        (*it)->accept(this);
    scopes->close(); // close the global scope
}

/* Checks the types of all the expressions.
 *
 * PARAMETERS:
 *   tree  - AST of the program
 */
void MindCompiler::checkTypes(ast::Program *tree) {
    tree->accept(new SemPass2());
}
