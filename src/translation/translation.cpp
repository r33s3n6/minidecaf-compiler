/*****************************************************
 *  Implementation of the third translation pass.
 *
 *  In the third pass, we will:
 *    translate all the statements and expressions
 *
 *  Keltin Leung 
 */

#include "translation.hpp"
#include "asm/offset_counter.hpp"
#include "ast/ast.hpp"
#include "compiler.hpp"
#include "config.hpp"
#include "scope/scope.hpp"
#include "symb/symbol.hpp"
#include "tac/tac.hpp"
#include "tac/trans_helper.hpp"
#include "type/type.hpp"

using namespace mind;
using namespace mind::symb;
using namespace mind::tac;
using namespace mind::type;
using namespace mind::assembly;

/* Constructor.
 *
 * PARAMETERS:
 *   helper - the translation helper
 */
Translation::Translation(tac::TransHelper *helper) {
    mind_assert(NULL != helper);

    tr = helper;
}

/* Translating an ast::Program node.
 */
void Translation::visit(ast::Program *p) {
    for (auto it = p->func_and_globals->begin();
         it != p->func_and_globals->end(); ++it)
        (*it)->accept(this);
}

// three sugars for parameter offset management
#define RESET_OFFSET() tr->getOffsetCounter()->reset(OffsetCounter::PARAMETER)
#define NEXT_OFFSET(x) tr->getOffsetCounter()->next(OffsetCounter::PARAMETER, x)

/* Translating an ast::FuncDefn node.
 *
 * NOTE:
 *   call tr->startFunc() before translating the statements and
 *   call tr->endFunc() after all the statements have been translated
 */
void Translation::visit(ast::FuncDefn *f) {
    Function *fun = f->ATTR(sym);

    
    if (fun->getEntryLabel() == nullptr) {
            // attaching function entry label
        fun->attachEntryLabel(tr->getNewEntryLabel(fun));
    }

    if (f->forward_decl) {
        return;
    }


    // arguments
    int order = 0;
    for (auto it = f->formals->begin(); it != f->formals->end(); ++it) {
        auto v = (*it)->ATTR(sym);
        v->setOrder(order++);
        v->attachTemp(tr->getNewTempI4());
    }

    // Note: unused?
    fun->offset = fun->getOrder() * POINTER_SIZE;

    RESET_OFFSET();

    tr->startFunc(fun);

    // save callee-saved registers
    for (int i=0;i<11;i++){
        callee_save_regs[i] = tr->genCalleeSave();
    }

    // get parameters
    for (auto it = f->formals->begin(); it != f->formals->end(); ++it) {
        auto v = (*it)->ATTR(sym);
        tr->genPop(v->getTemp());
    }


    // translates statement by statement
    for (auto it = f->stmts->begin(); it != f->stmts->end(); ++it)
        (*it)->accept(this);


    // restore callee-saved registers
    for (int i=10;i>=0;i--){
        tr->genCalleeRestore(callee_save_regs[i]);
    }
    tr->genReturn(tr->genLoadImm4(0)); // Return 0 by default

    tr->endFunc();
}

void Translation::visit(ast::CallExpr *e) {
    // calculates the arguments `e->args`

    for (auto it = e->args->begin(); it != e->args->end(); ++it)
        (*it)->accept(this);


    int param_count = e->args->length();
    auto it = e->args->begin();
    for (int i=0;i<param_count;i++){
        tr->genPush((*it)->ATTR(val));
        it++;
    }
    if (param_count > 8){
        // push the arguments in reverse order
        auto it = e->args->rbegin();
        for (int i=0;i<param_count-8;i++){
            tr->genPush((*it)->ATTR(val));
            it++;
        }
    }
    
    Function* fun = e->ATTR(sym);

    Temp ret = tr->genCall(fun->getEntryLabel());


    e->ATTR(val) = ret;
    
}

/* Translating an ast::AssignStmt node.
 *
 * NOTE:
 *   different kinds of Lvalue require different translation
 */
void Translation::visit(ast::AssignExpr *s) {
    // generate temp for right first
    s->e->accept(this);

    s->left->accept(this);

    ast::VarRef * ref;

    switch (s->left->ATTR(lv_kind)) {
    case ast::Lvalue::SIMPLE_VAR:
        ref = dynamic_cast<ast::VarRef *>(s->left);
        mind_assert(ref);
        tr->genAssign(ref->ATTR(sym)->getTemp(), s->e->ATTR(val));
        break;
    case ast::Lvalue::MEM_VAR:   // load address
        tr->genStore(s->e->ATTR(val), s->left->ATTR(addr), 0);
        break;
    default:
        mind_assert(false); // impossible
    }
  
    // s->ATTR(val) = s->left->ATTR(sym)->getTemp();
    s->ATTR(val) = s->e->ATTR(val);

}

/* Translating an ast::ExprStmt node.
 */
void Translation::visit(ast::ExprStmt *s) { s->e->accept(this); }

/* Translating an ast::IfStmt node.
 *
 * NOTE:
 *   you don't need to test whether the false_brch is empty
 */
void Translation::visit(ast::IfStmt *s) {
    if(dynamic_cast<ast::EmptyStmt*>(s->false_brch)) {
        Label done_label = tr->getNewLabel(); // done
        s->condition->accept(this);
        tr->genJumpOnZero(done_label, s->condition->ATTR(val));
        s->true_brch->accept(this);
        tr->genMarkLabel(done_label);

    } else {
        Label false_branch_label = tr->getNewLabel(); // entry of the false branch

        Label done_label = tr->getNewLabel(); // done
        s->condition->accept(this);
        tr->genJumpOnZero(false_branch_label, s->condition->ATTR(val));

        s->true_brch->accept(this);
        tr->genJump(done_label); // done

        tr->genMarkLabel(false_branch_label);
        s->false_brch->accept(this);

        tr->genMarkLabel(done_label);

    }

}
/* Translating an ast::WhileStmt node.
 */
void Translation::visit(ast::WhileStmt *s) {
    Label start = tr->getNewLabel();
    Label end = tr->getNewLabel();

    Label old_break = current_break_label;
    Label old_cont = current_cont_label;
    current_break_label = end;

    tr->genMarkLabel(start);
    s->condition->accept(this);
    tr->genJumpOnZero(end, s->condition->ATTR(val));

    s->loop_body->accept(this);
    tr->genJump(start);

    tr->genMarkLabel(end);

    current_break_label = old_break;
    current_cont_label = old_cont;
}


void Translation::visit(ast::ForStmt *s) {
    Label update = tr->getNewLabel();
    Label start = tr->getNewLabel();
    Label end = tr->getNewLabel();

    Label old_break = current_break_label;
    Label old_cont = current_cont_label;
    current_break_label = end;
    current_cont_label = update;

    s->init->accept(this);

    tr->genMarkLabel(start);
    s->condition->accept(this);
    tr->genJumpOnZero(end, s->condition->ATTR(val));

    s->loop_body->accept(this);

    tr->genMarkLabel(update);
    s->update->accept(this);
    tr->genJump(start);

    tr->genMarkLabel(end);

    current_break_label = old_break;
    current_cont_label = old_cont;
}

void Translation::visit(ast::DoWhileStmt *s) {
    Label start = tr->getNewLabel();
    Label end = tr->getNewLabel();

    Label old_break = current_break_label;
    Label old_cont = current_cont_label;
    current_break_label = end;
    current_cont_label = start;

    tr->genMarkLabel(start);
    s->loop_body->accept(this);
    s->condition->accept(this);
    tr->genJumpOnZero(end, s->condition->ATTR(val));

    tr->genJump(start);

    tr->genMarkLabel(end);

    current_break_label = old_break;
    current_cont_label = old_cont;
}


/* Translating an ast::BreakStmt node.
 */
void Translation::visit(ast::BreakStmt *s) { 

    tr->genJump(current_break_label);
    
    
}

/* Translating an ast::ContStmt node.
 */
void Translation::visit(ast::ContStmt *s) { tr->genJump(current_cont_label); }

/* Translating an ast::CompStmt node.
 */
void Translation::visit(ast::CompStmt *c) {
    // translates statement by statement
    for (auto it = c->stmts->begin(); it != c->stmts->end(); ++it)
        (*it)->accept(this);
}
/* Translating an ast::ReturnStmt node.
 */
void Translation::visit(ast::ReturnStmt *s) {
    s->e->accept(this);
    for (int i=10;i>=0;i--){
        tr->genCalleeRestore(callee_save_regs[i]);
    }
    tr->genReturn(s->e->ATTR(val));
}

/* Translating an ast::AddExpr node.
 */
void Translation::visit(ast::AddExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genAdd(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::SubExpr node.
 */
void Translation::visit(ast::SubExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genSub(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::MulExpr node.
 */
void Translation::visit(ast::MulExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genMul(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::DivExpr node.
 */
void Translation::visit(ast::DivExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genDiv(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::ModExpr node.
 */
void Translation::visit(ast::ModExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genMod(e->e1->ATTR(val), e->e2->ATTR(val));
}
void Translation::visit(ast::EquExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genEqu(e->e1->ATTR(val), e->e2->ATTR(val));
}
void Translation::visit(ast::NeqExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genNeq(e->e1->ATTR(val), e->e2->ATTR(val));
}
void Translation::visit(ast::LeqExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genLeq(e->e1->ATTR(val), e->e2->ATTR(val));
}
void Translation::visit(ast::GeqExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genGeq(e->e1->ATTR(val), e->e2->ATTR(val));
}
void Translation::visit(ast::LesExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genLes(e->e1->ATTR(val), e->e2->ATTR(val));
}
void Translation::visit(ast::GrtExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genGrt(e->e1->ATTR(val), e->e2->ATTR(val));
}
void Translation::visit(ast::AndExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genLAnd(e->e1->ATTR(val), e->e2->ATTR(val));
}
void Translation::visit(ast::OrExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genLOr(e->e1->ATTR(val), e->e2->ATTR(val));
}

void Translation::visit(ast::IfExpr *e) {
    e->ATTR(val) = tr->getNewTempI4(); // result
    Label false_branch_label = tr->getNewLabel(); // false branch
    Label done_label = tr->getNewLabel(); // done

    e->condition->accept(this); // condition
    tr->genJumpOnZero(false_branch_label, e->condition->ATTR(val));
    // true branch
    e->true_brch->accept(this);
    tr->genAssign(e->ATTR(val), e->true_brch->ATTR(val));
    tr->genJump(done_label);

    tr->genMarkLabel(false_branch_label);
    // false branch
    e->false_brch->accept(this);
    tr->genAssign(e->ATTR(val), e->false_brch->ATTR(val));

    tr->genMarkLabel(done_label);

}   


/* Translating an ast::IntConst node.
 */
void Translation::visit(ast::IntConst *e) {
    e->ATTR(val) = tr->genLoadImm4(e->value);
}

/* Translating an ast::NegExpr node.
 */
void Translation::visit(ast::NegExpr *e) {
    e->e->accept(this);

    e->ATTR(val) = tr->genNeg(e->e->ATTR(val));
}

/* Translating an ast::NotExpr node.
 */
void Translation::visit(ast::NotExpr *e) {
    e->e->accept(this);

    e->ATTR(val) = tr->genLNot(e->e->ATTR(val));
}

/* Translating an ast::NegExpr node.
 */
void Translation::visit(ast::BitNotExpr *e) {
    e->e->accept(this);

    e->ATTR(val) = tr->genBNot(e->e->ATTR(val));
}

/* Translating an ast::LvalueExpr node.
 *
 * NOTE:
 *   different Lvalue kinds need different translation
 */
void Translation::visit(ast::LvalueExpr *e) {
    e->lvalue->accept(this);

    ast::VarRef *ref;

    switch (e->lvalue->ATTR(lv_kind)) {
    case ast::Lvalue::SIMPLE_VAR:
        ref = dynamic_cast<ast::VarRef *>(e->lvalue);
        mind_assert(ref);
        e->ATTR(val) =ref->ATTR(sym)->getTemp();
        break;
    case ast::Lvalue::MEM_VAR:   // load address
        e->ATTR(val) = tr->genLoad(e->lvalue->ATTR(addr), 0);
        break;
    default:
        mind_assert(false); // impossible
    }
  

}

/* Translating an ast::VarRef node.
 *
 * NOTE:
 *   there are two kinds of variable reference: member variables or simple
 * variables
 */
void Translation::visit(ast::VarRef *ref) {
    symb::Variable *var;

    switch (ref->ATTR(lv_kind)) {
    case ast::Lvalue::SIMPLE_VAR:
        // nothing to do
        break;
    case ast::Lvalue::MEM_VAR:   // load address
        var = dynamic_cast<symb::Variable *>(ref->ATTR(sym));
        mind_assert(var);
        if(var->isGlobalVar()){
            ref->ATTR(addr) = tr->genLoadSymbol(var);
        }else{
            ref->ATTR(addr) = var->getTemp();
        }
        
        break;
    default:
        mind_assert(false); // impossible
    }
    // actually it is so simple :-)
}

void Translation::visit(ast::ArrayRef *ref) {
    ref->arr_base->accept(this); // set ATTR(addr)
    ref->index->accept(this); // set ATTR(val)

    // get element size
    int elem_size = ref->ATTR(type)->getSize();

    // compute offset
    ref->ATTR(addr) = tr->genAdd(ref->arr_base->ATTR(addr),
                                 tr->genMul(ref->index->ATTR(val),
                                            tr->genLoadImm4(elem_size)));


}




/* Translating an ast::VarDecl node.
 */
void Translation::visit(ast::VarDecl *decl) {

    Variable *var = dynamic_cast<Variable *>(decl->ATTR(sym));
    mind_assert(var);

    if(var->isGlobalVar()) {
        // TODO: riscv_md do this now
        // global variable
        // tr->genGlobalVar(var);
    } else {
        // local variable
        if (var->getType()->equal(BaseType::Int)){
            var->attachTemp(tr->getNewTempI4());
            if(decl->init){
                decl->init->accept(this);
                tr->genAssign(decl->ATTR(sym)->getTemp(), decl->init->ATTR(val));
            }
        }else{
            var->attachTemp(tr->genAlloc(var->getType()->getSize()));
        } 
        
    }

    

}

/* Translates an entire AST into a Piece list.
 *
 * PARAMETERS:
 *   tree  - the AST
 * RETURNS:
 *   the result Piece list (represented by the first node)
 */
Piece *MindCompiler::translate(ast::Program *tree) {
    TransHelper *helper = new TransHelper(md);

    tree->accept(new Translation(helper));

    return helper->getPiece();
}
