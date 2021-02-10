use std::fmt;

use fmt::Debug;

use crate::expression::Expression;
use crate::syntax::module::*;
use crate::syntax::routinedef::*;
use crate::syntax::statement::*;
use crate::syntax::structdef::*;

/**
 * This traverses the AST and determines what size register to
 * assign to each node in the AST: if it makes sense to assign
 * it to a register.
 */

pub struct TraverserMut {
}

impl TraverserMut {
    /**
     * Determine the size of register needed to store a value, based upon the number of bytes
     * the type takes.
     *
     * Custom types are always represented using 64bit registers because they are currently
     * always referred to via addresses.
     */

    pub fn for_module<A, B,F>(
        &self,
        m: &mut Module<A>,
        f: F,
    ) where A: Debug, B: Debug, F: Fn(A)->B + Copy {
        for child_module in m.get_modules_mut().iter_mut() {
            self.for_module(child_module, f);
        }
        self.for_items(m.get_functions_mut());

        self.for_items(m.get_coroutines_mut());

        self.for_items(m.get_structs_mut());
    }

    fn for_items<A>(
        &self,
        items: &mut Vec<Item<A>>,
    ) where A: Debug {
        for i in items.iter_mut() {
            match i {
                Item::Struct(sd) => {
                    self.for_structdef(sd);
                }
                Item::Routine(rd) => {
                    self.for_routine(rd);
                }
            };
        }
    }

    fn for_structdef<A>(
        &self,
        sd: &mut StructDef<A>,
    ) where A: Debug {
    }

    fn for_routine<A>(
        &self,
        rd: &mut RoutineDef<A>,
    ) where A: Debug {
        // loop through all the params
        for p in rd.get_params() {
        }

        // loop through every statement and analyze the child nodes of the routine definition
        for e in rd.get_body_mut().iter_mut() {
            self.for_statement(e);
        }
    }

    fn for_statement<A>(
        &self,
        statement: &mut Statement<A>,
    ) where A: Debug {
        match statement {
            Statement::Bind(b) => {
                self.for_bind(b);
            }
            Statement::Mutate(m) => {
                self.for_mutate(m);
            }
            Statement::Return(r) => {
                self.for_return(r);
            }
            Statement::YieldReturn(yr) => {
                self.for_yieldreturn(yr);
            }
            Statement::Expression(e) => {
                self.for_expression(e);
            }
        };
    }

    fn for_bind<A>(&self, bind: &mut Bind<A>) where A: Debug {
        self.for_expression(bind.get_rhs_mut())
    }

    fn for_mutate<A>(
        &self,
        mutate: &mut Mutate<A>,
    ) where A: Debug {
        self.for_expression(mutate.get_rhs_mut());
    }

    fn for_yieldreturn<A>(
        &self,
        yr: &mut YieldReturn<A>,
    ) where A: Debug {
        yr.get_value_mut()
            .as_mut()
            .map(|rv| self.for_expression(rv));
    }

    fn for_return<A>(&self, r: &mut Return<A>) where A: Debug {
        r.get_value_mut()
            .as_mut()
            .map(|rv| self.for_expression(rv));
    }

    fn for_expression<A>(
        &self,
        exp: &mut Expression<A>,
    ) where A: Debug {
        use Expression::*;

        match exp {
            ExpressionBlock(..) => self.for_expression_block(exp),
            Expression::Integer32(_m, _i) => {}
            Expression::Integer64(_m, _i) => {}
            Expression::Boolean(_m, _b) => {}
            Expression::StringLiteral(_m, _s) => {}
            Expression::CustomType(_m, _name) => {}
            Expression::Identifier(_m, _id) => {}
            Path(_m, _path) => {}
            Expression::IdentifierDeclare(_m, _id, _p) => {}
            MemberAccess(..) => self.for_member_access(exp),
            UnaryOp(..) => self.for_unary_op(exp),
            BinaryOp(..) => self.for_binary_op(exp),
            If { .. } => self.for_if(exp),
            Yield(..) => self.for_yield(exp),
            RoutineCall(..) => self.for_routine_call(exp),
            StructExpression(..) => self.for_struct_expression(exp),
        }
    }

    fn for_expression_block<A>(
        &self,
        block: &mut Expression<A>,
    ) where A: Debug {
        if let Expression::ExpressionBlock(_m, ref mut body, ref mut final_exp) = block {
            for e in body.iter_mut() {
                self.for_statement(e);
            }

            final_exp
                .as_mut()
                .map(|fe| self.for_expression(fe));
        } else {
            panic!("Expected ExpressionBlock, but got {:?}", block)
        }
    }

    fn for_member_access<A>(
        &self,
        access: &mut Expression<A>,
    ) where A: Debug {
        if let Expression::MemberAccess(_m, src, _member) = access {
            self.for_expression(src);
        } else {
            panic!("Expected MemberAccess, but got {:?}", access)
        }
    }

    fn for_unary_op<A>(
        &self,
        un_op: &mut Expression<A>,
    ) where A: Debug {
        if let Expression::UnaryOp(_m, _op, operand) = un_op {
            self.for_expression(operand);
        } else {
            panic!("Expected UnaryOp, but got {:?}", un_op)
        }
    }

    fn for_binary_op<A>(
        &self,
        bin_op: &mut Expression<A>,
    ) where A: Debug {
        if let Expression::BinaryOp(_m, _op, l, r) = bin_op {
            self.for_expression(l);
            self.for_expression(r);
        } else {
            panic!("Expected BinaryOp, but got {:?}", bin_op)
        }
    }

    fn for_if<A>(
        &self,
        if_exp: &mut Expression<A>,
    ) where A: Debug {
        if let Expression::If {
            annotation: _m,
            cond,
            if_arm,
            else_arm,
        } = if_exp
        {
            self.for_expression(cond);
            self.for_expression(if_arm);
            else_arm
                .as_mut()
                .map(|ea| self.for_expression(ea));
        } else {
            panic!("Expected IfExpression, but got {:?}", if_exp)
        }
    }

    fn for_routine_call<A>(
        &self,
        rc: &mut Expression<A>,
    ) where A: Debug {
        if let Expression::RoutineCall(_m, _call, _name, params) = rc {
            for p in params {
                self.for_expression(p);
            }
        } else {
            panic!("Expected RoutineCall, but got {:?}", rc)
        }
    }

    fn for_yield<A>(
        &self,
        yield_exp: &mut Expression<A>,
    ) where A: Debug {
        if let Expression::Yield(_m, e) = yield_exp {
            self.for_expression(e);
        } else {
            panic!("Expected Yield, but got {:?}", yield_exp)
        }
    }

    fn for_struct_expression<A>(
        &self,
        se: &mut Expression<A>,
    ) where A: Debug {
        if let Expression::StructExpression(_annotations, _struct_name, fields) = se {
            for (_, fe) in fields {
                self.for_expression(fe);
            }
        } else {
            panic!("Expected StructExpression, but got {:?}", se)
        }
    }
}
