use super::any::{Any, AnyExt};
use super::parser::{parse, Expr, FnDef, Statment};
use std::any::TypeId;
use std::collections::HashMap;
use std::ops::{Add, BitAnd, BitOr, BitXor, Deref, Div, Mul, Neg, Rem, Shl, Shr, Sub};
use std::sync::Arc;

#[derive(Clone)]
pub struct Engine {
    pub functions: HashMap<FnSpec, Arc<FnIntExt>>,
    pub types: HashMap<TypeId, String>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct FnSpec {
    id: String,
    args: Option<Vec<TypeId>>,
}

pub enum FnIntExt {
    Ext(Box<FnAny>),
    Int(FnDef),
}

pub type FnAny = Fn(Vec<&mut Any>) -> Result<Box<Any>, ()>;

pub type Scope = Vec<(String, Box<Any>)>;
impl Engine {
    pub fn print_engine(&self) {
        println!("engine empty");
    }
    pub fn new() -> Self {
        let mut engine = Engine {
            functions: HashMap::new(),
            types: HashMap::new(),
        };
        return engine;
    }

    pub fn call_fn(&self, ident: String, args: Vec<&mut Any>) -> Result<Box<Any>, ()> {
        let spec = FnSpec {
            id: ident.clone(),
            args: Some(args.iter().map(|a| <Any as Any>::type_id(&**a)).collect()),
        };
        self.functions
            .get(&spec)
            .ok_or(())
            .and_then(move |f| match **f {
                FnIntExt::Ext(ref f) => f(args),
                FnIntExt::Int(ref f) => Err(()),
            });
        return Ok(Box::new(0));
    }

    pub fn register_fn_raw(&mut self, ident: String, args: Option<Vec<TypeId>>, f: Box<FnAny>) {
        let spec = FnSpec { id: ident, args };

        self.functions.insert(spec, Arc::new(FnIntExt::Ext(f)));
    }

    fn search_scope<'a, F, T>(scope: &'a mut Scope, id: &str, map: F) -> Result<(usize, T), ()>
    where
        F: FnOnce(&'a mut Any) -> Result<T, ()>,
    {
        scope
            .iter_mut()
            .enumerate()
            .rev()
            .find(|&(_, &mut (ref name, _))| *id == *name)
            .ok_or_else(|| ())
            .and_then(move |(idx, &mut (_, ref mut val))| map(val.as_mut()).map(|val| (idx, val)))
    }
    fn array_value(
        &self,
        scope: &mut Scope,
        id: &str,
        idx: &Expr,
    ) -> Result<(usize, usize, Box<Any>), ()> {
        let idx_boxed = self
            .evaluate_express(scope, idx)?
            .downcast::<i64>()
            .map_err(|_| ())?;
        let idx = *idx_boxed as usize;
        let (idx_sc, val) = Self::search_scope(scope, id, |val| {
            ((*val).downcast_mut() as Option<&mut Vec<Box<Any>>>)
                .map(|arr| arr[idx].clone())
                .ok_or(())
        })?;

        Ok((idx_sc, idx, val))
    }
    fn set_dot_val_helper(
        &self,
        this_ptr: &mut Any,
        dot_rhs: &Expr,
        mut source_val: Box<Any>,
    ) -> Result<Box<Any>, ()> {
        match *dot_rhs {
            Expr::Identifier(ref id) => {
                let set_fn_name = "set$".to_string() + id;
                self.call_fn(set_fn_name, vec![this_ptr, source_val.as_mut()])
            }
            Expr::Dot(ref inner_lhs, ref inner_rhs) => match **inner_lhs {
                Expr::Identifier(ref id) => {
                    let get_fn_name = "get$".to_string() + id;
                    self.call_fn(get_fn_name, vec![this_ptr])
                        .and_then(|mut v| {
                            self.set_dot_val_helper(v.as_mut(), inner_rhs, source_val)
                                .map(|_| v) // Discard Ok return value
                        })
                        .and_then(|mut v| {
                            let set_fn_name = "set$".to_string() + id;

                            self.call_fn(set_fn_name, vec![this_ptr, v.as_mut()])
                        })
                }
                _ => Err(()),
            },
            _ => Err(()),
        }
    }
    fn set_dot_val(
        &self,
        scope: &mut Scope,
        dot_lhs: &Expr,
        dot_rhs: &Expr,
        source_val: Box<Any>,
    ) -> Result<Box<Any>, ()> {
        match *dot_lhs {
            Expr::Identifier(ref id) => {
                let (sc_idx, mut target) = Self::search_scope(scope, id, |x| Ok(x.box_clone()))?;
                let value = self.set_dot_val_helper(target.as_mut(), dot_rhs, source_val);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                scope[sc_idx].1 = target;

                value
            }
            Expr::Index(ref id, ref idx_raw) => {
                let (sc_idx, idx, mut target) = self.array_value(scope, id, idx_raw)?;
                let value = self.set_dot_val_helper(target.as_mut(), dot_rhs, source_val);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                scope[sc_idx].1.downcast_mut::<Vec<Box<Any>>>().unwrap()[idx] = target;

                value
            }
            _ => Err(()),
        }
    }
    fn get_dot_val(
        &self,
        scope: &mut Scope,
        dot_lhs: &Expr,
        dot_rhs: &Expr,
    ) -> Result<Box<Any>, ()> {
        match *dot_lhs {
            Expr::Identifier(ref id) => {
                let (sc_idx, mut target) = Self::search_scope(scope, id, |x| Ok(x.box_clone()))?;
                let value = self.get_dot_val_helper(scope, target.as_mut(), dot_rhs);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                scope[sc_idx].1 = target;

                value
            }
            Expr::Index(ref id, ref idx_raw) => {
                let (sc_idx, idx, mut target) = self.array_value(scope, id, idx_raw)?;
                let value = self.get_dot_val_helper(scope, target.as_mut(), dot_rhs);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                scope[sc_idx].1.downcast_mut::<Vec<Box<Any>>>().unwrap()[idx] = target;

                value
            }
            _ => Err(()),
        }
    }
    fn get_dot_val_helper(
        &self,
        scope: &mut Scope,
        this_ptr: &mut Any,
        dot_rhs: &Expr,
    ) -> Result<Box<Any>, ()> {
        use std::iter::once;

        match *dot_rhs {
            Expr::FnCall(ref fn_name, ref args) => {
                let mut args: Vec<Box<Any>> = args
                    .iter()
                    .map(|arg| self.evaluate_express(scope, arg))
                    .collect::<Result<Vec<_>, _>>()?;
                let args = once(this_ptr)
                    .chain(args.iter_mut().map(|b| b.as_mut()))
                    .collect();

                self.call_fn(fn_name.to_owned(), args)
            }
            Expr::Identifier(ref id) => {
                let get_fn_name = "get$".to_string() + id;

                self.call_fn(get_fn_name, vec![this_ptr])
            }
            Expr::Index(ref id, ref idx_raw) => {
                let idx = self.evaluate_express(scope, idx_raw)?;
                let get_fn_name = "get$".to_string() + id;

                let mut val = self.call_fn(get_fn_name, vec![this_ptr])?;

                ((*val).downcast_mut() as Option<&mut Vec<Box<Any>>>)
                    .and_then(|arr| idx.downcast_ref::<i64>().map(|idx| (arr, *idx as usize)))
                    .map(|(arr, idx)| arr[idx].clone())
                    .ok_or(())
            }
            Expr::Dot(ref inner_lhs, ref inner_rhs) => match **inner_lhs {
                Expr::Identifier(ref id) => {
                    let get_fn_name = "get$".to_string() + id;
                    self.call_fn(get_fn_name, vec![this_ptr])
                        .and_then(|mut v| self.get_dot_val_helper(scope, v.as_mut(), inner_rhs))
                }
                _ => Err(()),
            },
            Expr::Array(ref contents) => {
                let mut arr = Vec::new();

                for item in &(*contents) {
                    let arg = self.evaluate_express(scope, item)?;
                    arr.push(arg);
                }

                Ok(Box::new(arr))
            }
            _ => Err(()),
        }
    }
    pub fn evaluate_express(&self, scope: &mut Scope, expr: &Expr) -> Result<Box<Any>, ()> {
        let mut testv = 0;
        println!("expr {:?}", expr);
        match *expr {
            Expr::IntConst(i) => Ok(Box::new(i)),
            Expr::FloatConst(i) => Ok(Box::new(i)),
            Expr::StringConst(ref s) => Ok(Box::new(s.clone())),
            Expr::CharConst(ref c) => Ok(Box::new(*c)),
            Expr::Identifier(ref id) => {
                for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                    if *id == *name {
                        return Ok(val.clone());
                    }
                }
                Err(())
            }
            Expr::Index(ref id, ref idx_raw) => {
                self.array_value(scope, id, idx_raw).map(|(_, _, x)| x)
            }
            Expr::Assignment(ref id, ref rhs) => {
                let rhs_val = self.evaluate_express(scope, rhs)?;

                match **id {
                    Expr::Identifier(ref n) => {
                        for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                            if *n == *name {
                                *val = rhs_val;

                                return Ok(Box::new(()));
                            }
                        }
                        Err(())
                    }
                    Expr::Index(ref id, ref idx_raw) => {
                        let idx = self.evaluate_express(scope, idx_raw)?;

                        for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                            if *id == *name {
                                if let Some(i) = idx.downcast_ref::<i64>() {
                                    if let Some(arr_typed) =
                                        (*val).downcast_mut() as Option<&mut Vec<Box<Any>>>
                                    {
                                        arr_typed[*i as usize] = rhs_val;
                                        return Ok(Box::new(()));
                                    } else {
                                        return Err(());
                                    }
                                } else {
                                    return Err(());
                                }
                            }
                        }

                        Err(())
                    }
                    Expr::Dot(ref dot_lhs, ref dot_rhs) => {
                        self.set_dot_val(scope, dot_lhs, dot_rhs, rhs_val)
                    }
                    _ => Err(()),
                }
            }
            Expr::Dot(ref lhs, ref rhs) => self.get_dot_val(scope, lhs, rhs),
            Expr::Array(ref contents) => {
                let mut arr = Vec::new();

                for item in &(*contents) {
                    let arg = self.evaluate_express(scope, item)?;
                    arr.push(arg);
                }

                Ok(Box::new(arr))
            }
            Expr::FnCall(ref fn_name, ref args) => {
                self.call_fn(fn_name.to_owned(), vec![&mut testv])
            }
            Expr::True => Ok(Box::new(true)),
            Expr::False => Ok(Box::new(false)),
            Expr::Unit => Ok(Box::new(())),
            _ => Err(()),
        }
    }
    fn eval_stmt(&self, scope: &mut Scope, stmt: &Statment) -> Result<Box<Any>, ()> {
        match *stmt {
            Statment::Expr(ref e) => self.evaluate_express(scope, e),
            Statment::Block(ref b) => {
                let prev_len = scope.len();
                let mut last_result: Result<Box<Any>, ()> = Ok(Box::new(()));

                for s in b.iter() {
                    last_result = self.eval_stmt(scope, s);
                    if let Err(x) = last_result {
                        last_result = Err(x);
                        break;
                    }
                }

                while scope.len() > prev_len {
                    scope.pop();
                }

                last_result
            }
            Statment::If(ref guard, ref body) => {
                let guard_result = self.evaluate_express(scope, guard)?;
                match guard_result.downcast::<bool>() {
                    Ok(g) => {
                        if *g {
                            self.eval_stmt(scope, body)
                        } else {
                            Ok(Box::new(()))
                        }
                    }
                    Err(_) => Err(()),
                }
            }
            Statment::IfElse(ref guard, ref body, ref else_body) => {
                let guard_result = self.evaluate_express(scope, guard)?;
                match guard_result.downcast::<bool>() {
                    Ok(g) => {
                        if *g {
                            self.eval_stmt(scope, body)
                        } else {
                            self.eval_stmt(scope, else_body)
                        }
                    }
                    Err(_) => Err(()),
                }
            }
            Statment::While(ref guard, ref body) => loop {
                let guard_result = self.evaluate_express(scope, guard)?;
                match guard_result.downcast::<bool>() {
                    Ok(g) => {
                        if *g {
                            match self.eval_stmt(scope, body) {
                                Err(()) => return Ok(Box::new(())), //It couldn't support break, since err type is deleted
                                Err(x) => return Err(x),
                                _ => (),
                            }
                        } else {
                            return Ok(Box::new(()));
                        }
                    }
                    Err(_) => return Err(()),
                }
            },
            Statment::Loop(ref body) => loop {
                match self.eval_stmt(scope, body) {
                    Err(()) => return Ok(Box::new(())), //same as while
                    Err(x) => return Err(x),
                    _ => (),
                }
            },
            Statment::Break => Err(()), //Err(EvalAltResult::LoopBreak) not supported
            Statment::Return => Ok(Box::new(())), //return need to be fixed
            Statment::ReturnWithVal(ref a) => {
                let result = self.evaluate_express(scope, a)?;
                Ok(result)
            }
            Statment::Var(ref name, ref init) => {
                match *init {
                    Some(ref v) => {
                        let i = self.evaluate_express(scope, v)?;
                        scope.push((name.clone(), i));
                    }
                    None => scope.push((name.clone(), Box::new(()))),
                };
                Ok(Box::new(()))
            }
            _ => Err(()),
        }
    }
    pub fn eval_file<T: Any + Clone>(&mut self, fname: &str) -> Result<T, ()> {
        use std::fs::File;
        use std::io::prelude::*;
        if let Ok(mut f) = File::open(fname) {
            let mut contents = String::new();

            if f.read_to_string(&mut contents).is_ok() {
                self.eval::<T>(&mut contents);
                Err(())
            } else {
                Err(())
            }
        } else {
            Err(())
        }
    }
    pub fn eval<T: Any + Clone>(&mut self, input: &mut str) -> Result<T, ()> {
        let tree = parse(input);
        let mut scope: Scope = Vec::new();

        match tree {
            Ok((ref os, ref fns)) => {
                let mut x: Result<Box<Any>, ()> = Ok(Box::new(()));
                for f in fns {
                    let name = f.name.clone();
                    let local_f = f.clone();

                    let spec = FnSpec {
                        id: name,
                        args: None,
                    };
                    self.functions
                        .insert(spec, Arc::new(FnIntExt::Int(local_f)));
                }

                for o in os {
                    x = match self.eval_stmt(&mut scope, o) {
                        Ok(v) => Ok(v),
                        Err(e) => Err(()),
                    };
                    println!("x = {:?}", x);
                }

                Err(())
            }
            Err(_) => Err(()),
        }
    }
    pub fn register_default_lib(engine: &mut Engine) {
        macro_rules! reg_op {
            ($engine:expr, $x:expr, $op:expr, $( $y:ty ),*) => (
                $(
                    $engine.register_fn($x, ($op as fn(x: $y, y: $y)->$y));
                )*
            )
        }

        fn add<T: Add>(x: T, y: T) -> <T as Add>::Output {
            x + y
        }
        fn sub<T: Sub>(x: T, y: T) -> <T as Sub>::Output {
            x - y
        }
        fn mul<T: Mul>(x: T, y: T) -> <T as Mul>::Output {
            x * y
        }
        fn div<T: Div>(x: T, y: T) -> <T as Div>::Output {
            x / y
        }
        fn neg<T: Neg>(x: T) -> <T as Neg>::Output {
            -x
        }
        fn lt<T: PartialOrd>(x: T, y: T) -> bool {
            x < y
        }
        fn lte<T: PartialOrd>(x: T, y: T) -> bool {
            x <= y
        }
        fn gt<T: PartialOrd>(x: T, y: T) -> bool {
            x > y
        }
        fn gte<T: PartialOrd>(x: T, y: T) -> bool {
            x >= y
        }
        fn eq<T: PartialEq>(x: T, y: T) -> bool {
            x == y
        }
        fn ne<T: PartialEq>(x: T, y: T) -> bool {
            x != y
        }
        fn and(x: bool, y: bool) -> bool {
            x && y
        }
        fn or(x: bool, y: bool) -> bool {
            x || y
        }
        fn not(x: bool) -> bool {
            !x
        }
        fn concat(x: String, y: String) -> String {
            x + &y
        }
        fn binary_and<T: BitAnd>(x: T, y: T) -> <T as BitAnd>::Output {
            x & y
        }
        fn binary_or<T: BitOr>(x: T, y: T) -> <T as BitOr>::Output {
            x | y
        }
        fn binary_xor<T: BitXor>(x: T, y: T) -> <T as BitXor>::Output {
            x ^ y
        }
        fn left_shift<T: Shl<T>>(x: T, y: T) -> <T as Shl<T>>::Output {
            x.shl(y)
        }
        fn right_shift<T: Shr<T>>(x: T, y: T) -> <T as Shr<T>>::Output {
            x.shr(y)
        }
        fn modulo<T: Rem<T>>(x: T, y: T) -> <T as Rem<T>>::Output {
            x % y
        }
        fn pow_i64_i64(x: i64, y: i64) -> i64 {
            x.pow(y as u32)
        }
        fn pow_f64_f64(x: f64, y: f64) -> f64 {
            x.powf(y)
        }
        fn pow_f64_i64(x: f64, y: i64) -> f64 {
            x.powi(y as i32)
        }
        fn unit_eq(a: (), b: ()) -> bool {
            true
        }

        //reg_op!(engine, "+", add, i32, i64, u32, u64, f32, f64);
    }
}

pub trait RegisterFn<FN, ARGS, RET> {
    fn register_fn(&mut self, name: &str, f: FN);
}

impl<P, Q, R, RET, FN> RegisterFn<FN, (P, Q, R), RET> for Engine
where
    FN: Fn(P, Q, R) -> RET + 'static,
    RET: Any,
    P: Any + Clone,
    Q: Any + Clone,
    R: Any + Clone,
{
    fn register_fn(&mut self, name: &str, f: FN) {
        let fun = move |mut args: Vec<&mut Any>| {
            let mut drain = args.drain(..);
            let P = ((*drain.next().unwrap()).downcast_mut() as Option<&mut P>).ok_or(())?;
            let Q = ((*drain.next().unwrap()).downcast_mut() as Option<&mut Q>).ok_or(())?;
            let R = ((*drain.next().unwrap()).downcast_mut() as Option<&mut R>).ok_or(())?;

            Ok(Box::new(f((Clone::clone)(P), (Clone::clone)(Q), (Clone::clone)(R))) as Box<Any>)
        };
        self.register_fn_raw(
            name.to_owned(),
            Some(vec![
                TypeId::of::<P>(),
                TypeId::of::<Q>(),
                TypeId::of::<R>(),
            ]),
            Box::new(fun),
        );
    }
}
impl<P, Q, RET, FN> RegisterFn<FN, (P, Q), RET> for Engine
where
    FN: Fn(P, Q) -> RET + 'static,
    RET: Any,
    P: Any + Clone,
    Q: Any + Clone,
{
    fn register_fn(&mut self, name: &str, f: FN) {
        let fun = move |mut args: Vec<&mut Any>| {
            let mut drain = args.drain(..);
            let P = ((*drain.next().unwrap()).downcast_mut() as Option<&mut P>).ok_or(())?;
            let Q = ((*drain.next().unwrap()).downcast_mut() as Option<&mut Q>).ok_or(())?;

            Ok(Box::new(f((Clone::clone)(P), (Clone::clone)(Q))) as Box<Any>)
        };
        self.register_fn_raw(
            name.to_owned(),
            Some(vec![TypeId::of::<P>(), TypeId::of::<Q>()]),
            Box::new(fun),
        );
    }
}
impl<P, RET, FN> RegisterFn<FN, (P,), RET> for Engine
where
    FN: Fn(P) -> RET + 'static,
    RET: Any,
    P: Any + Clone,
{
    fn register_fn(&mut self, name: &str, f: FN) {
        let fun = move |mut args: Vec<&mut Any>| {
            let mut drain = args.drain(..);
            let P = ((*drain.next().unwrap()).downcast_mut() as Option<&mut P>).ok_or(())?;

            Ok(Box::new(f((Clone::clone)(P))) as Box<Any>)
        };
        self.register_fn_raw(
            name.to_owned(),
            Some(vec![TypeId::of::<P>()]),
            Box::new(fun),
        );
    }
}
impl<FN, RET> RegisterFn<FN, (), RET> for Engine
where
    FN: Fn() -> RET + 'static,
    RET: Any,
{
    fn register_fn(&mut self, name: &str, f: FN) {
        let fun = move |mut args: Vec<&mut Any>| {
            let mut drain = args.drain(..);
            Ok(Box::new(f()) as Box<Any>)
        };
        self.register_fn_raw(name.to_owned(), Some(vec![]), Box::new(fun));
    }
}
