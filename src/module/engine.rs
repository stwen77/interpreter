use super::any::{Any, AnyExt};
use super::parser::{parse, Expr, FnDef, Statment};
use std::any::TypeId;
use std::collections::HashMap;
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

    pub fn register_fn<P, Q, R, FN, RET>(&mut self, name: &str, f: FN)
    where
        FN: Fn(P, Q, R) -> RET + 'static,
        RET: Any,
        P: Any + Clone,
        Q: Any + Clone,
        R: Any + Clone,
    {
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
    pub fn evaluate_express(&self, scope: &mut Scope, expr: &Expr) -> Result<Box<Any>, ()> {
        let mut testv = 0;
        println!("expr {:?}" ,expr);
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
            Expr::FnCall(ref fn_name, ref args) => {
                self.call_fn(fn_name.to_owned(), vec![&mut testv])
            }
            _ => Err(()),
        }
    }
    fn eval_stmt(&self, scope: &mut Scope, stmt: &Statment) -> Result<Box<Any>, ()> {
        match *stmt {
            Statment::Expr(ref e) => self.evaluate_express(scope, e),
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
}
