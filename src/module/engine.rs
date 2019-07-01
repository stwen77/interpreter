use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::sync::Arc;
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
    //Int(FnDef),
}

pub type FnAny = Fn(Vec<&mut Any>) -> Result<Box<Any>, ()>;

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
		self.functions.get(&spec)
			.ok_or(())
			.and_then(move |f| match **f{
				FnIntExt::Ext(ref f) => f(args),
				//FnIntExt::Int(ref f) => (),
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
}
