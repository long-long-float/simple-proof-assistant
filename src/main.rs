use std::fmt;

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Variable(String),
    Function(Box<Type>, Box<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Variable(name) => write!(f, "{}", name),
            Type::Function(t1, t2) => write!(f, "({} -> {})", t1, t2),
        }
    }
}

#[derive(Debug, Clone)]
enum Term {
    Variable(String),
    Abstraction(String, Type, Box<Term>),
    Application(Box<Term>, Box<Term>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Variable(name) => write!(f, "{}", name),
            Term::Abstraction(vname, vtype, t) => write!(f, "λ {}:{}. {}", vname, vtype, t),
            Term::Application(t1, t2) => write!(f, "{} {}", t1, t2),
        }
    }
}

#[derive(Clone)]
enum AST {
    ImpI(Term, Box<AST>),
    ImpE(Box<AST>, Box<AST>),
    Assume(Term, Type),
}

#[derive(Clone)]
enum Env<K, V> {
    Entry(K, V, Box<Env<K, V>>),
    Empty,
}

impl<K:PartialEq+Clone, V:Clone> Env<K, V> {
    fn new() -> Env<K, V> {
        Env::Empty
    }

    fn insert(&self, key: K, value: V) -> Env<K, V> {
        Env::Entry(key, value, Box::new(self.clone()))
    }

    fn find(&self, key: &K) -> Option<V> {
        match self {
            Env::Entry(k, v, n) => {
                if *k == *key {
                    Some(v.clone())
                } else {
                    n.find(key)
                }
            },
            Env::Empty => None
        }
    }

    fn marge(&self, env: Env<K, V>) -> Env<K, V> {
        match self {
            Env::Entry(k, v, n) => {
                n.marge(Env::Entry(k.clone(), v.clone(), Box::new(env)))
            }
            Env::Empty => env
        }
    }
}

type VarEnv = Env<String, Type>;

macro_rules! s {
    ($str: expr) => { $str.to_string() }
}

macro_rules! impI {
    ($t1: expr, $t2: expr) => { AST::ImpI($t1, Box::new($t2)) }
}

macro_rules! impE {
    ($t1: expr, $t2: expr) => { AST::ImpE(Box::new($t1), Box::new($t2)) }
}

macro_rules! assume {
    ($t1: expr, $t2: expr) => { AST::Assume($t1, $t2) }
}

macro_rules! fun {
    ($t1: expr, $t2: expr) => { Type::Function(Box::new($t1), Box::new($t2)) }
}

// reduces function type
// apply_fun(a -> b, a) => b
fn apply_fun(t1: Type, t2: Type) -> Result<Type, String> {
    if let Type::Function(a, b) = t1.clone() {
        if *a == t2 {
            Ok(*b)
        }
        else {
            Err(format!("argument type of '{}' and '{}' must be equal", t1, t2))
        }
    }
    else {
        Err(format!("'{}' must be function type", t1))
    }
}

fn eval(ast: AST) -> Result<(Term, Type, VarEnv), String> {
    match ast {
        AST::ImpE(t1, t2) => {
            let (t1, t1t, env1) = eval(*t1)?;
            let (t2, t2t, env2) = eval(*t2)?;
            if let Type::Function(_, _) = t1t {
                Ok((Term::Application(Box::new(t1), Box::new(t2)), apply_fun(t1t, t2t)?, env1.marge(env2)))
            }
            else {
                Err(s!("t1 must be a abstraction"))
            }
        },
        AST::ImpI(t1, t2) => {
            let (t2, t2t, env2) = eval(*t2)?;
            if let Term::Variable(name) = t1 {
                let t1t = env2.find(&name).unwrap();
                Ok((Term::Abstraction(name, t1t.clone(), Box::new(t2)), fun!(t1t, t2t), env2))
            }
            else {
                Err(s!("t1 must be a variable"))
            }
        },
        AST::Assume(t1, t2) => {
            if let Term::Variable(name) = t1 {
                Ok((Term::Variable(name.clone()), t2.clone(), VarEnv::new().insert(name, t2)))
            }
            else {
                Err(s!("t1 must be a Variable"))
            }
        }
    }
}

fn main() {
    let x = Term::Variable(s!("x"));
    let y = Term::Variable(s!("y"));
    let a = Type::Variable(s!("a"));
    let b = Type::Variable(s!("b"));
    let prop = impI!(x.clone(), impI!(y.clone(), impE!(assume!(y, fun!(a.clone(), b)), assume!(x, a))));

    let (expr, ty, _) = eval(prop).unwrap();
    println!("|- {}", ty);
    println!("because");
    println!("|- {} : {}", expr, ty);

    println!("");

    let m = Term::Variable(s!("m"));
    let n = Term::Variable(s!("n"));
    let f = Term::Variable(s!("f"));
    let z = Term::Variable(s!("z"));
    let a = Type::Variable(s!("a"));
    // (a -> a) -> (a -> a)
    let church = fun!(fun!(a.clone(), a.clone()), fun!(a.clone(), a.clone()));
    let prop = impI!(m.clone(), impI!(n.clone(), impI!(f.clone(), impI!(z.clone(), impE!(
                            impE!(assume!(m, church.clone()), assume!(f.clone(), fun!(a.clone(), a.clone()))),
                            impE!(impE!(assume!(n, church), assume!(f, fun!(a.clone(), a.clone()))), assume!(z, a))
                            )))));
    match eval(prop) {
        Ok((expr, ty, _)) => {
            println!("|- {}", ty);
            println!("because");
            println!("|- {} : {}", expr, ty);
        },
        Err(msg) => {
            println!("error: {}", msg);
        }
    }
}
