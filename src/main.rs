// TODO: 4. Provide safe term constructors

use std::fmt;

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Variable(String),
    Function(Box<Type>, Box<Type>),
    Undefined,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Variable(name) => write!(f, "{}", name),
            Type::Function(t1, t2) => write!(f, "({} -> {})", t1, t2),
            Type::Undefined => write!(f, "undefined")
        }
    }
}

#[derive(Debug, Clone)]
enum Term {
    Variable(String, Type),
    Abstraction(String, Type, Box<Term>),
    Application(Box<Term>, Box<Term>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Variable(name, _) => write!(f, "{}", name),
            Term::Abstraction(vname, vtype, t) => write!(f, "λ{}:{}. {})", vname, vtype, t),
            Term::Application(t1, t2) => write!(f, "{} {}", t1, t2),
        }
    }
}

#[derive(Clone)]
enum AST {
    Variable(String),
    Function(String, Vec<AST>),
    Assume(Box<AST>, Type),
}

macro_rules! s {
    ($str: expr) => { $str.to_string() }
}

macro_rules! impI {
    ($($t: expr), *) => { AST::Function(s!("impI"), vec!($($t, )*)) }
}

macro_rules! impE {
    ($($t: expr), *) => { AST::Function(s!("impE"), vec!($($t, )*)) }
}

macro_rules! assume {
    ($t1: expr, $t2: expr) => { AST::Assume(Box::new($t1), $t2) }
}

macro_rules! fun {
    ($t1: expr, $t2: expr) => { Type::Function(Box::new($t1), Box::new($t2)) }
}

/*
macro_rules! check_args {
    ($args: expr, $($ty: ty), *)
}
*/

// reduces function type
// apply_fun(a -> b, b) => a
fn apply_fun(t1: Type, t2: Type) -> Result<Type, String> {
    if let Type::Function(a, b) = t1.clone() {
        if *a == t2 {
            Ok(t1)
        }
        else {
            Err(format!("argument type of '{:?}' and '{:?}' must be equal", t1, t2))
        }
    }
    else {
        Err(format!("'{:?}' must be function type", t1))
    }
}

fn eval(ast: AST) -> Result<(Term, Type), String> {
    match ast {
        AST::Variable(name) => Ok((Term::Variable(name, Type::Undefined), Type::Undefined)),
        AST::Function(name, args) => {
            match &*name {
                "impI" => {
                    let (t1, t1t) = eval(args[0].clone())?;
                    let (t2, t2t) = eval(args[1].clone())?;
                    if let Term::Variable(name, _) = t1 {
                        Ok((Term::Abstraction(name, t1t.clone(), Box::new(t2)), fun!(t1t, t2t)))
                    }
                    else {
                        Err(s!("t1 must be a variable"))
                    }

                },
                "impE" => {
                    let (t1, t1t) = eval(args[0].clone())?;
                    let (t2, t2t) = eval(args[1].clone())?;
                    if let Type::Function(_, _) = t1t {
                        Ok((Term::Application(Box::new(t1), Box::new(t2)), apply_fun(t1t, t2t)?))
                    }
                    else {
                        Err(s!("t1 must be a abstraction"))
                    }

                },
                _ => Err(format!("'{}' is not defined", name)),
            }
        }
        AST::Assume(t1, t2) => {
            let (t1, t1t) = eval(*t1)?;
            if let Term::Variable(name, _) = t1 {
                Ok((Term::Variable(name, t2.clone()), t2))
            }
            else {
                Err(s!("t1 must be a Variable"))
            }
        }
    }
}

fn main() {
    let x = AST::Variable(s!("x"));
    let y = AST::Variable(s!("y"));
    let a = Type::Variable(s!("a"));
    let b = Type::Variable(s!("b"));
    let prop = impI!(x.clone(), impI!(y.clone(), impE!(assume!(y, fun!(a.clone(), b)), assume!(x, a))));

    let (expr, ty) = eval(prop).unwrap();
    println!("{}", expr);
    println!("{}", ty);
}
