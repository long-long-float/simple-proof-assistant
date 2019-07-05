// TODO: 4. Provide safe term constructors

enum Type {
    Variable(String),
    Function(Box<Type>, Box<Type>),
}

enum Term {
    Variable(String, Type),
    Abstraction(String, Type, Box<Term>),
    Application(Box<Term>, Box<Term>),
}

fn impI(t1: Term, t2: Term) -> Result<Term, String> {
    if let Term::Variable(name, ty) = t1 {
        Ok(Term::Abstraction(name, ty, Box::new(t2)))
    }
    else {
        Err("t1 must be a variable".to_string())
    }
}

fn impE(t1: Term, t2: Term) -> Result<Term, String> {
    if let Term::Abstraction(_, _, _) = t1 {
        Ok(Term::Application(Box::new(t1), Box::new(t2)))
    }
    else {
        Err("t1 must be a abstraction".to_string())
    }
}

fn assume(var: Term, ty: Type) -> Result<Term, String> {
    if let Term::Variable(name, vt) = var {
        // Need type equality between ty and vt?
        Ok(Term::Variable(name, ty))
    }
    else {
        Err("var must be a Variable".to_string())
    }
}

fn main() {
    println!("Hello, world!");
}
