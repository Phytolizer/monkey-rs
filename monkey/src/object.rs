use enum_dispatch::enum_dispatch;

pub(crate) trait Truthy {
    fn isTruthy(&self) -> bool;
}

impl Truthy for ObjectEnum {
    fn isTruthy(&self) -> bool {
        !matches!(
            self,
            ObjectEnum::Null(Null) | ObjectEnum::Boolean(Boolean { value: false })
        )
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ObjectKind {
    INTEGER,
    BOOLEAN,
    NULL,
    RETURN_VALUE,
}

#[enum_dispatch]
#[derive(Debug, Clone, PartialEq)]
pub enum ObjectEnum {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
    ReturnValue(ReturnValue),
}

#[enum_dispatch(ObjectEnum)]
pub trait Object: std::fmt::Debug + Clone + PartialEq {
    fn Type(&self) -> ObjectKind;
    fn Inspect(&self) -> String;
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Integer {
    pub value: i64,
}

impl Object for Integer {
    fn Type(&self) -> ObjectKind {
        ObjectKind::INTEGER
    }

    fn Inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Boolean {
    pub value: bool,
}

impl Object for Boolean {
    fn Type(&self) -> ObjectKind {
        ObjectKind::BOOLEAN
    }

    fn Inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Null;

impl Object for Null {
    fn Type(&self) -> ObjectKind {
        ObjectKind::NULL
    }

    fn Inspect(&self) -> String {
        "null".into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnValue(pub(crate) Box<ObjectEnum>);

impl Object for ReturnValue {
    fn Type(&self) -> ObjectKind {
        ObjectKind::RETURN_VALUE
    }

    fn Inspect(&self) -> String {
        self.0.Inspect()
    }
}
