use enum_dispatch::enum_dispatch;

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ObjectKind {
    INTEGER,
    BOOLEAN,
    NULL,
}

#[enum_dispatch]
#[derive(Debug, Clone, PartialEq)]
pub enum ObjectEnum {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
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
