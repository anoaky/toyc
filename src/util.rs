pub trait CompilerPass {
    fn num_errors(&self) -> u32;
    fn inc_error(&mut self);
    fn has_error(&self) -> bool {
        self.num_errors() > 0
    }
}

#[macro_export]
macro_rules! rc {
    ( $e:expr ) => {
        Rc::new($e)
    };
}

#[macro_export]
macro_rules! rc_ref {
    ( $e:expr ) => {{
        use std::cell::RefCell;
        use std::rc::Rc;
        Rc::new(RefCell::new($e))
    }};
}
