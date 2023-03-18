use std::arch::global_asm;

use crate::compiled_expr::Comb;

use super::{Cell, CellPtr, IntoCellPtr, TigreEngine, CALL_LEN, CALL_OPCODE};

#[macro_use]
pub mod macros;

/// Implementations of bulk combinators.
// Bulk combinators are automatically generated using a build script.
pub mod bulk;
pub use bulk::ALL_COMBINATORS;

// LIT combinator
global_asm!(
    r#"
    .global comb_LIT
    comb_LIT:
        pop rax
        mov rax, [rax]
        ret
"#
);
extern "C" {
    pub fn comb_LIT() -> usize;
}
// The LIT combinator is an implementation detail of Tigre, and therefore not added to the `Comb` enum.
// Instead we define a struct type for it, so it can be used as an argument in functions generic over [`IntoCellPtr`]
pub struct Lit;
impl IntoCellPtr for Lit {
    fn into_cell_ptr(&self, _: &TigreEngine) -> CellPtr {
        CellPtr(comb_LIT as *mut Cell)
    }
}

// Abort combinator
#[allow(non_snake_case)]
pub extern "C" fn comb_Abort() -> usize {
    panic!("Abort called")
}

// I combinator
global_asm!(
    r#"
    .global comb_I
    comb_I:
        pop rax
        jmp [rax]
"#
);
extern "C" {
    pub fn comb_I() -> usize;
}

// K combinator
global_asm!(
    r#"
    .global comb_K
    comb_K:
        mov rax, [rsp]
        add rsp, 16
        jmp [rax]
"#
);
extern "C" {
    pub fn comb_K() -> usize;
}

impl TigreEngine {
    // Updates the left and right values of the cell for which `top_arg_ptr` points to the right (argument) value.
    unsafe fn update_top_cell<CP0: IntoCellPtr, CP1: IntoCellPtr>(
        &mut self,
        left: CP0,
        right: CP1,
        top_arg_ptr: *const CellPtr,
    ) -> CellPtr {
        let left = left.into_cell_ptr(self);
        let right = right.into_cell_ptr(self);
        // Pointers on the stack are to the right pointer of a cell, after the call instruction.
        // To get a pointer to the full cell, we subtract the length of a call instruction.
        let top_cell_ptr = CellPtr((top_arg_ptr as *mut u8).offset(-CALL_LEN) as *mut Cell);
        let top_cell = self.cell_mut(top_cell_ptr);
        debug_assert_eq!(top_cell.call_opcode, CALL_OPCODE);
        top_cell.set_call_addr(left.0 as usize);
        top_cell.arg = right.0 as i64;

        top_cell_ptr
    }
}

macros::comb3!(comb_S, make_s, |engine, f, g, x| {
    // Make new cells for (f x), (g x)
    let fx = engine.make_cell(*f, *x);
    let gx = engine.make_cell(*g, *x);
    engine.update_top_cell(fx, gx, x)
});

macros::comb3!(comb_B, make_b, |engine, f, g, x| {
    // Make new cell for (g x)
    let gx = engine.make_cell(*g, *x);
    engine.update_top_cell(*f, gx, x)
});

macros::comb3!(comb_C, make_c, |engine, f, g, x| {
    // Make new cell for (f x)
    let fx = engine.make_cell(*f, *x);
    engine.update_top_cell(fx, *g, x)
});

// A pointer to a function that evaluates an argument to a strict operator.
type ArgFn = unsafe extern "C" fn(*mut TigreEngine) -> i64;
macros::comb_bin_op!(comb_plus, apply_plus, |a, b| a + b);
macros::comb_bin_op!(comb_min, apply_min, |a, b| a - b);
macros::comb_bin_op!(comb_eq, apply_eq, |a, b| if a == b { 1 } else { 0 });
macros::comb_bin_op!(comb_lt, apply_lt, |a, b| if a < b { 1 } else { 0 });
macros::comb_bin_op!(comb_times, apply_times, |a, b| a * b);

macros::comb3!(comb_cond, apply_cond, |engine, c, t, f| {
    // Evaluate the c cell as a strict argument
    let c = c as *const ArgFn;
    // TODO: avoid loading pointers to both branches in the assembly
    let c_res = (*c)(engine as *mut TigreEngine);
    let branch_ptr = match c_res {
        0 => f,
        1 => t,
        v => {
            // We expect never to reach this code.
            // In debug mode we panic, in release the behaviour is undefined.
            debug_assert!(v == 0 || v == 1, "condition variable should be 0 or 1");
            unsafe { std::hint::unreachable_unchecked() }
        }
    };

    // Update the top cell with an indirection to the taken branch.
    engine.update_top_cell(Comb::I, *branch_ptr, f);
    *branch_ptr
});

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lit_dispatch() {
        let res = unsafe { lit_dispatch_helper() };
        assert_eq!(res, 42);
    }

    global_asm!(
        r#"
        .global lit_dispatch_helper
        lit_dispatch_helper:
        call comb_LIT
        .long 42
        .long 0
    "#
    );
    extern "C" {
        fn lit_dispatch_helper() -> u64;
    }
}
