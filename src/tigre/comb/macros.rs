//! Macros to generate implementations of combinators that require calls to Rust code.
//!
//! # Alignment
//! The System V ABI that Rust uses on x86-64 Linux (at least for an extern "C" `fn`) mandates that before a `call` instruction, the stack is aligned to 16 bytes (as documented [here](https://c9x.me/compile/doc/abi.html)).
//! In particular it seems that SIMD instructions require this alignment, without it I have observed crashes in Rust library code that uses such instructions.
//!
//! We regularly chain `call` instructions until a combinator a reached, so we cannot guarantee that the stack is aligned properly.
//! What we do know for sure is that the stack is aligned to 8 bytes, because we always push 64-bit values on the stack.
//! So, before we can jump to a helper function in Rust, we must check if the stack is aligned to 16 bytes.
//! If it is not, we jump to a `*_need_align` label and substract 8 bytes from the stack pointer.
//! Since the stack pointer is always aligned to at least 8 bytes, this must result in alignment to 16 bytes.
//! If the stack is already properly aligned, we can simply jump to the function.
//! After the call returns, we pop the argument off the stack, and if extra alignment was needed, add an extra 8 bytes offset to the stack pointer.

// Generate an assembly target and helper function for a combinator that takes three arguments.
macro_rules! comb3 {
    ($name:ident, $helper_func:ident, $impl:expr) => {
        global_asm!(
            concat!(".global ", stringify!($name)),
            concat!(stringify!($name), ":"),
            r#"
                // Load f, g, x pointers as arguments
                mov rsi, [rsp]
                mov rdx, [rsp+8]
                mov rcx, [rsp+16]
                // Save engine pointer
                push rdi

                // Align stack to 16 bytes if needed
                mov rax, rsp
                and rax, 8
                cmp rax, 0
            "#,
            concat!("jne ", stringify!($name), "_need_align"),
            // No alignment needed
            concat!(stringify!($name), "_no_align:"),
            concat!("call ", stringify!($helper_func)),
            r#"
                // Restore engine
                mov rdi, [rsp]
                // Pop arguments (+engine)
                add rsp, 32
                jmp rax
            "#,
            // Alignment needed
            concat!(stringify!($name), "_need_align:"),
            "sub rsp, 8",
            concat!("call ", stringify!($helper_func)),
            r#"
                // Restore engine
                mov rdi, [rsp+8]
                // Pop arguments
                add rsp, 40
                jmp rax
            "#,
        );
        extern "C" {
            pub fn $name() -> usize;
        }
        #[no_mangle]
        unsafe extern "C" fn $helper_func(
            engine: *mut TigreEngine,
            f: *const CellPtr,
            g: *const CellPtr,
            x: *const CellPtr,
        ) -> CellPtr {
            let fimpl: fn(
                &mut TigreEngine,
                *const CellPtr,
                *const CellPtr,
                *const CellPtr,
            ) -> CellPtr = $impl;
            fimpl(&mut *engine, f, g, x)
        }
    };
}
pub(crate) use comb3;

// Generate an assembly target and helper function for a binary operator.
macro_rules! comb_bin_op {
    ($name:ident, $helper_func:ident, $op:expr) => {
        global_asm!(
            concat!(".global ", stringify!($name)),
            concat!(stringify!($name), ":"),
            r#"
                // Load argument pointers as arguments
                mov rsi, [rsp]
                mov rdx, [rsp+8]

                // Save engine
                push rdi

                // Align stack to 16 bytes if needed
                mov rax, rsp
                and rax, 8
                cmp rax, 0
            "#,
            concat!("jne ", stringify!($name), "_need_align"),
            // No alignment needed
            concat!(stringify!($name), "_no_align:"),
            concat!("call ", stringify!($helper_func)),
            r#"
                // Restore engine
                mov rdi, [rsp]
                // Pop arguments (+engine)
                add rsp, 24
                // Return the computed value
                ret
            "#,
            // Alignment needed
            concat!(stringify!($name), "_need_align:"),
            "sub rsp, 8",
            concat!("call ", stringify!($helper_func)),
            r#"
                // Restore engine
                mov rdi, [rsp+8]
                // Pop arguments (+engine)
                add rsp, 32
                // Return the computed value
                ret
            "#,
        );
        extern "C" {
            pub fn $name() -> usize;
        }
        #[no_mangle]
        unsafe extern "C" fn $helper_func(
            engine: *mut TigreEngine,
            a0: *const ArgFn,
            a1: *const ArgFn,
        ) -> i64 {
            let op: fn(i64, i64) -> i64 = $op;
            let res = op((*a0)(engine), (*a1)(engine));

            // Update the top cell to the literal value
            (*engine).update_top_cell(Lit, res, a1 as *const CellPtr);
            res
        }
    };
}
pub(crate) use comb_bin_op;

// Instantiates a generic bulk combinator with no specific implementation.
macro_rules! bulk_comb {
    ($name:ident, $arg_count:literal, $stack_off_no_align:literal, $stack_off_need_align:literal, $helper_func:ident, $impl:expr) => {
        global_asm!(
            concat!(".global ", stringify!($name)),
            concat!(stringify!($name), ":"),
            r#"
                // Load pointer to stacked arguments
                mov rsi, rsp

                // Save engine
                push rdi

                // Align stack to 16 bytes if needed
                mov rax, rsp
                and rax, 8
                cmp rax, 0
            "#,
            concat!("jne ", stringify!($name), "_need_align"),
            // No alignment needed
            concat!(stringify!($name), "_no_align:"),
            concat!("call ", stringify!($helper_func)),
            // Restore engine
            "mov rdi, [rsp]",
            // Pop arguments (+engine)
            concat!("add rsp, ", stringify!($stack_off_no_align)),
            "jmp rax",
            // Alignment needed
            concat!(stringify!($name), "_need_align:"),
            "sub rsp, 8",
            concat!("call ", stringify!($helper_func)),
            // Restore engine
            "mov rdi, [rsp+8]",
            // Pop arguments (+engine)
            concat!("add rsp, ", stringify!($stack_off_need_align)),
            "jmp rax",
        );
        extern "C" {
            pub fn $name() -> usize;
        }
        #[no_mangle]
        unsafe extern "C" fn $helper_func(
            engine: *mut TigreEngine,
            args: *const *const CellPtr,
        ) -> CellPtr {
            let fimpl: fn(&mut TigreEngine, &[&CellPtr]) -> CellPtr = $impl;
            // Check that the stack size arguments to the macro were valid
            debug_assert_eq!(($arg_count + 1) * 8, $stack_off_no_align);
            debug_assert_eq!(($arg_count + 2) * 8, $stack_off_need_align);
            let args = args as *const &CellPtr;
            let args = std::slice::from_raw_parts(args, $arg_count);
            fimpl(&mut *engine, args)
        }
    };
}

// Instantiates a bulk S combinator
macro_rules! bulk_comb_s {
    ($name:ident, $arg_count:literal, $stack_off_no_align:literal, $stack_off_need_align:literal, $helper_func:ident) => {
        bulk_comb!(
            $name,
            $arg_count,
            $stack_off_no_align,
            $stack_off_need_align,
            $helper_func,
            |engine, args| {
                let f = *args[0];
                let g = *args[1];

                let mut left_cell = f;
                let mut right_cell = g;
                for arg in &args[2..] {
                    left_cell = engine.make_cell(left_cell, **arg);
                    right_cell = engine.make_cell(right_cell, **arg);
                }

                // Update the top cell to point to the new left_cell and right_cell
                let top_arg_ptr = *args.last().unwrap() as *const CellPtr;
                engine.update_top_cell(left_cell, right_cell, top_arg_ptr)
            }
        );
    };
}
pub(crate) use bulk_comb_s;

// Instantiates a bulk B combinator
macro_rules! bulk_comb_b {
    ($name:ident, $arg_count:literal, $stack_off_no_align:literal, $stack_off_need_align:literal, $helper_func:ident) => {
        bulk_comb!(
            $name,
            $arg_count,
            $stack_off_no_align,
            $stack_off_need_align,
            $helper_func,
            |engine, args| {
                let f = *args[0];
                let g = *args[1];

                let mut right_cell = g;
                for arg in &args[2..] {
                    right_cell = engine.make_cell(right_cell, **arg);
                }

                // Update the top cell to point to the new left_cell and right_cell
                let top_arg_ptr = *args.last().unwrap() as *const CellPtr;
                engine.update_top_cell(f, right_cell, top_arg_ptr)
            }
        );
    };
}
pub(crate) use bulk_comb_b;

// Instantiates a bulk C combinator
macro_rules! bulk_comb_c {
    ($name:ident, $arg_count:literal, $stack_off_no_align:literal, $stack_off_need_align:literal, $helper_func:ident) => {
        bulk_comb!(
            $name,
            $arg_count,
            $stack_off_no_align,
            $stack_off_need_align,
            $helper_func,
            |engine, args| {
                let f = *args[0];
                let g = *args[1];

                let mut left_cell = f;
                for arg in &args[2..] {
                    left_cell = engine.make_cell(left_cell, **arg);
                }

                // Update the top cell to point to the new left_cell and right_cell
                let top_arg_ptr = *args.last().unwrap() as *const CellPtr;
                engine.update_top_cell(left_cell, g, top_arg_ptr)
            }
        );
    };
}
pub(crate) use bulk_comb_c;
