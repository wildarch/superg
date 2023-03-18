use std::fs::File;
use std::io::Write;

// Generate bulk combinators Sn, Bn and Cn up to this n
const BULK_COMB_MAX_N: usize = 10;

fn main() -> std::io::Result<()> {
    let out_dir = std::env::var("OUT_DIR").unwrap();

    // This script depends only on itself
    println!("cargo:rerun-if-changed=build.rs");

    let turner_combinators = std::fs::File::create(&format!("{}/turner_combinators.rs", out_dir))?;
    generate_turner_combinators(turner_combinators)?;

    let tigre_combinators = std::fs::File::create(&format!("{}/tigre_combinators.rs", out_dir))?;
    generate_tigre_combinators(tigre_combinators)?;

    Ok(())
}

fn generate_turner_combinators(mut f: File) -> std::io::Result<()> {
    let max_combn = std::cmp::max(
        // Regular combinators use up to 3 arguments.
        3,
        // Bulk combinator take two functions f and g and distribute n arguments over them.
        2 + BULK_COMB_MAX_N,
    );

    writeln!(f, "use super::*;")?;
    writeln!(f, "impl TurnerEngine {{")?;

    for i in 1..=max_combn {
        writeln!(f, "    macros::run_comb!(run_comb{i}, {i});")?;
    }

    for i in 2..=BULK_COMB_MAX_N {
        writeln!(
            f,
            "    macros::run_sn_comb!(run_s{}_comb, run_comb{});",
            i,
            i + 2
        )?;
        writeln!(
            f,
            "    macros::run_bn_comb!(run_b{}_comb, run_comb{});",
            i,
            i + 2
        )?;
        writeln!(
            f,
            "    macros::run_cn_comb!(run_c{}_comb, run_comb{});",
            i,
            i + 2
        )?;
    }

    writeln!(f, "}}")?;

    // Implementation mapping function
    write!(
        f,
        r#"
impl Comb {{
    pub(super) fn implementation(&self) -> fn(&mut TurnerEngine) -> Option<CellPtr> {{
        match self {{
            Comb::S => TurnerEngine::run_s_comb,
            Comb::K => TurnerEngine::run_k_comb,
            Comb::I => TurnerEngine::run_i_comb,
            Comb::B => TurnerEngine::run_b_comb,
            Comb::C => TurnerEngine::run_c_comb,
            Comb::Plus => TurnerEngine::run_plus_comb,
            Comb::Minus => TurnerEngine::run_minus_comb,
            Comb::Times => TurnerEngine::run_times_comb,
            Comb::Cond => TurnerEngine::run_cond_comb,
            Comb::Eq => TurnerEngine::run_eq_comb,
            Comb::Lt => TurnerEngine::run_lt_comb,
            Comb::Abort => TurnerEngine::run_abort_comb,
        "#
    )?;
    for i in 2..=BULK_COMB_MAX_N {
        write!(
            f,
            r#"
            Comb::Sn({i}) => TurnerEngine::run_s{i}_comb,
            Comb::Bn({i}) => TurnerEngine::run_b{i}_comb,
            Comb::Cn({i}) => TurnerEngine::run_c{i}_comb,
            "#
        )?;
    }
    write!(
        f,
        r#"
            _ => todo!("No implementation for combinator {{:?}}", self),
        }}
        "#
    )?;
    writeln!(f, "    }}")?;
    writeln!(f, "}}")?;
    Ok(())
}

fn generate_tigre_combinators(mut f: File) -> std::io::Result<()> {
    writeln!(f, "use super::*;")?;

    for i in 2..=BULK_COMB_MAX_N {
        for (k, k_upper) in [('s', 'S'), ('b', 'B'), ('c', 'C')] {
            writeln!(
                f,
                "macros::bulk_comb_{k}!(comb_{k_upper}{}, {}, {}, {}, make_{k}{});",
                // Number of arguments to distribute
                i,
                // Number of values on the stack (f, g and xs)
                i + 2,
                // Size of the values on the stack plus the engine pointer, in bytes
                (i + 3) * 8,
                // Size of the values on the stack, plus the engine pointer and an extra 8 bytes for alignment to a 16 byte boundary.
                (i + 4) * 8,
                i
            )?;
        }
    }

    // all combinators
    write!(
        f,
        r#"
pub const ALL_COMBINATORS: &[unsafe extern "C" fn() -> usize] = &[
    comb_LIT, 
    comb_Abort, 
    comb_I, 
    comb_K, 
    comb_S, 
    comb_B, 
    comb_C, 
    comb_plus, 
    comb_min, 
    comb_eq,
    comb_lt, 
    comb_times, 
    comb_cond, 
    "#
    )?;

    for i in 2..BULK_COMB_MAX_N {
        writeln!(f, "    comb_S{},", i)?;
        writeln!(f, "    comb_B{},", i)?;
        writeln!(f, "    comb_C{},", i)?;
    }

    writeln!(f, "];")?;

    // Impls
    for k in ['S', 'B', 'C'] {
        write!(
            f,
            r#"
pub const COMB_{k}N_IMPLS: &[unsafe extern "C" fn() -> usize] = &[
    comb_Abort,
    comb_{k},
        "#
        )?;
        for i in 2..BULK_COMB_MAX_N {
            writeln!(f, "    comb_{k}{i},")?;
        }
        writeln!(f, "];")?;
    }

    Ok(())
}
