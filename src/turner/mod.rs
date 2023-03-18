use crate::compiled_expr::{Comb, CompiledExpr, ExprCompiler};
use std::io::Write;
use std::{collections::HashMap, fs::File, io::BufWriter, path::PathBuf};

use crate::ast;

#[macro_use]
mod macros;
// Bulk combinators are automatically generated using a build script.
mod combgen {
    include!(concat!(env!("OUT_DIR"), "/turner_combinators.rs"));
}

// TODO: Allow dynamically growing the heap.
const CELLS: usize = 250_000;

/// The core atom in the reduction machine.
/// Low pointer values represent combinators, high values are indices into the global cells array.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct CellPtr(i32);

// The I combinator is used so often that we hardwire a cell pointer to it.
pub const COMB_I: CellPtr = CellPtr(0);

impl CellPtr {
    pub fn comb(&self, engine: &TurnerEngineDebug) -> Option<Comb> {
        engine.comb_inst.get(self.0 as usize).copied()
    }

    pub fn is_comb(&self, engine: &TurnerEngine) -> bool {
        (self.0 as usize) < engine.comb_impl.len()
    }

    pub fn is_i_comb(&self) -> bool {
        return *self == COMB_I;
    }
}

trait IntoCellPtr {
    fn into_cell_ptr(self, engine: &TurnerEngine) -> CellPtr;
}

impl IntoCellPtr for CellPtr {
    fn into_cell_ptr(self, _: &TurnerEngine) -> CellPtr {
        self
    }
}

impl IntoCellPtr for Comb {
    fn into_cell_ptr(self, engine: &TurnerEngine) -> CellPtr {
        debug_assert!(self != Comb::I, "Use hardwired COMB_I instead (faster)");
        if let Some(idx) = engine.comb_map.get(&self) {
            CellPtr(*idx as i32)
        } else {
            panic!("Combinator {:?} not found in mapping", self);
        }
    }
}

/// Tag byte.
/// Contains bitflags for a cell.
#[derive(Debug, Copy, Clone)]
struct Tag(u8);

impl Tag {
    const WANTED: u8 = 1u8 << 7;
    const RHS_INT: u8 = 1u8 << 6;
    pub fn wanted() -> Tag {
        Tag(Tag::WANTED)
    }

    pub fn set_rhs_int(self) -> Tag {
        Tag(self.0 | Tag::RHS_INT)
    }

    pub fn set_wanted(self) -> Tag {
        Tag(self.0 | Tag::WANTED)
    }

    pub fn set_unwanted(self) -> Tag {
        Tag(self.0 & (!Tag::WANTED))
    }

    pub fn is_wanted(self) -> bool {
        return self.0 & Tag::WANTED != 0;
    }

    pub fn is_rhs_int(self) -> bool {
        return self.0 & Tag::RHS_INT != 0;
    }
}

pub struct TurnerEngine {
    // Conceptually, a cell looks like this:
    //
    // |=====|======|======|
    // | TAG | HEAD | TAIL |
    // |=====|======|======|
    //
    // We could store that in a Vec<(Tag, CellPtr,CellPtr), or even define a Cell type, but Miranda chooses this decomposed form, so we stick with that.
    // TODO: Experiment with storing tuples instead.
    tag: Vec<Tag>,
    hd: Vec<CellPtr>,
    tl: Vec<CellPtr>,
    // The next free cell
    next_cell: CellPtr,
    // Points to the cell containing the compiled definition
    def_lookup: HashMap<String, CellPtr>,
    stack: Vec<CellPtr>,

    // Combinator implementation lookup table
    comb_impl: Vec<fn(&mut TurnerEngine) -> Option<CellPtr>>,
    // Maps combinator to the index in comb_impl
    comb_map: HashMap<Comb, usize>,
}

#[derive(Debug)]
struct CompiledDef {
    name: String,
    expr: CompiledExpr,
}

impl crate::Engine for TurnerEngine {
    fn compile<C: ExprCompiler>(compiler: &mut C, program: &ast::Program) -> TurnerEngine {
        let mut engine = TurnerEngine {
            tag: vec![Tag::wanted(); CELLS],
            hd: vec![CellPtr(0i32); CELLS],
            tl: vec![CellPtr(0i32); CELLS],
            // We don't allocate cells when the pointer to it would be ambiguous
            next_cell: CellPtr(0 as i32),
            def_lookup: HashMap::new(),
            stack: Vec::new(),
            comb_impl: Vec::new(),
            comb_map: HashMap::new(),
        };

        // Compile all definitions
        let compiled_defs: Vec<CompiledDef> = program
            .defs
            .iter()
            .map(|def| CompiledDef {
                name: def.name.clone(),
                expr: compiler.compile(&def.as_lam()),
            })
            .collect();

        // We always need to have I mapped for indirection nodes.
        engine.add_comb_impl(Comb::I);
        // This must hold for Comb::is_I_comb to be correct
        assert_eq!(engine.comb_map.get(&Comb::I), Some(&0));
        // Populate the combinator lookup table based on the combinators present in the program
        for def in &compiled_defs {
            engine.add_comb_impls(&def.expr);
        }

        // Reserve a spot for each definition
        engine.add_comb_impl(Comb::Abort);
        for def in &compiled_defs {
            let cell_ptr = engine.make_cell(Tag::wanted(), COMB_I, Comb::Abort);
            if let Some(_) = engine.def_lookup.insert(def.name.clone(), cell_ptr) {
                panic!("Duplicate definition of {}", def.name);
            }
        }

        // Compile all definitions
        for def in compiled_defs {
            engine.alloc_def(def);
        }
        engine
    }

    fn run(&mut self) -> i32 {
        self.stack.clear();
        self.stack
            .push(*self.def_lookup.get("main").expect("no main function found"));
        loop {
            if let Some(cell_ptr) = self.step() {
                return self.get_int(cell_ptr).expect("did not evaluate to int");
            }
        }
    }
}

impl TurnerEngine {
    fn add_comb_impls(&mut self, e: &CompiledExpr) {
        match e {
            CompiledExpr::Comb(c) => self.add_comb_impl(*c),
            CompiledExpr::Ap(f, a) => {
                self.add_comb_impls(f);
                self.add_comb_impls(a);
            }
            CompiledExpr::Var(_) => {}
            CompiledExpr::Int(_) => {}
        }
    }

    fn add_comb_impl(&mut self, c: Comb) {
        if self.comb_map.contains_key(&c) {
            // Already added
            return;
        }
        // Add the combinator implementation
        self.comb_impl.push(c.implementation());
        let idx = self.comb_impl.len() - 1;
        self.comb_map.insert(c, idx);
        self.next_cell = CellPtr(idx as i32);
    }

    pub fn run(&mut self) -> i32 {
        self.stack.clear();
        self.stack
            .push(*self.def_lookup.get("main").expect("no main function found"));
        loop {
            if let Some(cell_ptr) = self.step() {
                return self.get_int(cell_ptr).expect("did not evaluate to int");
            }
        }
    }

    fn step(&mut self) -> Option<CellPtr> {
        let top = self.stack.last().unwrap();
        if let Some(comb_impl) = self.comb_impl.get(top.0 as usize) {
            comb_impl(self)
        } else {
            // An application, so push the left subtree
            self.stack.push(self.hd(*top));
            None
        }
    }

    fn run_s_comb(&mut self) -> Option<CellPtr> {
        self.run_comb3(|engine, args| {
            // S f g x => f x (g x)
            let x = engine.tl(args[2]);
            let g = engine.tl(args[1]);
            let f = engine.tl(args[0]);
            // If x is an int, we should transfer that to the new location
            let mut tag = Tag::wanted();
            if engine.tag(args[2]).is_rhs_int() {
                tag = tag.set_rhs_int();
            }
            // Make lower cells
            let left_cell = engine.make_cell(tag, f, x);
            let right_cell = engine.make_cell(tag, g, x);
            StepResult::CellContents(Tag::wanted(), left_cell, right_cell)
        })
    }

    fn run_k_comb(&mut self) -> Option<CellPtr> {
        self.run_comb2(|engine, args| {
            // K x y = x
            if let Some(v) = engine.int_rhs(args[0]) {
                StepResult::Value(v)
            } else {
                StepResult::Cell(engine.tl(args[0]))
            }
        })
    }

    fn run_i_comb(&mut self) -> Option<CellPtr> {
        self.run_comb1(|engine, args| {
            // I x = x
            if let Some(v) = engine.int_rhs(args[0]) {
                StepResult::Value(v)
            } else {
                StepResult::Cell(engine.tl(args[0]))
            }
        })
    }

    fn run_b_comb(&mut self) -> Option<CellPtr> {
        self.run_comb3(|engine, args| {
            // B f g x => f (g x)
            let x = engine.tl(args[2]);
            let g = engine.tl(args[1]);
            let f = engine.tl(args[0]);
            // If x is an int, we should transfer that to the new location
            let mut tag = Tag::wanted();
            if engine.tag(args[2]).is_rhs_int() {
                tag = tag.set_rhs_int();
            }
            let gx = engine.make_cell(tag, g, x);
            StepResult::CellContents(Tag::wanted(), f, gx)
        })
    }

    fn run_c_comb(&mut self) -> Option<CellPtr> {
        self.run_comb3(|engine, args| {
            // C f g x => (f x) g
            let x = engine.tl(args[2]);
            let g = engine.tl(args[1]);
            let f = engine.tl(args[0]);
            // If x is an int, we should transfer that to the new location
            let mut x_tag = Tag::wanted();
            if engine.tag(args[2]).is_rhs_int() {
                x_tag = x_tag.set_rhs_int();
            }
            // If g is an int, we should transfer that to the new location
            let mut g_tag = Tag::wanted();
            if engine.tag(args[1]).is_rhs_int() {
                g_tag = g_tag.set_rhs_int();
            }
            let fx = engine.make_cell(x_tag, f, x);
            StepResult::CellContents(g_tag, fx, g)
        })
    }

    fn run_plus_comb(&mut self) -> Option<CellPtr> {
        self.run_strict_binop(|a, b| a + b)
    }

    fn run_minus_comb(&mut self) -> Option<CellPtr> {
        self.run_strict_binop(|a, b| a - b)
    }

    fn run_times_comb(&mut self) -> Option<CellPtr> {
        self.run_strict_binop(|a, b| a * b)
    }

    fn run_eq_comb(&mut self) -> Option<CellPtr> {
        self.run_strict_binop(|a, b| if a == b { 1 } else { 0 })
    }

    fn run_lt_comb(&mut self) -> Option<CellPtr> {
        self.run_strict_binop(|a, b| if a < b { 1 } else { 0 })
    }

    fn run_cond_comb(&mut self) -> Option<CellPtr> {
        self.run_comb3(|engine, args| {
            // COND c t f = if(c) t else f
            if let Some(c) = engine.int_rhs(args[0]) {
                let branch_ptr = match c {
                    0 => args[2],
                    1 => args[1],
                    v => {
                        // We expect never to reach this code.
                        // In debug mode we panic, in release the behaviour is undefined.
                        debug_assert!(v == 0 || v == 1, "condition variable should be 0 or 1");
                        unsafe { std::hint::unreachable_unchecked() }
                    }
                };
                let branch_tl = engine.tl(branch_ptr);
                // Check if tl is int
                if engine.tag(branch_ptr).is_rhs_int() {
                    StepResult::Value(branch_tl.0)
                } else {
                    StepResult::Cell(branch_tl)
                }
            } else {
                StepResult::EvaluateArg(engine.tl(args[0]))
            }
        })
    }

    fn run_abort_comb(&mut self) -> Option<CellPtr> {
        panic!("Abort combinator reduced")
    }

    fn run_strict_binop(&mut self, op: fn(i32, i32) -> i32) -> Option<CellPtr> {
        self.run_comb2(|engine, args| {
            let a = engine.int_rhs(args[0]);
            let b = engine.int_rhs(args[1]);

            match (a, b) {
                (Some(a), Some(b)) => StepResult::Value(op(a, b)),
                (Some(_), None) => StepResult::EvaluateArg(engine.tl(args[1])),
                (None, Some(_)) => StepResult::EvaluateArg(engine.tl(args[0])),
                (None, None) => StepResult::EvaluateArg2(engine.tl(args[0]), engine.tl(args[1])),
            }
        })
    }

    fn int_rhs(&mut self, cell_ptr: CellPtr) -> Option<i32> {
        let tag = self.tag(cell_ptr);
        let rhs_ptr = self.tl(cell_ptr);
        if tag.is_rhs_int() {
            return Some(rhs_ptr.0);
        }
        if rhs_ptr.is_comb(self) {
            // rhs points to a combinator
            return None;
        }
        // Check to see if RHS happens to point to an indirection node
        if self.hd(rhs_ptr).is_i_comb() {
            // TODO: Consider making indirection compression a job for I reduction
            // Remove one layer of indirection
            self.set_tag(cell_ptr, self.tag(rhs_ptr));
            self.set_tl(cell_ptr, self.tl(rhs_ptr));
            return self.int_rhs(cell_ptr);
        }
        None
    }

    fn alloc_def(&mut self, def: CompiledDef) {
        let mut tag = Tag::wanted();
        if let CompiledExpr::Int(_) = def.expr {
            tag = tag.set_rhs_int();
        }
        let cell_ptr = self.alloc_compiled_expr(def.expr);
        let def_ptr = *self.def_lookup.get(&def.name).unwrap();
        // Set up the indirection from the definition to the compiled expression
        self.set_tag(def_ptr, tag);
        self.set_tl(def_ptr, cell_ptr);
    }

    fn make_cell<CP0: IntoCellPtr, CP1: IntoCellPtr>(
        &mut self,
        tag: Tag,
        hd: CP0,
        tl: CP1,
    ) -> CellPtr {
        // Search for an unwanted cell
        let mut cell_idx = self.next_cell.0 as usize;
        let hd = hd.into_cell_ptr(self);
        let tl = tl.into_cell_ptr(self);
        while cell_idx >= self.tag.len() || self.tag[cell_idx].is_wanted() {
            cell_idx += 1;

            // We have exhausted the memory
            if cell_idx >= self.tag.len() {
                // Time for gc!
                let initial_queue = if tag.is_rhs_int() {
                    vec![hd]
                } else {
                    vec![hd, tl]
                };
                self.garbage_collect(initial_queue);
                cell_idx = self.comb_impl.len();
                continue;
            }
        }
        let cell_ptr = CellPtr(cell_idx as i32);
        self.set_tag(cell_ptr, tag);
        self.set_hd(cell_ptr, hd);
        self.set_tl(cell_ptr, tl);
        self.next_cell = CellPtr(cell_idx as i32 + 1);
        cell_ptr
    }

    // Simple mark and sweep garbage collect
    fn garbage_collect(&mut self, mut queue: Vec<CellPtr>) {
        // Phase 1: Mark everything unwanted
        for t in self.tag.iter_mut().skip(self.comb_impl.len()) {
            *t = t.set_unwanted();
        }

        // Phase 2: Start at the stack and mark everything reachable from it
        queue.extend(&self.stack);
        let mut cells_wanted = 0;
        while let Some(cell_ptr) = queue.pop() {
            // Skip combinators
            if cell_ptr.is_comb(self) {
                continue;
            }
            // Skip if already marked
            let tag = self.tag(cell_ptr);
            let visited = tag.is_wanted();
            if visited {
                continue;
            }

            // Mark this cell as wanted
            self.set_tag(cell_ptr, self.tag(cell_ptr).set_wanted());
            cells_wanted += 1;

            // Check children
            let hd = self.hd(cell_ptr);
            if !hd.is_comb(self) {
                // hd is ptr
                let hd_visited = self.tag(hd).is_wanted();
                if !hd_visited {
                    queue.push(hd);
                }
            }

            if !tag.is_rhs_int() {
                // tl is a ptr or comb, not int
                let tl = self.tl(cell_ptr);
                if !tl.is_comb(self) {
                    // tl is ptr
                    let tl_visited = self.tag(tl).is_wanted();
                    if !tl_visited {
                        queue.push(tl);
                    }
                }
            }
        }

        if cells_wanted >= CELLS {
            panic!("Out of memory!")
        }
        // Reset next cell pointer
        self.next_cell = CellPtr(self.comb_impl.len() as i32);
    }

    fn alloc_compiled_expr(&mut self, expr: CompiledExpr) -> CellPtr {
        match expr {
            CompiledExpr::Comb(Comb::I) => COMB_I,
            CompiledExpr::Comb(c) => c.into_cell_ptr(self),
            CompiledExpr::Var(s) => self
                .def_lookup
                .get(&s)
                .copied()
                .expect(&format!("Missing definition for {:?}", s)),
            CompiledExpr::Ap(l, r) => {
                let mut tag = Tag::wanted();
                if let CompiledExpr::Int(_) = *r {
                    tag = tag.set_rhs_int();
                }
                let l = self.alloc_compiled_expr(*l);
                let r = self.alloc_compiled_expr(*r);
                self.make_cell(tag, l, r)
            }
            CompiledExpr::Int(i) => CellPtr(i),
        }
    }

    // In debug mode, checks that this is a valid CellPtr.
    // In release mode it is a no-op.
    fn debug_assert_ptr(&self, ptr: CellPtr) {
        // Responsibility of the caller to
        // check this in release mode.
        // Even if ptr is actually to a combinator,
        // it still points to a valid element in the array,
        // so it is a correctness bug rather than a memory error.
        debug_assert!(!ptr.is_comb(self));
        // Bounds check only in debug mode.
        // We assume CellPtr is never fabricated,
        // so it always contains a valid index.
        debug_assert!(ptr.0 >= 0);
        debug_assert!(
            (ptr.0 as usize) < self.tag.len()
                && (ptr.0 as usize) < self.hd.len()
                && (ptr.0 as usize) < self.tl.len()
        );
    }

    fn set_tag(&mut self, ptr: CellPtr, t: Tag) {
        self.debug_assert_ptr(ptr);
        *unsafe { self.tag.get_unchecked_mut(ptr.0 as usize) } = t;
    }

    fn tag(&self, ptr: CellPtr) -> Tag {
        self.debug_assert_ptr(ptr);
        *unsafe { self.tag.get_unchecked(ptr.0 as usize) }
    }

    fn set_hd<I: IntoCellPtr>(&mut self, ptr: CellPtr, v: I) {
        self.debug_assert_ptr(ptr);
        *unsafe { self.hd.get_unchecked_mut(ptr.0 as usize) } = v.into_cell_ptr(self);
    }

    fn hd(&self, ptr: CellPtr) -> CellPtr {
        self.debug_assert_ptr(ptr);
        *unsafe { self.hd.get_unchecked(ptr.0 as usize) }
    }

    fn set_tl<I: IntoCellPtr>(&mut self, ptr: CellPtr, v: I) {
        self.debug_assert_ptr(ptr);
        *unsafe { self.tl.get_unchecked_mut(ptr.0 as usize) } = v.into_cell_ptr(self);
    }

    fn tl(&self, ptr: CellPtr) -> CellPtr {
        self.debug_assert_ptr(ptr);
        *unsafe { self.tl.get_unchecked(ptr.0 as usize) }
    }

    pub fn get_int(&self, ptr: CellPtr) -> Option<i32> {
        if self.tag(ptr).is_rhs_int() {
            Some(self.tl(ptr).0)
        } else {
            None
        }
    }

    pub fn with_debug(self) -> TurnerEngineDebug {
        let mut comb_inst = Vec::new();
        // Fill with dummies
        for _ in &self.comb_impl {
            comb_inst.push(Comb::Abort);
        }
        // Put in the correct ones using the map
        for (c, i) in self.comb_map.iter() {
            comb_inst[*i] = *c;
        }
        TurnerEngineDebug {
            engine: self,
            dump_path: None,
            step_counter: 0,
            step_limit: None,
            comb_inst,
        }
    }
}

enum StepResult {
    Value(i32),
    Cell(CellPtr),
    CellContents(Tag, CellPtr, CellPtr),
    EvaluateArg(CellPtr),
    EvaluateArg2(CellPtr, CellPtr),
}

/// To avoid slowing down [`TurnerEngine`] in benchmarks, all debugging related routines are in this wrapper.
pub struct TurnerEngineDebug {
    engine: TurnerEngine,
    // The directory to dump debug information to
    dump_path: Option<PathBuf>,
    step_counter: usize,
    step_limit: Option<usize>,
    // Combinator instance lookup table
    comb_inst: Vec<Comb>,
}

impl TurnerEngineDebug {
    fn step(&mut self) -> Option<CellPtr> {
        self.dump_dot().expect("Dump failed");
        self.step_counter += 1;
        if let Some(limit) = self.step_limit {
            if self.step_counter > limit {
                panic!("Max cycle reached");
            }
        }
        self.engine.step()
    }

    pub fn run(&mut self) -> i32 {
        // This is a copy of run under TurnerEngine.
        // TODO: deduplicate?
        self.engine.stack.clear();
        self.engine.stack.push(
            *self
                .engine
                .def_lookup
                .get("main")
                .expect("no main function found"),
        );
        loop {
            if let Some(cell_ptr) = self.step() {
                return self
                    .engine
                    .get_int(cell_ptr)
                    .expect("did not evaluate to int");
            }
        }
    }

    pub fn dump_dot(&mut self) -> std::io::Result<()> {
        let mut w = if let Some(dump_path) = &self.dump_path {
            let f = File::create(dump_path.join(format!("step{}.dot", self.step_counter)))?;
            BufWriter::new(f)
        } else {
            return Ok(());
        };
        // Garbage collect so we don't render dead cells
        self.engine.garbage_collect(vec![]);
        writeln!(w, "digraph {{")?;
        // Nodes
        writeln!(w, "node [shape=record];")?;
        for c in self.comb_inst.len()..CELLS {
            let tag = self.engine.tag[c];
            if !tag.is_wanted() {
                // unwanted
                continue;
            }
            let hd = self.engine.hd[c];
            let hd = if let Some(comb) = hd.comb(&self) {
                format!("{:?}", comb)
            } else {
                String::new()
            };
            let tl = self.engine.tl[c];
            let tl = if tag.is_rhs_int() {
                format!("{}", tl.0)
            } else if let Some(comb) = tl.comb(&self) {
                format!("{:?}", comb)
            } else {
                String::new()
            };
            writeln!(w, "cell{} [label=\"<hd> {}|<tl> {}\"];", c, hd, tl)?;
        }
        // Stack
        if !self.engine.stack.is_empty() {
            write!(w, "stack [pos=\"0,0!\", label=\"{{")?;
            for (i, c) in self.engine.stack.iter().enumerate() {
                if let Some(comb) = c.comb(&self) {
                    write!(w, "<s{}> {:?}", i, comb)?;
                } else {
                    write!(w, "<s{}> ", i)?;
                }
                if i != self.engine.stack.len() - 1 {
                    write!(w, "|")?;
                }
            }
            writeln!(w, "}}\"];")?;
        }

        // Edges
        for c in self.comb_inst.len()..CELLS {
            let tag = self.engine.tag[c];
            if !tag.is_wanted() {
                continue;
            }
            let hd = self.engine.hd[c];
            if !hd.is_comb(&self.engine) {
                writeln!(w, "cell{}:hd -> cell{};", c, hd.0)?;
            }
            let tl = self.engine.tl[c];
            if !tl.is_comb(&self.engine) && !tag.is_rhs_int() {
                writeln!(w, "cell{}:tl -> cell{};", c, tl.0)?;
            }
        }
        // Defs
        for (n, p) in self.engine.def_lookup.iter() {
            writeln!(w, "{} -> cell{}", n, p.0)?;
        }

        // Stack edges
        if !self.engine.stack.is_empty() {
            for (i, c) in self.engine.stack.iter().enumerate() {
                if !c.is_comb(&self.engine) {
                    writeln!(w, "stack:s{} -> cell{}", i, c.0)?;
                }
            }
        }

        writeln!(w, "}}")?;
        Ok(())
    }

    pub fn set_dump_path(&mut self, dump_path: String) {
        let dump_path = PathBuf::from(dump_path);
        std::fs::create_dir_all(&dump_path).expect("failed to create dump directory");
        // Clean up old dump files
        for entry in std::fs::read_dir(&dump_path).unwrap() {
            let entry = entry.unwrap();
            if let Some(name) = entry.file_name().to_str() {
                if name.ends_with(".dot") {
                    let path = dump_path.join(entry.file_name());
                    println!("Deleting file {:?}", path);
                    std::fs::remove_file(path).expect("failed to delete file");
                }
            }
        }
        self.dump_path = Some(dump_path);
    }

    pub fn set_step_limit(&mut self, l: usize) {
        self.step_limit = Some(l);
    }
}
