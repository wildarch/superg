pub fn assemble_call(off: i32) -> [u8; 5] {
    let mut buf = [0u8; 5];
    // CALL rel32
    buf[0] = 0xE8;
    buf[1..5].copy_from_slice(&off.to_le_bytes());
    buf
}

const PAGE_SIZE: usize = 4096;

pub struct JitMem {
    buf: *mut u8,
    len: usize,
}

impl JitMem {
    pub fn new_at(ptr: *mut u8, len: usize) -> std::io::Result<JitMem> {
        // Allow RWX, everything!
        let prot = libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC;
        let flags = libc::MAP_ANONYMOUS | libc::MAP_PRIVATE | libc::MAP_FIXED_NOREPLACE;
        // Unused because the mmap is anonymous
        let fd = -1;
        let offset = 0;
        let buf = unsafe {
            libc::mmap(ptr as *mut libc::c_void, len, prot, flags, fd, offset) as *mut u8
        };
        if buf == libc::MAP_FAILED as *mut u8 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(JitMem { buf, len })
        }
    }

    pub fn new_in_range(min_ptr: *mut u8, max_ptr: *mut u8, len: usize) -> std::io::Result<JitMem> {
        // Check inputs
        assert_eq!(
            min_ptr as usize % PAGE_SIZE,
            0,
            "min_ptr must be aligned to {}",
            PAGE_SIZE
        );
        assert_eq!(
            max_ptr as usize % PAGE_SIZE,
            0,
            "max_ptr must be aligned to {}",
            PAGE_SIZE
        );
        assert!(min_ptr < max_ptr, "min_ptr must be less than max_ptr");

        let mut ptr = min_ptr;
        loop {
            match JitMem::new_at(ptr, len) {
                Ok(jit) => return Ok(jit),
                Err(e) => {
                    // Try next page
                    ptr = ptr.wrapping_add(PAGE_SIZE);
                    if ptr >= max_ptr {
                        // Out of pages to try
                        return Err(e);
                    }
                }
            }
        }
    }

    pub fn slice_mut(&mut self) -> &mut [u8] {
        unsafe { std::slice::from_raw_parts_mut(self.buf, self.len) }
    }

    pub fn as_fn<R>(&self, offset: usize) -> unsafe extern "C" fn() -> R {
        unsafe { std::mem::transmute(self.buf.wrapping_add(offset)) }
    }

    pub fn ptr_at<R>(&self, offset: usize) -> *const R {
        self.buf.wrapping_add(offset) as *const R
    }
}

impl core::ops::Drop for JitMem {
    fn drop(&mut self) {
        let res = unsafe { libc::munmap(self.buf as *mut libc::c_void, self.len) };
        if res != 0 {
            panic!(
                "munmap({:?}, {}) failed: {}",
                self.buf,
                self.len,
                std::io::Error::last_os_error()
            );
        }
    }
}

pub struct JitPlacer {
    min_target: usize,
    max_target: usize,
}

impl JitPlacer {
    pub fn new() -> JitPlacer {
        JitPlacer {
            min_target: usize::MAX,
            max_target: usize::MIN,
        }
    }

    pub fn add_target(&mut self, target: usize) {
        self.min_target = std::cmp::min(self.min_target, target);
        self.max_target = std::cmp::max(self.max_target, target);
    }

    pub fn place_jit(&self, len: usize) -> std::io::Result<JitMem> {
        // Must be able to reach min_target from the end of the allocated memory
        // (the biggest possible backward jump).
        // We must have: min_target - (ptr + len) > i32::MIN
        let max_ptr = self.min_target.saturating_add(i32::MAX as usize - len);

        // Must be able to reach max_target from the start of the allocated memory
        // (the biggest possible forward jump).
        // We must have: max_target - ptr < i32::MAX
        let min_ptr = self.max_target.saturating_sub(i32::MAX as usize);

        // Align to page boundary
        let min_ptr = (min_ptr / PAGE_SIZE) * PAGE_SIZE + PAGE_SIZE;
        let max_ptr = (max_ptr / PAGE_SIZE) * PAGE_SIZE;

        JitMem::new_in_range(min_ptr as *mut u8, max_ptr as *mut u8, len)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn write_exec() {
        let code = &[
            0x55, //    push   %rbp
            0x48, 0x89, 0xe5, //    mov    %rsp,%rbp
            0xb8, 0x37, 0x00, 0x00, 0x00, //    mov    $0x37,%eax
            0xc9, //    leaveq
            0xc3, //    retq
        ];
        let jit_placer = JitPlacer::new();
        let mut jit_mem = jit_placer.place_jit(code.len()).unwrap();
        jit_mem.slice_mut().copy_from_slice(code);
        let func = jit_mem.as_fn(0);

        let res: usize = unsafe { func() };

        assert_eq!(res, 55);
    }

    #[test]
    fn call_subroutine() {
        unsafe extern "C" fn target_func() -> u32 {
            42
        }

        let mut jit_placer = JitPlacer::new();
        jit_placer.add_target(target_func as usize);
        let mut jit_mem = jit_placer.place_jit(6).unwrap();

        const CALL_LEN: usize = 5;
        // Setup jump address
        let base_addr = jit_mem.ptr_at(CALL_LEN);
        println!("base at {:?}", base_addr);
        let target_addr = target_func as *const u8;
        println!("target at {:?}", target_addr);

        let offset = unsafe { target_addr.offset_from(base_addr) };
        println!("offset: {} (min: {}, max: {})", offset, i32::MIN, i32::MAX);

        // Encode the jump target
        let offset: i32 = offset.try_into().unwrap();
        jit_mem.slice_mut()[0..5].copy_from_slice(&assemble_call(offset));
        jit_mem.slice_mut()[5] = RET;

        let func = jit_mem.as_fn(0);

        let res: usize = unsafe { func() };
        assert_eq!(res, 42);
    }

    // Opcode for return
    const RET: u8 = 0xC3;
}
