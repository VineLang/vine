const RED_ZONE_SIZE: usize = 32 * 1024; // 32KB
const MIN_REALLOC_SIZE: usize = 1024 * 1024; // 1MB

/// Grows the stack on demand to prevent stack overflow. Has a little overhead.
#[inline]
pub fn ensure_sufficient_stack<T>(f: impl FnOnce() -> T) -> T {
  stacker::maybe_grow(RED_ZONE_SIZE, MIN_REALLOC_SIZE, f)
}
