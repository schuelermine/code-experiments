fn main() {
    let f = |f: fn(fn(fn())) -> fn(fn())| {
        f(|g| match |f: fn(fn())| f(g) {
            f => f(|f| f()),
        })(|| {})
    };
    f(
        match |f: fn(fn(fn())) -> fn(fn(fn())) -> fn(fn())| f(|f| f()) {
            f => f(|_| |_| |f| f()),
        },
    )
}
