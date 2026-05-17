#[allow(dead_code)]
fn run_self_tests() {
    let x = 1 + 1;
    assert_eq!(x, 2);
}
const fn main() {}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_run_self_tests() {
        run_self_tests();
    }
}
