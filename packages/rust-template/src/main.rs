#[allow(dead_code)]
fn run_tests() {
    let x = 1 + 1;
    assert_eq!(x, 2);
}
const fn main() {}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_all() {
        run_tests();
    }
}
