// https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=d98c05f42dc7e653a3513493d5ab8600

fn fact(n: i32) -> i32 {
    return (1..n).fold(1, |acc, n| acc * n);
}

fn main() {
    println!("{:?}", fact(10));
}
