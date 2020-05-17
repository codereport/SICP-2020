// https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=dde1720a26e2530d29944c3427480518

fn fact(n: i32) -> i32 {
    (1..n+1).fold(1, |acc, n| acc * n)
}

fn main() {
    println!("{:?}", fact(10));
}
