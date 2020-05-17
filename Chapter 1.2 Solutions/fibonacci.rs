// https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=334c850c417dd1b33059a7ee2855a339

fn fib(n: i32) -> i32 {
    (1..n-1).fold((1, 1), |(a, b), _unused| (b, a + b)).1
}

fn main() {
    println!("{:?}", fib(10));
}
