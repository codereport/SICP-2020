// https://godbolt.org/z/paNVtU

#include <iostream>
#include <vector>
#include <numeric>

auto fib(int n) -> int {
    std::vector<int> v(n-2);
    return std::accumulate(v.cbegin(), v.cend(), 
        std::pair{1, 1},
        [](auto pair, auto unused) { 
            auto [a, b] = pair;    
            return std::pair{b, a + b}; 
        }).second;
}

int main () {
    std::cout << fib(10) << '\n';
    return 0;
}
