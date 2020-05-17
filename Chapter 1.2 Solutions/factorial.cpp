// https://godbolt.org/z/4nhjHj

#include <iostream>
#include <vector>
#include <numeric>

auto fact(int n) -> int {
    std::vector<int> v(n);
    std::iota(v.begin(), v.end(), 1);
    return std::accumulate(v.cbegin(), v.cend(), 1,
        [](auto acc, auto n) { return acc * n; });
}

int main () {    
    std::cout << fact(10) << '\n';
    return 0;
}
