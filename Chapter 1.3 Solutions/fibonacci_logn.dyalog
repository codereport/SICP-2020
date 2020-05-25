
⍝ This is a super hacky solution that only works for even numbers :P
⍝ Godbolt link to modified Ben Deane C++ code: https://www.godbolt.org/z/M8yY8G

rowToMat ← {2 2⍴(+/⍵),(⊃⍵),⍵}
fibMult  ← {⍺+.×rowToMat⍵}
fibSq    ← {⍵fibMult⍵}

nthFibonacci ← {
    x ← ⌊⍵÷2∘*⍳20
    y ← (x>0)/x
    n ← ≢ y
    ⊃⊃fibMult/(2|y)/{(fibSq⍣⍵)1 0}¨⍳n
}

⍝ nthFibonacci 70
⍝    1.903924907E14
