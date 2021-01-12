#### Links

* [Advent of Code 2020 in APL!](https://youtu.be/0RQFW6P1Tt0)
* [Category Theory Bartosz Milewski Playlist](https://www.youtube.com/playlist?list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_)
* [Fantasy Birds](https://github.com/fantasyland/fantasy-birds)
* [Angelfire Combinator Birds](https://www.angelfire.com/tx4/cus/combinator/birds.html)
* [Functional JS - Pointfree and Combinators](https://www.youtube.com/watch?v=ZS2_X-ZjFBI)
* [Dyalog'19: Tacit Techniques with Dyalog version 18.0 Operators](https://www.youtube.com/watch?v=czWC4tjwzOQ)
* [Factor Combinators](https://elasticdog.com/2008/12/beginning-factor-shufflers-and-combinators/)
* [To Mock a Mockingbird: and Other Logic Puzzles](https://www.amazon.ca/Mock-Mockingbird-Other-Logic-Puzzles/dp/0192801422)
* [Panda Series `agg`](https://pandas.pydata.org/docs/reference/api/pandas.Series.agg.html?highlight=agg#pandas.Series.agg)
* [Six Languages Worth Knowing Blog](http://www.elbeno.com/blog/?p=420)
* [Dan Ingalls - Sanskrit and OCR](https://vimeo.com/4714623)
* [Cuis Smalltalk](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev)
* [Dave Abrahams vs Matt Calabrese Twitter thread](https://twitter.com/BarryRevzin/status/1348375647857553417)
* [Conor Hoekstra - Concepts vs Typeclasses vs Traits vs Protocols - Meeting C++ 2020](https://www.youtube.com/watch?v=Qh7QdG5RK9E)
* [Call with David Ungar](https://www.youtube.com/watch?v=8nfrC-YLYqc)
* [Pharo Smalltalk](https://pharo.org/)
* [APL demonstration 1975](https://www.youtube.com/watch?v=_DTpQ4Kk2wA)
* [Combinators ES6 Blog](https://codeburst.io/combinators-82799638c90d)
* [Lambda Calculus - Fundamentals of Lambda Calculus & Functional Programming in JavaScript](https://youtu.be/3VQ382QG-y4)
* [Frontendmasters by Brian Lonsdorf AKA Dr. Boolean on point-free javascript](https://github.com/FrontendMasters/fm-transcripts/tree/master/en-US/2020-03-17-hardcore-js-v2)
* [LYAH - Functors, Applicative Functors and Monoids](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors)
* [Programming with Categories - Lecture 0](https://www.youtube.com/watch?v=NUBEB9QlNCM)
* [Epic Jean Yang PL Twitter thread](https://twitter.com/jeanqasaur/status/1290883041418649600)

#### Code

```dyalog
×/(×⍨∘⊃,⊢)⌽⍳5
```
```hs
twitterFollowers = product 
                 . (flip (:) <*> (^2) . head) 
                 . reverse 
                 . iota                                  
```
```hs
Prelude Control.Applicative Control.Monad> :t (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
Prelude Control.Applicative Control.Monad> :t (<$>)
(<$>) :: Functor f => (a -> b) -> f a -> f b
Prelude Control.Applicative Control.Monad> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
Prelude Control.Applicative Control.Monad> fmap (+1) [1..5]
[2,3,4,5,6]
Prelude Control.Applicative Control.Monad> (+1) <$> [1..5]
[2,3,4,5,6]
Prelude Control.Applicative Control.Monad> [(+1)] <*> [1..5]
[2,3,4,5,6]
Prelude Control.Applicative Control.Monad> [(+1), (+2)] <*> [1..5]
[2,3,4,5,6,3,4,5,6,7]
```
#### Books

* [https://learnyousomeerlang.com/](https://learnyousomeerlang.com/)
* [Purely Functional Data Structures](https://www.amazon.ca/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504)
* [Practical Foundations for Programming Languages](http://www.cs.cmu.edu/~rwh/pfpl/2nded.pdf)
* [Category Theory for Programmers](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/)
* [Thinking Forth](https://www.amazon.ca/Thinking-Forth-Leo-Brodie/dp/0976458705/ref=sr_1_1?dchild=1&keywords=thinking+forth&qid=1610428300&s=books&sr=1-1)

#### Votes

```
3 | PFPL   - ~450 (Free-ish) Lesley(1), Vasanth, Mike 
4 | PFDS   - ~230 Lesley, Manu, Frank, Taylor
2 | FMtoGP - ~250 Manu(1), Geoff(1) (w/ YT 4 journey videos)
1 | TaPL   - ~650 (ML) Cezar 
4 | CTforP - ~473 (Free) Geoff, Jean-Michel(1), Vasanth(1), Conor
1 | EOP    - ~??  (Free)  
1 | T. Forth -    (Free) Ezequiel
```
![image](https://user-images.githubusercontent.com/36027403/103606343-2a0df580-4ee4-11eb-9b4f-422f44f9e1d3.png)
