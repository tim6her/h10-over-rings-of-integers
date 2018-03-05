---
    bibliography: references.bib
    author: Tim B. Herbstrith
    title: Hilbert's 10th problem in rings of algebraic integers
---

# Preliminaries

Before stating Hilbert's 10th problem and proving its undecidability in certain rings of algebraic integers, we remind the reader on some notions of theoretical computer science and number theory, as well as fix some notations.

## Preliminaries from theoretical computer science

### Problems and Turing machines

In this section we closely follow the lecture notes on the subject by @Mueller2016.

<div class="Definition">
A _(decision) problem_ is a subset of the set of finite $\zer$-$\one$-strings $\lbrace \zer, \one \rbrace^*$ including the empty string $λ$.
We call $\lbrace \zer, \one \rbrace$ _alphabet_ and its elements _bits._
</div>

One immediate objection against this definition is that not all problems arise as subsets of these strings.
However, such problems $Q$ are captured up to an encoding.

$$ \ulcorner . \urcorner : Q → \lbrace \zer, \one \rbrace^*$$

We usually do not concern ourselves with the details of this encoding.
However, the encoding should capture the structure of the problem.

<div class="Example">
Consider the problem of deciding wether a finite simple graph is connected or not.
If we fix each vertex set to be of the form $\lbrace 0, 1, …, n\rbrace$, then the set of all graphs can be encoded as the set of the respective adjacency matrices written as a string

$$b_{00}b_{01} …b_{0n}b_{10}…b_{nn}$$

of length $(n + 1)^2$, where $b_{ij} = \one$ if and only if the vertices $i$ and $j$ are connected.
</div>

<div class="Definition">
A _Turing machine_ $\mathbb A$ on the _alphabet_ $A =  \lbrace \sta, \emp, \zer, \one \rbrace$ is a tuple $(S, δ)$, where $\sstart, \shalt ∈ S$ is a finite non-empty set, called _set of states_, and

$$δ: S × A → S × A × \lbrace -1, 0, 1 \rbrace$$

is called _transition function_. If $δ(s, a) = (s', b, m)$, we demand that the following axioms are satisfied.

1. $a = \sta$ if and only if $b = \sta$.
2. If $a = \sta$, then $m ≠ -1$.
3. If $s = \shalt$, then $s' = \shalt$, $a = b$ and $m = 0$.
</div>

Let $\mathbb A = (S, δ)$ be a Turing machine. A _configuration_ of $\mathbb A$ is a triple $(s, j, c) ∈ S × ℕ × A^ℕ$. It reflects the current state of $\mathbb A$ the current position of its _head_ and the content of its _work-tape_.

A configuration of the form $(\shalt, 0, c)$ is called _halting_.
A _start configuration_ is of the form $(\sstart, 0, c)$ such that $c(0) = \sta$ and there exists an $n ∈ ℕ$ such that $c(i) = \square$ if and only if $i > n$.
This means that in a start configuration the work-tape reads

$$\sta x_1 x_2 … x_n \square \square …$$

It will be very convenient to identify the finite string $x_1…x_n$ with this tape content.

We write $(s, j, c) \vdash_1 (s', j', c')$ and call $(s', j', c')$ a _successor configuration_ of $(s, j, c)$ if there exists an $m ∈ \lbrace -1, 0, 1 \rbrace$ such that

1. $δ(s, c) = (s', c', m)$,
2. $j' = j + m$, and
3. $c'(ℓ) = c(ℓ)$ for all $ℓ ≠ j$.

This relation makes the set of all configurations of $\mathbb A$ into a directed graph.
A _run_ of $\mathbb A$ on $x$ is a path in this directed graph starting in the start configuration $(\sstart, 0, x)$.
A run of $\mathbb A$ on $x$ is _halting_ or _complete_ if it reaches a halting configuration $(\shalt, 0, y)$.
In this case we write $\mathbb A (x) = y$.

We will denote Turing machines using listings, where the fact that $δ_\text{a} (s_\text{state}, b) = (s_\text{state'}, c, m)$ is encoded by

``` haskell
a "state" b = ("state'", c, m)
```

See the Appendix of this thesis on how to simulate these Turing machines using the _Haskell_ programming language.
We actually use valid syntax of the _glorious Glasgow Haskell compiler_ in the listings.

<div class="Example">
Consider the Turing machine $\mathbb A_\text{add1} = (\lbrace \sstart, \shalt, s_\text{overflow}, s_\text{return} \rbrace, δ_\text{add1})$ that adds $1$ to a (possible zero-patched) binary representation of a natural number $n$.


``` haskell
-- start by entering the "overflow" state ...
add1 "start"    '§' = ("overflow", '§', 1   )
-- ... and stay in this state, as long as you read only '1'-s
add1 "overflow" '1' = ("overflow", '0', 1   )
-- if you read the first '0' or an empty cell replace it by '1'
-- and enter the "return" state to move the head to the first cell
add1 "overflow" '0' = ("return",   '1', (-1))
add1 "overflow" '□' = ("return",   '1', (-1))
-- we finish if we read '§' again or ...
add1 "return"   '§' = ("halt",     '§', 0   )
-- ... continue to move to the right and don't change the cell
-- content. Here `b` matches '0' or '1'
add1 "return"   b   = ("return",   b  , (-1))

```

The complete run of $\mathbb A_\text{add1}$ on $\one\one\zer\one$ cas be seen in [@fig:add1].

<div id="fig:add1">

![$δ(\sstart, \sta) = (s_\text{overflow}, \sta, 1)$](res/turing_add1_1.svg)

![$δ(s_\text{overflow}, \one) = (s_\text{overflow}, \one, 1)$](res/turing_add1_2.svg)

![$δ(s_\text{overflow}, \one) = (s_\text{overflow}, \one, 1)$](res/turing_add1_3.svg)

![$δ(s_\text{overflow}, \zer) = (s_\text{return}, \one, -1)$](res/turing_add1_4.svg)

![$δ(s_\text{return}, \one) = (s_\text{return}, \one, -1)$](res/turing_add1_5.svg)

![$δ(s_\text{return}, \one) = (s_\text{return}, \one, -1)$](res/turing_add1_6.svg)

![$δ(s_\text{return}, \sta) = (s_\text{halt}, \sta, 0)$](res/turing_add1_7.svg)

![$δ(s_\text{halt}, \sta) = (s_\text{halt}, \sta, 0)$](res/turing_add1_8.svg)

The complete run of $\mathbb A_\text{add1}$ on $\one\one\zer\one$
</div>
</div>

<div class="Definition">
Let $\mathbb A$ be a Turing machine.

1. $\mathbb A$ _computes_ the partial function that maps each $x$ with a complete run to $\mathbb A(x)$ and is undefined for all other strings.
2. $\mathbb A$ _accepts_ all $x$ such that $\mathbb A(x) = \one$ and _rejects_ them if $\mathbb A(x) = \zer$.
3. A partial function on $\lbrace \sta, \emp, \zer, \one \rbrace^*$ is _computable_ if there is a Turing machine computing it.
4. A subset of $\lbrace \sta, \emp, \zer, \one \rbrace^*$, i.e. a problem, is _decidable_ if there is a Turing machine computing its characteristic function.
5. A problem is called _semi-decidable_ or _computably enumerable_ if there is a Turing machine accepting precisely the elements of the problem.
</div>

The last postulate of the definition above means that a problem is semi-decidable if there is a Turing machine affirming membership of the corresponding set but it might not be able to refute membership.

<div class="Example">
We can encode a natural number $n$

1. in tally notation
   $$n ↦ \underbrace{\one…\one}_{n\text{-times}},$$
2. by its binary representation
   $$n = 2^k + \sum_{i = 0}^{k-1} b_i 2^i ↦ b_0…b_{k-1}\one,$$
   $$0 ↦ \zer \text{, or}$$
3. by a shifted and truncated form of its binary representation
   $$n = 1 + \sum_{i = 0}^k b_i 2^i ↦ b_0…b_{k-1},$$
   $$0 ↦ λ$$

In either case the set obtained by encoding $ℕ$ is easily seen to be decidable.
In the first case, check that the string contains only copies of the bit $\one$.

This can be achieved by the Turing machine $\mathbb A_\text{tally} = ( \lbrace \sstart, \shalt, \scheck, s_\text{accept}, s_\text{reject},  s_\text{rejectMR}\rbrace, δ)$ with

``` haskell
-- start by entering the "check" state and ...
tally "start"    '§'    = ("check",    '§', 1   )
-- ... stay in this state while reading only '1'-s
tally "check"    '1'    = ("check",    '1', 1   )
-- on reading '□' accept the input and clear the tape ...
tally "check"    '□'    = ("accept",   '□', (-1))
tally "accept"   '1'    = ("accept",   '□', (-1))
-- ...except for cell c(1) where you write a '1'
tally "accept"   '§'    = ("accept",   '§', 1   )
tally "accept"   '□'    = ("halt",     '1', (-1))
-- however, if you read a '0' first reject the input
-- by moving to the end of the input string ...
tally "check"    '0'    = ("rejectMR", '0', 1   )
tally "rejectMR" '□'    = ("reject",   '□', (-1))
tally "rejectMR" b      = ("rejectMR", b,   1   ) -- `b` is '0' or '1'
-- ... and clear the tape except for cell c(1) where you
-- write a '0'
tally "reject"   '§'    = ("reject",   '§', 1   )
tally "reject"   '□'    = ("halt",     '0', (-1))
tally "reject"   b      = ("reject",   '□', (-1)) -- `b` is '0' or '1'
```

In the second case it suffices to check that the string has length $1$ or ends in a $\one$, and in the third case every string is accepted.
</div>

### Halting problem

One can extend the alphabet of Turing machines by encoding characters i.e. assigning bit sequences to them.
Very common encodings are _ASCII_ and _UTF-8_[^dee0aaaa].
Using either one of these encodings the string 'haskell' is encoded as

```
h         a         s         k         e         l         l
0110 1000 0110 0001 0111 0011 0110 1011 0110 0101 0110 1100 0110 1100
```

In this way we can encode a Turing machine as the UTF-8–encoding of their transition function $δ$ written as a string as above.
Note that the set of states can implicitly be deduced from the transition function as the set of acceptable first arguments of $δ$.

One of the fundamental theorems of theoretical computer science is the existence of a universal Turing machine.

<div class="Theorem">
There exists a Turing machine $\mathbb U$ that computes upon receiving the tuple $(\ulcorner \mathbb A \urcorner, x)$ as input, the output of Turing machine $\mathbb A$ on $x$ i.e.
$$ \mathbb U(\ulcorner \mathbb A \urcorner, x) = y \Leftrightarrow \mathbb A (x) = y$$
</div>

A natural question is:

<div class="Example" title="Halting problem">
Given a machine $\mathbb A$ and a string $x$.
Does $\mathbb A$ halt on $x$?
</div>

It is one of the most fundamental results of theoretical computer science that the halting problem is unsolvable.

<div class="Theorem">
The halting problem is undecidable.
</div>

<div class="proof">
Assume there exists a Turing machine $\mathbb B$ that decides the halting problem i.e. for all Turing machines $\mathbb A$ and all strings $x$

$$ \mathbb B(\mathbb A, x) =
\begin{cases}
  \one  & \text{if } \mathbb A \text{ halts on } x\\
  \zer  & \text{if } \mathbb A \text{ does not halt on } x
\end{cases}$$

Now using $\mathbb B$ construct a Turing machine $\mathbb B'$ such that

$$ \mathbb B'(\mathbb A) = \one \Leftrightarrow \mathbb A \text{ does not halt on } \ulcorner \mathbb A\urcorner $$

Now if $\mathbb B'(\mathbb B, \ulcorner \mathbb B\urcorner) = \one$ then by definition of $\mathbb B'$ we find that $\mathbb B  does not halt on  \ulcorner \mathbb B\urcorner$.
But by definiton of $\mathbb B$ this means that $\mathbb B(\mathbb B, \ulcorner \mathbb B \urcorner) = \zer$
</div>

<!-- TODO: A section on computable rings -->

[^dee0aaaa]: See the Appendix for an _ASCII_ encoding table and details on _UTF-8_.
