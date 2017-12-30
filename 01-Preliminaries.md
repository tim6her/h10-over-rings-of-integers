---
    bibliography: references.bib
    author: Tim B. Herbstrith
    title: Hilbert's 10th problem in rings of algebraic integers
---

# Preliminaries

Before stating Hilbert's 10th problem and proving its undecidability in certain rings of algebraic integers, we remind the reader on some notions of theoretical computer science and number theory, as well as fix some notations.

## Preliminaries from theoretical computer science

<div class="Definition">
A _(decision) problem_ is a subset of the set of finite $\zer$-$\one$-strings $\lbrace \zer, \one \rbrace^*$ including the empty string $λ$. We call $\lbrace \zer, \one \rbrace$ _alphabet_ and its elements _bits._
</div>

One immediate objection against this definition is that not all problems arise as subsets of these strings.
However, we can refute this objection by saying that all elements of the sets involved in problems we are interested in can be encoded into such strings.
We usually do not concern our selves with the details of this encoding other than that we demand, that it is _decidable_ whether a given string belongs to a given set—a notion that will be made precise in the following.

<div class="Definition">
A _Turing machine_ $\mathbb A$ on the _alphabet_ $A =  \lbrace \sta, \emp, \zer, \one \rbrace$ is a tuple $(S, δ)$, where $\sstart, \shalt ∈ S$ is a finite non-empty set, called _set of states_, and

$$δ: S \times A \to S \times A \times \lbrace -1, 0, 1 \rbrace$$

is called _transition function_. If $δ(s, a) = (s', b, m)$, we demand that the following axioms are satisfied.

1. $a = \sta$ if and only if $b = \sta$.
2. If $a = \sta$, then $m ≠ -1$.
3. If $s = \shalt$, then $s' = \shalt$, $a = b$ and $m = 0$.
</div>

<div class="Definition">
Let $\mathbb A = (S, δ)$ be a Turing machine. A _configuration_ of $\mathbb A$ is a triple $(s, j, c) ∈ S \times ℕ \times A^ℕ$. It reflects the current state of $\mathbb A$ the current position of its _head_ and the content of its _work-tape_.

A configuration of the form $(\shalt, 0, c)$ is called _halting_.
A _start configuration_ is of the form $(\sstart, 0, c)$ such that $c(0) = \sta$ and it exists an $n ∈ ℕ$ such that $c(i) = \square$ if and only if $i > n$.
This means that in a start configuration the work-tape reads

$$\sta x_1 x_2 … x_n \square \square …$$

We write $(s, j, c) \vdash_1 (s', j', c')$ and call $(s', j', c')$ a _successor configuration_ of $(s, j, c)$ if there exists an $m ∈ \lbrace -1, 0, 1 \rbrace$ such that

1. $δ(s, c) = (s', c', m)$,
2. $j' = j + m$, and
3. $c'(ℓ) = c(ℓ)$ for all $ℓ ≠ j$.

This relation makes the set of all configurations of $\mathbb A$ into a directed graph. A _run_ of $\mathbb A$ on $x$ is a path in this directed graph starting in the start configuration $(\sstart, 0, x)$.
A run of $\mathbb A$ on $x$ is _halting_ if it reaches a halting configuration $(\shalt, 0, y)$.
In this case we write $\mathbb A (x) = y$.
</div>

We will denote Turing machines using listings, where the fact that $δ_\text{a} (s_\text{state}, b) = (s_\text{state'}, c, m)$ is encoded by

``` haskell
a "state" b = ("state'", c, m)
```

See the Appendix of this thesis on how to simulate these Turing machines using the _Haskell_ programming language.

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
-- we finish if we read '§' again
add1 "return"   '§' = ("halt",     '§', 0   )
-- continue to move to the right and don't change the cell contents
add1 "return"   b   = ("return",   b  , (-1))

```

The following series of figures shows a grahpic representation of the run of $\mathbb A_\text{add1}$ on $\sta\one\one\zer\one\square…$
</div>

<div class="Example">
We can encode a natural number $n$

1. in tally notation
   $$n \mapsto \underbrace{\one…\one}_{n\text{-times}},$$
2. by its binary representation
   $$n = 2^k + \sum_{i = 0}^{k-1} b_i 2^i \mapsto b_0…b_{k-1}\one,$$
   $$0 \mapsto \zer \text{, or}$$
3. by a shifted and truncated form of its binary representation
   $$n = 1 + \sum_{i = 0}^k b_i 2^i \mapsto b_0…b_{k-1},$$
   $$0 \mapsto λ$$

In either case the set obtained by encoding $ℕ$ is easily seen to be decidable. In the first case, check that the string contains only copies of the bit $\one$:

This can be achieved by the Turing machine $\mathbb A_\text{tally} = ( \lbrace \sstart, \shalt, \scheck \rbrace, δ)$ with

``` haskell
tally "start"    'S'    = ("check",    'S', 1   )
tally "check"    '1'    = ("check",    '1', 1   )
tally "check"    '_'    = ("accept",   '_', (-1))
tally "accept"   '1'    = ("accept",   '_', (-1))
tally "accept"   'S'    = ("accept",   'S', 1   )
tally "accept"   '_'    = ("halt",     '1', (-1))
tally "check"    '0'    = ("rejectMR", '0', 1   )
tally "rejectMR" '_'    = ("reject",   '_', (-1))
tally "rejectMR" b      = ("rejectMR", b,   1   )
tally "reject"   'S'    = ("reject",   'S', 1   )
tally "reject"   '_'    = ("halt",     '0', (-1))
tally "reject"   b      = ("reject",   '_', (-1))
```

In the second case it suffices to check that the string has length $1$ or ends in a $\one$, and in the third case every string is accepted.
</div>
