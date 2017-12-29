---
    bibliography: references.bib
    author: Tim B. Herbstrith
    title: Hilbert's 10th problem in rings of algebraic integers
---

# Some structural results

Before tackling __H10__ in selected number fields, we list some structural results and methods used within the subsequent proofs.

<div class="Proposition">
Let $R$ be a Dedekind domain, whose quotient field is not algebraically closed. Then if $S_1, S_2 ∈ R$ are diophantine so are

\[ S_1 ∩ S_2 \text{ and } S_1 ∪ S_2 \]
</div>

<div class="Proof">
Let $f(X, Y_1,…, Y_n)$ and $g(X, Y_1,…, Y_n)$ give diophantine definitions[^8cee5683] of $S_1$ and $S_2$ resp. then

$$ h := fg $$

vanishes if and only if $f$ or $g$ vanishes. As a consequence,

$$ S_1 ∪ S_2 = \lbrace x \mid ∃ y_1, … , ∃ y_n \; h(x, y_1, … , y_n) = 0 \rbrace. $$
</div>



[^8cee5683]: By inserting dummy indeterminates we may whish that both polynomials have the same arity.
