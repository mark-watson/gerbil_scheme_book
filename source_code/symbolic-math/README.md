# Symbolic Math: Polynomial Differentiation and Integration

Symbolic calculus on single-variable polynomials in pure Gerbil Scheme, ported from the Common Lisp `symbolic-math` example in *Loving Common Lisp*. It needs only the Gerbil standard library.

Symbolic math has been a classic Lisp application since the 1960s. This example is a small, readable version of it. Gambit's numeric tower keeps coefficients as **exact rationals**, so the antiderivative of `-x` is printed as `-1/2x^2` instead of `-0.5x^2`, and the integral of `3x^2 - x + 5` from 0 to 1 comes out as exactly `11/2`.

## Files

| File | Description |
|------|-------------|
| `data.ss` | Data types (`defstruct`) for variables, constants, terms, polynomials, and integrals, plus polynomial arithmetic, evaluation, and printing |
| `differentiation.ss` | Power rule, n-th derivatives, gradient, critical-point test |
| `integration.ss` | Reverse power rule, n-th antiderivatives, definite integrals (Fundamental Theorem of Calculus) |
| `test.ss` | Smoke tests for all three modules |

## Running

```bash
make test     # or: gxi test.ss
make build    # compile with gxc
```

## Example

```scheme
(import "data" "differentiation" "integration")

(def x (make-variable 'x))
(def p (make-polynomial x (list (make-term 3 x 2)      ; 3x^2
                                (make-term -1 x 1)     ; -x
                                (make-term 5 x 0))))   ; +5

(polynomial->string (differentiate p))   ; => "6x + -1"
(polynomial->string (integrate p))       ; => "1x^3 + -1/2x^2 + 5x"
(evaluate-definite p 0 1)                ; => 11/2
(evaluate-definite p 0 (make-constant 'pi 'pi))  ; => 41.779...
(critical-point? p 1/6)                  ; => #t
```

## API Summary

**Data layer (`data.ss`)**
- `(make-variable name domain: 'real)`, `(make-constant name value)` where `value` is a number, `'pi` or `'e`
- `(make-term coefficient variable exponent)`
- `(make-polynomial variable terms domain: 'real)` combines like terms, drops zero terms, and sorts by descending exponent
- `polynomial-add`, `polynomial-subtract`, `polynomial-negate`, `polynomial-scale`
- `polynomial-evaluate`, `polynomial-degree`, `polynomial->string`
- `(make-integral integrand variable lower: #f upper: #f)`, `integral->string`

**Differentiation (`differentiation.ss`)**
- `differentiate`, `differentiate-term`, `(differentiate-n poly n)`
- `(gradient-at poly value)`, `(critical-point? poly value tolerance: 1e-9)`

**Integration (`integration.ss`)**
- `integrate`, `integrate-term`, `(integrate-n poly n)`
- `(evaluate-definite poly lower upper)`, `(evaluate-integral integral)`
- `make-indefinite-integral`, `make-definite-integral`

## Porting Notes (Common Lisp to Gerbil)

- CL `defstruct` became Gerbil `defstruct`. Gerbil generates positional constructors such as `make-sym-term`, so thin wrapper functions such as `make-term` add the argument validation that CL did with `check-type`.
- CL keyword symbols (`:real`, `:pi`) became plain quoted symbols (`'real`, `'pi`).
- CL predicates ending in `-p` were renamed to the Scheme `?` convention (`critical-point?`, `integral-definite?`).
- `(remove nil (mapcar ...))` became `filter-map`.
