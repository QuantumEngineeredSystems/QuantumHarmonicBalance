import SymbolicUtils: quick_cancel;
using SymbolicUtils: Postwalk, @compactified
using SymbolicUtils: Term, Add, Div, Mul, Pow, Sym, BasicSymbolic
using SymbolicUtils: isterm, ispow, isadd, isdiv, ismul, issym
using Symbolics: unwrap, get_variables, substitute, simplify
using SymbolicUtils: add_with_div, frac_similarterm

# Taken from Symbolics v5.28
"""
filterchildren(c, x)
Returns all parts of `x` that fufills the condition given in c. c can be a function or an expression.
If it is a function, returns everything for which the function is `true`. If c is an expression, returns
all expressions that matches it.

Examples:
```julia
@syms x
Symbolics.filterchildren(x, log(x) + x + 1)
```
returns `[x, x]`

```julia
@variables t X(t)
D = Differential(t)
Symbolics.filterchildren(Symbolics.is_derivative, X + D(X) + D(X^2))
```
returns `[Differential(t)(X(t)^2), Differential(t)(X(t))]`
"""
filterchildren(r, y) = filterchildren!(r, y, [])

function filterchildren!(r::Any, y, acc)
    y = unwrap(y)
    r = unwrap(r)
    if isequal(r, y)
        push!(acc, y)
        return acc
    elseif r isa Function
        if r(y)
            push!(acc, y)
            return acc
        end
    end

    if istree(y)
        if isequal(r, operation(y))
            push!(acc, operation(y))
        elseif r isa Function && r(operation(y))
            push!(acc, operation(y))
        end
        foreach(c->filterchildren!(r, c, acc),
                arguments(y))
        return acc
    end
end

# change SymbolicUtils' quick_cancel to simplify powers of fractions correctly
function quick_cancel(x::Term, y::Term)
	if x.f == exp && y.f == exp
		return exp(x.arguments[1] - y.arguments[1]), 1
	else
		return x,y
	end
end

quick_cancel(x::Term, y::Pow) = y.base isa Term && y.base.f == exp ? quick_cancel(x, expand_exp_power(y)) : x,y

"Returns true if expr is an exponential"
is_exp(expr) = isterm(expr) && expr.f == exp

"Expand powers of exponential such that exp(x)^n => exp(x*n) "
expand_exp_power(expr) = ispow(expr) && is_exp(expr.base) ? exp(expr.base.arguments[1] * expr.exp) : expr
expand_exp_power_add(expr) = sum([expand_exp_power(arg) for arg in arguments(expr)])
expand_exp_power_mul(expr) = prod([expand_exp_power(arg) for arg in arguments(expr)])

function expand_exp_power(expr::BasicSymbolic)
    if isadd(expr)
        return expand_exp_power_add(expr)
    elseif ismul(expr)
        return expand_exp_power_mul(expr)
    else
        return ispow(expr) && is_exp(expr.base) ? exp(expr.base.arguments[1] * expr.exp) : expr
    end
end

# "Expands using SymbolicUtils.expand and expand_exp_power (changes exp(x)^n to exp(x*n)"
expand_all(x) = Postwalk(expand_exp_power)(SymbolicUtils.expand(x))

# "Return all the terms contained in `x`"
get_all_terms(x::BasicSymbolic) = unique(_get_all_terms(x))

_get_all_terms_mul(x) = SymbolicUtils.arguments(x)
_get_all_terms_div(x) = [_get_all_terms(x.num)..., _get_all_terms(x.den)...]
function _get_all_terms_add(x)::Vector
    list = []
    for term in keys(x.dict)
        list = cat(list, _get_all_terms(term), dims = 1)
    end
    return list
end
_get_all_terms(x) = x

function _get_all_terms(x::BasicSymbolic)
    if isadd(x)
        return _get_all_terms_add(x)
    elseif ismul(x)
        return _get_all_terms_mul(x)
    elseif isdiv(x)
        return _get_all_terms_div(x)
    else
        return x
    end
end

# "Convert all sin/cos terms in `x` into exponentials."
function trig_to_exp(x::BasicSymbolic)
    all_terms = get_all_terms(x)
    trigs = filter(z -> is_trig(z), all_terms)

    rules = []
    for trig in trigs
        is_pow = ispow(trig) # trig is either a trig or a power of a trig
        power = is_pow ? trig.exp : 1
        arg = is_pow ? arguments(trig.base)[1] : arguments(trig)[1]
        type = is_pow ? operation(trig.base) : operation(trig)

        if type == cos
            term = (exp(im * arg) + exp(-im * arg))^power * (1 // 2)^power
        elseif type == sin
            term = (1 * im^power) * ((exp(-im * arg) - exp(im * arg)))^power *
                   (1 // 2)^power
        end

        append!(rules, [trig => term])
    end
    result = substitute(x, Dict(rules))
    return simplify(expand_all(result))
end

"Return true if `f` is a function of `var`."
is_function(f, var) = any(isequal.(get_variables(f), var))

"Return true if `f` is a sin or cos."
function is_trig(f::BasicSymbolic)
    f = ispow(f) ? f.base : f
    isterm(f) && SymbolicUtils.operation(f) ∈ [cos, sin] && return true
    return false
end
