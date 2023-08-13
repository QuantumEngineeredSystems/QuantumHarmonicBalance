using QuantumHarmonicBalance
using QuantumCumulants; QC = QuantumCumulants
using ModelingToolkit, OrdinaryDiffEq, Plots, SymbolicUtils, Symbolics

# Hilbert space
h = FockSpace(:cavity)

# Parameter
@cnumbers Δ F κ ω ω₀ t
@qnumbers a::Destroy(h)


function perform_hadamard(X, Y; cut=10)
    out = 1
    term_prev = 0
    for i in 0:cut
        term = i==0 ? Y : commutator(X, term_prev)
        term = simplify(term/factorial(i))
        out += term
    end
    out
end
function rotate(H, a; direction=:left, ω=cnumber(:ω))
    dir_bool =  direction == :left ? true : false
    X = (-1)^dir_bool*im*ω*a'*a
    h = perform_hadamard(X, H)
    simplify(h - im*X)
end

simplify(commutator(-im*ω*a'*a, a'+a))


H = ω₀*a'*a
H= - F*(a' + a)*(exp(im*ω*t) + exp(-im*ω*t))
Hrot = rotate(H, a)
simplify(Hrot)
