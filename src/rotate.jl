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
