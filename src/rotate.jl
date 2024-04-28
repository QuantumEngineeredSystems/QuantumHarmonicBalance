using QuantumCumulants: check_hilbert, QMul, QSym, Create, Destroy, simplify, QAdd

function rotate(
        input::QMul, a::QSym, ω = cnumber(:ω), t = rnumber(:t); extra_term = true)::QTerm
    out = 1.0
    for arg in input.args_nc
        if arg isa Create
            out *= exp(0 - im * ω * t) * arg
        elseif arg isa Destroy
            out *= exp(0 + im * ω * t) * arg
        else
            error("Unknown type: $(typeof(arg))")
        end
    end
    extra_term ? input.arg_c * simplify(out) - ω * a' * a : input.arg_c * simplify(out)
end
function rotate(input::QAdd, a::QSym, ω = cnumber(:ω), t = rnumber(:t); extra_term = true)
    sum(arg -> rotate(arg, a, ω, t; extra_term = false), input.arguments) - ω * a' * a
end
function rotate(in::Create, a::QSym, ω = cnumber(:ω), t = rnumber(:t); extra_term = true)
    extra_term ? exp(0 - im * ω * t) * in - ω * a' * a : exp(0 - im * ω * t) * in
end
function rotate(in::Destroy, a::QSym, ω = cnumber(:ω), t = rnumber(:t); extra_term = true)
    extra_term ? exp(0 + im * ω * t) * in - ω * a' * a : exp(0 + im * ω * t) * in
end
function rotate(input::Number, a::QSym, ω = cnumber(:ω), t = rnumber(:t); extra_term = true)
    extra_term ? input - ω * a' * a : input
end
