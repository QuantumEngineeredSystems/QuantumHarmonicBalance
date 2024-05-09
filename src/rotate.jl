using QuantumCumulants: check_hilbert, QMul, QSym, Create, Destroy, simplify, QAdd,
                        undo_average

function _rotate(
        input::QMul, a::QSym, ω, t; extra_term = true)::QTerm
    out = 1.0
    for arg in input.args_nc
        if arg isa Create
            out *= exp(-im * ω * t) * arg
        elseif arg isa Destroy
            out *= exp(im * ω * t) * arg
        else
            error("Unknown type: $(typeof(arg))")
        end
    end
    extra_term ? input.arg_c * simplify(out) - ω * a' * a : input.arg_c * simplify(out)
end
function _rotate(input::QAdd, a::QSym,ω , t; extra_term = true)
    if extra_term
        return sum(arg -> _rotate(arg, a, ω, t; extra_term = false), input.arguments) -
               ω * a' * a
    else
        return sum(arg -> _rotate(arg, a, ω, t; extra_term = false), input.arguments)
    end
end
function _rotate(in::Create, a::QSym, ω, t; extra_term = true)
    extra_term ? exp(-im * ω * t) * in - ω * a' * a : exp(-im * ω * t) * in
end
function _rotate(in::Destroy, a::QSym,ω , t; extra_term = true)
    extra_term ? exp(im * ω * t) * in - ω * a' * a : exp(im * ω * t) * in
end
function _rotate(input, a::QSym,ω , t; extra_term = true)
    extra_term ? input - ω * a' * a : input
end

function rotate(input::Union{QTerm, QSym}, a::QSym,ω , t)
    rot = _rotate(input, a, ω, t; extra_term = false)
    av_rot = average(rot)
    return undo_average(trig_to_exp(av_rot)) - ω * a' * a
end

# function rotate(input::Union{QTerm,QSym}, a::QSym,ω , t)
#     to_rot = average(input)
#     rot = rotate(to_rot, a, ω, t)
#     return undo_average(rot) - ω * a' * a
# end

# function rotate(input::BasicSymbolic, a::QSym,ω , t)
#     a_av = average(a)
#     a′_av = average(a')
#     rules = [a_av => exp(im * ω * t) * a_av, a′_av => exp(-im * ω * t) * a′_av]
#     rot = substitute(input, rules) |> trig_to_exp
#     return rot
# end

# const ROTATE_RULES = Dict{Symbol, Function}(
#     average(a) => exp(im * ω * t)*average(a),
#     average(a') => exp(-im * ω * t)*average(a'),
#     average(a*a) => exp(2 * im * ω * t)*average(a*a),
#     average(a'*a') => exp(-2 * im * ω * t)*average(a'*a'),
#     average(a*a*a) => exp(3 * im * ω * t)*average(a*a*a),
#     average(a'*a*a) => exp(im * ω * t)*average(a'*a*a),
#     average(a'*a'*a) => exp(-im * ω * t)*average(a'*a'*a),
#     average(a'*a'*a') => exp(-2 * im * ω * t)*average(a'*a'*a),
#     average(a*a*a*a) => exp(4 * im * ω * t)*average(a*a*a*a),
#     average(a'*a*a*a) => exp(2 * im * ω * t)*average(a'*a*a*a),
#     average(a'*a'*a'*a) => exp(-2 * im * ω * t)*average(a'*a'*a'*a),
#     average(a'*a'*a'*a') => exp(-4 * im * ω * t)*average(a'*a'*a'*a'),
# )
#We probably can wrtie it for symbolics with
#  QHB.filterchildren(w -> w isa BasicSymbolic{QuantumCumulants.AvgSym}, rot_av)
