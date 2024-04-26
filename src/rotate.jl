using QuantumCumulants: check_hilbert, QMul, QSym, Create, Destroy, simplify, QAdd

function rotate(input::QMul, a::QSym, ω=cnumber(:ω), t=rnumber(:t); extra_term=true)::QMul
    out = 1.0
    for arg in input.args_nc
        if arg isa Create
            out *= exp(-im*ω*t)*arg
        elseif arg isa Destroy
            out *= exp(im*ω*t)*arg
        else
            error("Unknown type: $(typeof(arg))")
        end
    end
    extra_term ? input.arg_c*simplify(out) - im*ω*a'*a : input.arg_c*simplify(out)
end
function rotate(input::QAdd, a::QSym; extra_term=true)
    sum(arg -> rotate(arg, a; extra_term=false), input.arguments) - im*ω*a'*a
end
function rotate(in::Create, a::QSym; extra_term=true)
    extra_term ? exp(-im*ω*t)*in - im*ω*a'*a : exp(-im*ω*t)*in
end
function rotate(in::Destroy, a::QSym; extra_term=true)
    extra_term ? exp(im*ω*t)*in - im*ω*a'*a : exp(im*ω*t)*in
end
