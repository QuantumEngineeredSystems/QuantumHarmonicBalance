using Symbolics: occursin, substitute
using SymbolicUtils: BasicSymbolic

Qexpression = Union{QTerm, QSym, Number, BasicSymbolic}

function is_fourier(x, ω, t)
    isexp = is_exp(x)
    hasωt = occursin(ω, x) && occursin(t, x)
    return isexp && hasωt
end
function get_fourier_mul(mul::BasicSymbolic, ω, t)
    !ismul(mul) ? error("Not a Mul: $mul") : nothing
    args = arguments(mul)
    idxs = findall(x -> is_fourier(x, ω, t), args)
    length(idxs) > 1 ? error("More than one Fourier component: $mul") : nothing
    rest = args[setdiff(1:end, idxs)]
    idxs, args[idxs], prod(rest) # Vector{Int}, Vector{BasicSymbolic}, BasicSymbolic
end

function add_components!(dict::Dict{Int, Qexpression}, i::Int, components)
    i ∈ keys(dict) ? dict[i] += components : dict[i] = components
    nothing
end
function add_components!(dict::Dict{Int, Qexpression}, term, ω, t)
    idxs, exponentials, rest = get_fourier_mul(term, ω, t)
    if isempty(idxs)
        add_components!(dict, 0, undo_average(rest))
    else
        args = first(arguments(first(exponentials))) # can be dealt with better
        multiple = substitute(args, Dict(ω=>-im, t=>1)) |> Int
        add_components!(dict, multiple, undo_average(rest))
    end
end
function get_fourier_components(rot::BasicSymbolic, ω, t)
    components = Dict{Int, Qexpression}()
    if isadd(rot)
        for term in arguments(rot)
            add_components!(components, term, ω, t)
        end
    elseif ismul(rot)
        add_components!(components, rot, ω, t)
    else
        error("Not a sum or product: $rot")
    end
    return components
end

function remove_constants(rot::BasicSymbolic)
    avgsym = filterchildren(w -> w isa BasicSymbolic{QuantumCumulants.AvgSym}, rot)
    constant = substitute(rot, Dict(avgsym .=> 0))
    rot - constant
end
