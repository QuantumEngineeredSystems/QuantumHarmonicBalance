struct Position <: QSym
    hilbert
    name
    aon
    metadata
end
function Position(
        hilbert, name, aon; metadata = QuantumCumulants.source_metadata(:Position, name))
    Position(hilbert, name, aon, metadata)
end

struct Momentum <: QSym
    hilbert
    name
    aon
    metadata
end
function Momentum(
        hilbert, name, aon; metadata = QuantumCumulants.source_metadata(:Momentum, name))
    Momentum(hilbert, name, aon, metadata)
end

QuantumCumulants.ismergeable(::Position, ::Momentum) = true
Base.:*(x::Position, p::Momentum) = im + p * x

for T in (:Position, :Momentum)
    @eval Base.isequal(a::$T, b::$T) = isequal(a.hilbert, b.hilbert) &&
                                       isequal(a.name, b.name) && isequal(a.aon, b.aon)
end
