using QuantumCumulants

struct Position <: QSym
    hilbert
    name
    aon
    metadata
end
Position(hilbert, name, aon; metadata=QuantumCumulants.source_metadata(:Position, name)) =
    Position(hilbert, name, aon, metadata)

struct Momentum <: QSym
    hilbert
    name
    aon
    metadata
end
Momentum(hilbert, name, aon; metadata=QuantumCumulants.source_metadata(:Momentum, name)) =
    Momentum(hilbert, name, aon, metadata)

    QuantumCumulants.ismergeable(::Position,::Momentum) = true
Base.:*(x::Position,p::Momentum) = im + p*x
for T in (:Position, :Momentum)
    @eval Base.isequal(a::$T, b::$T) = isequal(a.hilbert, b.hilbert) && isequal(a.name, b.name) && isequal(a.aon, b.aon)
end

h = FockSpace(:oscillator)
x = Position(h,:x,1)
p = Momentum(h,:p,1)

@cnumbers ω m
H = p^2/(2m) + 0.5m*ω^2*x^2

eqs = meanfield([x,p],H)
