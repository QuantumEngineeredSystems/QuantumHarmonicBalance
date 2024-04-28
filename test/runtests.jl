using QuantumHarmonicBalance
using Test

using Random
const SEED = 0xd8e5d8df
Random.seed!(SEED)

@testset "Rotate"
    include("rotate.jl")
end
