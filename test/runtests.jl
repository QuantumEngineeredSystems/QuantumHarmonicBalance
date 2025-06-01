using QuantumHarmonicBalance
using Test

using Random
const SEED = 0xd8e5d8df
Random.seed!(SEED)

@testset "Rotate" begin
    include("rotate.jl")
end
