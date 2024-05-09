using QuantumHarmonicBalance
using QuantumCumulants: simplify
using Test

# Hilbert space
h = FockSpace(:cavity)

@cnumbers Δ F κ ω ω₀
@qnumbers a::Destroy(h)
@syms t::Real

@testset begin
    expr = (a' + a) |> simplify
    rot = rotate(expr, a, ω, t)

    @test rot.arguments[end] + ω * a' * a |> simplify == 0
    @test string(rot) == "(exp((0 - 1im)*t*ω)*(a′)+exp((0 + 1im)*t*ω)*(a)+-ω*(a′*a))"
end

@testset begin
    expr = F * (a' + a) |> simplify
    rot = rotate(expr, a, ω, t)

    @test rot.arguments[end] + ω * a' * a |> simplify == 0
    @test string(rot) == "(F*exp((0 - 1im)*t*ω)*(a′)+F*exp((0 + 1im)*t*ω)*(a)+-ω*(a′*a))"
end

@testset begin
    expr = ω₀ * (a' * a) |> simplify
    rot = rotate(expr, a, ω, t) |> simplify

    @test simplify(average(rot) - average(-ω * (a' * a) + ω₀ * (a' * a))) == 0
    @test string(rot) == "(-ω*(a′*a)+ω₀*(a′*a))"
end

@testset begin
    expr = (a' + a)^4 |> simplify
    rot = rotate(expr, a, ω, t)

    @test string(rot) ==
        "(6*(a′*a′*a*a)+3+12*(a′*a)+exp((0 + 4im)*t*ω)*(a*a*a*a)+6exp((0 + 2im)*t*ω)*(a*a)+4exp((0 + 2im)*t*ω)*(a′*a*a*a)+6exp((0 - 2im)*t*ω)*(a′*a′)+4exp((0 - 2im)*t*ω)*(a′*a′*a′*a)+exp((0 - 4im)*t*ω)*(a′*a′*a′*a′)+-ω*(a′*a))"
end
