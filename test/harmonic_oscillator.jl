using QuantumHarmonicBalance
using QuantumCumulants; QC = QuantumCumulants
using ModelingToolkit, OrdinaryDiffEq, Plots

# Hilbert space
h = FockSpace(:cavity)

# Parameter
@cnumbers Δ F κ ω
@qnumbers a::Destroy(h)

QC.commutator(a,a')

-im*ω*a'*a  |> dump
ω |> dump
a |> dump
exp = exp()


H = Δ*a'a + F*(a' + a)
J = [a]
rates = [κ]

eqs_a = meanfield(a, H, J; rates=rates, order=1); complete!(eqs_a)
eqs_n = meanfield(a'*a, H, J; rates=rates, order=2); complete!(eqs_n)

@named sys = ODESystem(eqs_a)

u0 = zeros(ComplexF64, 1)
p = (Δ, F, κ); p0 = p .=> (0.1, 0.01, 0.001)
prob = ODEProblem(sys, u0, (0.0, 1000.0), p0)
sol = solve(prob, RK4())

aₜ = real.(sol[a] .+ sol[a'])
plot(sol.t, imag.(sol[a]), label="aₜ", xlabel="t")

# # Correlation function
# c = CorrelationFunction(a', a, eqs_n;steady_state=true)

# # Time evolution of correlation function
# @named csys = ODESystem(c)
# u0_c = correlation_u0(c,sol.u[end])
# p0_c = correlation_p0(c,sol.u[end],ps.=>p0)
# prob_c = ODEProblem(csys,u0_c,(0.0,500.0),p0_c)
# sol_c = solve(prob_c,RK4(),save_idxs=1)

# S = Spectrum(c,p)
# s_laplace = S(ω,sol.u[end],p0)
