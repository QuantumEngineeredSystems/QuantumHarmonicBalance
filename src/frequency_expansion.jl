function rotating_wave_approximation(H::QTerm, a::QSym, ω = cnumber(:ω), t = rnumber(:t))
    Hrot = rotate(H, a, ω, t)
    Hrot_av = average(Hrot) |> remove_constants
    components = get_fourier_components(Hrot_av, ω, t)
    return components[0]
end
