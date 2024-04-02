mutable struct TimeDependentHamiltonian
    """Assigns to each variable an equation of motion."""
    hamiltonian::QNumber
    """Assigns to each variable a set of harmonics."""
    vars::Vector{QSym}

end
