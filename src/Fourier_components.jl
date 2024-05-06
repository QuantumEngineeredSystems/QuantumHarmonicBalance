using QuantumHarmonicBalance: QAdd, QMul

# is_number(x) = x isa Number
# is_imaginary(x) = x isa Number && imag(x) != 0
# is_real(x) = x isa Number && imag(x) == 0.0

# function add_components!(dict, i::Int, components)
#     i ∈ keys(dict) ? dict[i] += components : dict[i] = components
#     nothing
# end

# function determine_fourier_multiple(term)
#     is_exp(term) || error("Not an exponential.")
#     exponents = arguments(term)
#     length(exponents) == 1 || error("Only one exponent is allowed.")
#     exponent = exponents[1]
#     ismul(exponent) || error("Not a multiplication.")
#     idxs = findall(is_number, arguments(exponent))
#     length(idxs) == 1 || error("Only one number is allowed in the exponential.")
#     idx = idxs[1]
#     multiple = arguments(exponent)[idx]
#     is_imaginary(multiple) || error("The multiple must be imaginary.")
#     return Int(real(-im * arguments(exponent)[idx]))
# end

# function extract_exponential(x::QMul)
#     output = []
#     for arg in arguments(x)
#         if is_exp(arg)
#             append!(output, [arg])
#         elseif arg isa BasicSymbolic
#             BS_args = arguments(arg)
#             exp_idxs = is_exp.(BS_args)
#             append!(output, BS_args[exp_idxs])
#         end
#     end
#     return output
# end

# function get_fourier_components(rot::QAdd)
#     components = Dict{Int, Union{QTerm, QSym, Number}}()
#     for term in arguments(rot)
#         if term isa QMul
#             args = arguments(term)
#             exponentials = extract_exponential(term)
#             if length(exponentials) == 1
#                 multiple = determine_fourier_multiple(exponentials[1])
#                 add_components!(components, multiple, term)
#             elseif isempty(exponentials)
#                 add_components!(components, 0, term)
#             else
#                 error("Only one exponential is allowed")
#             end
#         else
#             add_components!(components, 0, term)
#         end
#     end
#     return components
# end
