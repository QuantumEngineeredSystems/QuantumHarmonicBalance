# function declare_variable(name::Symbol)
#     @eval($(var_sym) = first(@variables $var_sym))
#     return eval(var_sym)
# end
# declare_variable(name::String) = declare_variable(Symbol(name))
