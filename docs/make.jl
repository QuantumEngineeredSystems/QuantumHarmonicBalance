using QuantumHarmonicBalance
using Documenter

DocMeta.setdocmeta!(QuantumHarmonicBalance, :DocTestSetup, :(using QuantumHarmonicBalance); recursive=true)

makedocs(;
    modules=[QuantumHarmonicBalance],
    authors="Orjan Ameye",
    repo="https://github.com/oameye/QuantumHarmonicBalance.jl/blob/{commit}{path}#{line}",
    sitename="QuantumHarmonicBalance.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://oameye.github.io/QuantumHarmonicBalance.jl",
        edit_link="master",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/oameye/QuantumHarmonicBalance.jl",
    devbranch="master",
)
