using Documenter

using Pkg
docs_dir = joinpath(@__DIR__, "..")
project_dir = isempty(ARGS) ? @__DIR__() : joinpath(pwd(), ARGS[1])
Pkg.activate(project_dir)

using ForwardMethods

DocMeta.setdocmeta!(ForwardMethods, :DocTestSetup, :(using ForwardMethods); recursive=true)

makedocs(;
    modules=[ForwardMethods],
    authors="Curt Da Silva",
    repo="https://github.com/curtd/ForwardMethods.jl/blob/{commit}{path}#{line}",
    sitename="ForwardMethods.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://curtd.github.io/ForwardMethods.jl",
        edit_link="main",
        assets=String[],
        ansicolor=true
    ),
    pages=[
        "Home" => "index.md",
        "API" => "api.md"
    ],
)

deploydocs(;
    repo="github.com/curtd/ForwardMethods.jl.git",
    devbranch="main", push_preview=true
)
