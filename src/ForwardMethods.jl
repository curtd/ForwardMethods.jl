module ForwardMethods
    using MLStyle 
    
    export @forward_methods, @forward_interface

    include("util.jl")
    include("forward_methods.jl")
    include("forward_interface.jl")
end
