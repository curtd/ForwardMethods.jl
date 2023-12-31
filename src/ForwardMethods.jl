module ForwardMethods
    using MacroUtilities, MLStyle
    
    export @forward_methods, @forward_interface, @define_interface

    include("util.jl")
    include("forward_methods.jl")
    include("forward_interface.jl")
    include("define_interface.jl")
end
