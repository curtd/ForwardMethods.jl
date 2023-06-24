using ForwardMethods

if VERSION ≥ v"1.9"
    using Aqua
    Aqua.test_all(ForwardMethods)
end

include("TestForwardMethods.jl")