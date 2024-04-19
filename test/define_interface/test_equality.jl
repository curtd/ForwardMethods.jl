
@define_interface SettableProperties2 interface=equality 

struct EqualityUsingProperties 
    d::Dict{Symbol,Int}
end
Base.propertynames(e::EqualityUsingProperties) = collect(keys(getfield(e, :d)))
Base.getproperty(e::EqualityUsingProperties, k::Symbol) = getfield(e, :d)[k]
Base.setproperty!(e::EqualityUsingProperties, k::Symbol, v::Int) = getfield(e, :d)[k] = v
@define_interface EqualityUsingProperties interface=equality compare_fields=propertynames

@testset "equality interface" begin 
    e = EqualityUsingProperties(Dict(:a => 1))
    @Test e == EqualityUsingProperties(Dict(:a => 1))
    @Test e != EqualityUsingProperties(Dict(:a => 1, :b => 2))


end