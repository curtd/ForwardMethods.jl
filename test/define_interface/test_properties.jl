mutable struct CompositeProperties
    settable::SettableProperties
    key4::Float64
end
@define_interface CompositeProperties interface=properties delegated_fields=settable

mutable struct CompositeProperties2
    settable::SettableProperties
    alsosettable::SettableProperties2
end
@test !ForwardMethods.has_defined_interface(CompositeProperties2, Val(:properties))
@test !ForwardMethods.has_defined_interface(SettableProperties2, Val(:properties))

@define_interface CompositeProperties2 interface=properties delegated_fields=(settable, alsosettable)

@test ForwardMethods.has_defined_interface(CompositeProperties2, Val(:properties))
@test !ForwardMethods.has_defined_interface(SettableProperties2, Val(:properties))

# Won't be a problem recursing into this type since, since unregistered types don't have recursively derived properties
struct UnregisteredComposite
    x1::A
    x2::CompositeProperties
end

mutable struct CompositePropertiesRecursive
    _key1::CompositeProperties
    _key2::UnregisteredComposite
end
@define_interface CompositePropertiesRecursive interface=properties delegated_fields=(_key1, _key2) recursive=true

struct ClashingKeys 
    key1::SettableProperties
    key2::String
end
@test_throws_compat ErrorException "Duplicate properties `[:key1, :key2]` found for type $ClashingKeys in child `key1::$SettableProperties`" @define_interface ClashingKeys interface=properties delegated_fields=key1

@testset "properties interface" begin 
    c = CompositeProperties(SettableProperties("abc", 0, true), 0.0)
    @Test propertynames(c) == (:settable, :key4, :key1, :key2, :key3)
    @Test c.settable == getfield(c, :settable)
    @Test c.key4 == getfield(c, :key4)
    @Test c.key1 == getfield(getfield(c, :settable), :key1)
    @Test c.key2 == getfield(getfield(c, :settable), :key2)
    @Test c.key3 == getfield(getfield(c, :settable), :key3)
    c.key1 = "zzz"
    @Test c.key1 == "zzz"
    
    if VERSION ≥ v"1.9"
        @inferred propertynames(c)
        f = t->t.key1
        @Test @inferred f(c) == "zzz"
    end

    e = UnregisteredComposite(A([1]), CompositeProperties(SettableProperties("a", -1, false), 0.1))
    d = CompositePropertiesRecursive(c, e)
    @Test propertynames(d) == (:_key1, :_key2, :settable, :key4, :key1, :key2, :key3, :x1, :x2)
    for p in (:_key1, :_key2)
        @Test getproperty(d, p) === getfield(d, p)
    end
    for p in (:settable, :key4)
        @Test getproperty(d, p) === getfield(getfield(d, :_key1), p)
    end
    for p in (:key1, :key2, :key3)
        @Test getproperty(d, p) === getfield(getfield(getfield(d, :_key1), :settable), p)
    end
    for p in (:x1, :x2)
        @Test getproperty(d, p) === getfield(getfield(d, :_key2), p)
    end
    @Test d.key1 == "zzz"
    d.key1 = "z"
    @Test d.key1 == "z"

    c = CompositeProperties2(SettableProperties("abc", 0, true), SettableProperties2(0.0, [0]))
    @Test propertynames(c) == (:settable, :alsosettable, :key1, :key2, :key3, :key4, :key5)
    @Test c.settable === getfield(c, :settable)
    @Test c.alsosettable === getfield(c, :alsosettable)
    @Test c.key1 === getfield(getfield(c, :settable), :key1)
    @Test c.key2 === getfield(getfield(c, :settable), :key2)
    @Test c.key3 === getfield(getfield(c, :settable), :key3)
    @Test c.key4 === getfield(getfield(c, :alsosettable), :key4)
    @Test c.key5 === getfield(getfield(c, :alsosettable), :key5)

    c.key3 = false 
    c.key4 = 1.0
    @Test getfield(getfield(c, :settable), :key3) == false
    @Test getfield(getfield(c, :alsosettable), :key4) == 1.0
    @Test c.alsosettable.key4 == 1.0
    @Test c.alsosettable.key5 == [0]

    c.alsosettable = SettableProperties2(2.0, [1])
    @Test c.alsosettable.key4 == 2.0
    @Test c.alsosettable.key5 == [1]
    @Test c.key4 == 2.0
    @Test c.key5 == [1]
    

end